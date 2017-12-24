-- Returns `true` on success, `false` if it didn't pass validation.
-- It is assumed that both `tag_id` and `parent_tag_id` exist in `bm.tag`.
CREATE OR REPLACE FUNCTION bm.set_parent_tag(tag_id uuid, parent_tag_id uuid)
RETURNS boolean AS $$
#variable_conflict use_variable
BEGIN
    IF NOT bm.check_tag_arrow(tag_id, parent_tag_id) THEN
        return false;
    end if;

    insert into bm.tag_arrow(tag_id, parent_tag_id)
    values (tag_id, parent_tag_id);

    insert into bm.bookmark_tag(bookmark_id, tag_id)
    select bt.bookmark_id, parent_tag_id
    from bm.bookmark_tag bt
    where bt.tag_id = tag_id and bt.bookmark_id not in (
        select bt2.bookmark_id
        from bm.bookmark_tag bt2
        where bt2.tag_id = parent_tag_id);

    with bookmarks AS (
        select bt.bookmark_id as id
        from bm.bookmark_tag bt
        where bt.tag_id = parent_tag_id
    )
    insert into bm.bookmark_tag(bookmark_id, tag_id)
    select b.id, t.id
    from bookmarks b
    cross join bm.get_ascendant_tags(parent_tag_id) t
    where b.id not in (
        select bt2.bookmark_id
        from bm.bookmark_tag bt2
        where bt2.tag_id = t.id);

    return true;
END;
$$ LANGUAGE plpgsql;
