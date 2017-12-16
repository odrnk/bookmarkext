-- for each tag of the bookmark find all its ascendants
-- and insert them into `bookmark_tag` table if they are not there yet
CREATE OR REPLACE FUNCTION bm.fix_bookmark_tags(bookmark_id uuid)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    with recursive ascendants as (
        select ta.parent_tag_id
        from bm.bookmark_tag bt
        inner join bm.tag_arrow ta on ta.tag_id = bt.tag_id
        where bt.bookmark_id = bookmark_id
        union
        select ta.parent_tag_id
        from bm.tag_arrow ta
        inner join ascendants a on a.parent_tag_id = ta.tag_id
    )
    insert into bm.bookmark_tag(bookmark_id, tag_id)
    select bookmark_id, a.parent_tag_id
    from ascendants a
    where a.parent_tag_id not in (
        select bt.tag_id
        from bm.bookmark_tag bt
        where bt.bookmark_id = bookmark_id);
END;
$$ LANGUAGE plpgsql;
