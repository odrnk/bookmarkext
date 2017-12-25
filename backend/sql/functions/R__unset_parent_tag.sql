CREATE OR REPLACE FUNCTION bm.unset_parent_tag(tag_id uuid, parent_tag_id uuid)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    -- quit if no such arrow
    if Not exists (select 1 from bm.tag_arrow ta
                    where   ta.tag_id = tag_id
                            and ta.parent_tag_id = parent_tag_id) then
        return;
    end if;

    delete from bm.tag_arrow ta
    where   ta.tag_id = tag_id
            and ta.parent_tag_id = parent_tag_id;

    with bookmarks AS (
        select bt.bookmark_id as id
        from bm.bookmark_tag bt
        where bt.tag_id = tag_id
    )
    delete from bm.bookmark_tag bt
    where   bt.bookmark_id in (select b.id from bookmarks b) and
            (bt.tag_id = parent_tag_id or
             bt.tag_id in (select a.id from bm.get_ascendant_tags(parent_tag_id) a));
END;
$$ LANGUAGE plpgsql;
