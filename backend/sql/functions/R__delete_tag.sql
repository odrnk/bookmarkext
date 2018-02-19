CREATE OR REPLACE FUNCTION bm.delete_tag(tag_id uuid)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    delete from bm.tag_arrow ta where ta.tag_id = tag_id;
    delete from bm.tag_arrow ta where ta.parent_tag_id = tag_id;

    with bookmarks as (
        delete from bm.bookmark_tag bt
        where bt.tag_id = tag_id
        returning bt.bookmark_id
    ),
    delete_bookmark_tag as (
        delete from bm.bookmark_tag bt
        where bt.bookmark_id in (select b.bookmark_id from bookmarks b)
        returning bt.bookmark_id
    )
    delete from bm.bookmark b
    where b.id in (select b.bookmark_id from bookmarks b);

    delete from bm.tag t where t.id = tag_id;
END;
$$ LANGUAGE plpgsql;
