CREATE OR REPLACE FUNCTION bm.edit_bookmark(
    bookmark_id uuid, url text, title text, description text, tags text[])
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    update bm.bookmark
    set url = url,
        title = title,
        description = description,
        date_modified = bm.get_utc_now()
    where id = bookmark_id;

    delete from bm.bookmark_tag bt
    where bt.bookmark_id = bookmark_id;

    perform bm.set_bookmark_tags(bookmark_id, tags);
END;
$$ LANGUAGE plpgsql;
