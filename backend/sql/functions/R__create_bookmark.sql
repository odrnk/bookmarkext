CREATE OR REPLACE FUNCTION bm.create_bookmark(
    url text, title text, description text, visit_count smallint, last_visit_date timestamp, tags text[])
RETURNS uuid AS $$
declare
    bookmark_id uuid;
BEGIN
    bookmark_id := bm.generate_id();

    INSERT INTO bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    VALUES (bookmark_id, url, title, description, visit_count, last_visit_date, '');

    perform bm.set_bookmark_tags(bookmark_id, tags);

    RETURN bookmark_id;
END;
$$ LANGUAGE plpgsql;
