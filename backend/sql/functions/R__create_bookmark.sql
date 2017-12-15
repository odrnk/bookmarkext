CREATE OR REPLACE FUNCTION bm.create_bookmark(
    url text, title text, description text, visit_count smallint, last_visit_date timestamp, tags text[]) 
RETURNS uuid AS $$
declare
    tags_to_create text[];
    bookmark_id uuid;
BEGIN
    bookmark_id := bm.generate_id();

    INSERT INTO bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    VALUES (bookmark_id, url, title, description, visit_count, last_visit_date, '');

    -- find nonexisting tags
    select array_agg(tag_name) into tags_to_create
    from unnest(tags) tag_name
    left join bm.tag t on t.name = tag_name
    where t.Id is null;

    INSERT INTO bm.tag(id, name)
    select bm.generate_id(), tag_name
    from unnest(tags_to_create) tag_name;

    insert into bm.bookmark_tag(bookmark_id, tag_id)
    select bookmark_id, t.id
    from unnest(tags) tag_name
    INNER join bm.tag t on t.name = tag_name;

    RETURN bookmark_id;
END;
$$ LANGUAGE plpgsql;
