CREATE OR REPLACE FUNCTION bm.set_bookmark_tags(bookmark_id uuid, tags text[])
RETURNS void AS $$
declare
    tags_to_create text[];
BEGIN
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
END;
$$ LANGUAGE plpgsql;
