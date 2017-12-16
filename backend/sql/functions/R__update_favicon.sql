CREATE OR REPLACE FUNCTION bm.update_favicon(bookmark_id uuid, favicon_data bytea)
RETURNS void AS $$
#variable_conflict use_variable
declare
    checksum text;
    favicon_id uuid;
BEGIN
    checksum := md5(favicon_data);

    select f.id into favicon_id
    from bm.favicon f where f.checksum = checksum;

    if favicon_id is null then
        favicon_id := bm.generate_id();
        insert into bm.favicon(id, data, checksum)
        values (favicon_id, favicon_data, md5(favicon_data));
    end if;

    update bm.bookmark
    set
        favicon_id = favicon_id,
        date_modified = bm.get_utc_now()
    where id = bookmark_id;
END;
$$ LANGUAGE plpgsql;
