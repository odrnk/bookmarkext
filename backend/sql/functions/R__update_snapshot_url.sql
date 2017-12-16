CREATE OR REPLACE FUNCTION bm.update_snapshot_url(bookmark_id uuid, snapshot_url text)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    update bm.bookmark
    set
        snapshot_url = snapshot_url,
        date_modified = bm.get_utc_now()
    where id = bookmark_id;
END;
$$ LANGUAGE plpgsql;
