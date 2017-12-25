CREATE OR REPLACE FUNCTION bm.delete_bookmark(bookmark_id uuid)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    delete from bm.bookmark_tag bt where bt.bookmark_id = bookmark_id;
    delete from bm.bookmark b where b.id = bookmark_id;
END;
$$ LANGUAGE plpgsql;
