CREATE OR REPLACE VIEW bm.root_tag AS
    SELECT t.*
    FROM bm.tag t
    where t.id not in
        (select ta.tag_id from bm.tag_arrow ta);
