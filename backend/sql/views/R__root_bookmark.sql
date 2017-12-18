CREATE OR REPLACE VIEW bm.root_bookmark AS
    select b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.date_added, b.date_modified,
        fa.data as favicon_data, '[]'::jsonb as tags
    from bm.bookmark b
    left join bm.favicon fa on fa.id = b.favicon_id
    where b.id not in
        (select bt.bookmark_id from bm.bookmark_tag bt);
