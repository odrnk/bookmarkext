CREATE OR REPLACE FUNCTION bm.get_bookmarks()
RETURNS TABLE(
    id uuid, url text, title text, description text, visit_count smallint,
    last_visit_date timestamp, snapshot_url text, date_added timestamp,
    date_modified timestamp, favicon_data bytea, tags jsonb
    ) AS
$$
#variable_conflict use_variable
BEGIN
    return QUERY with bookmarks_with_tags as (
        SELECT
            b.id,
            jsonb_agg(jsonb_build_object('name', t.name, 'id', t.id)) as tags
        from bm.bookmark b
        inner join bm.bookmark_tag bt on bt.bookmark_id = b.id
        inner join bm.tag t on t.id = bt.tag_id
        group by b.id
    )
    select b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.date_added, b.date_modified,
        fa.data as favicon_data, coalesce(bwt.tags, '[]'::jsonb) as tags
    from bm.bookmark b
    left join bookmarks_with_tags bwt on bwt.id = b.id
    left join bm.favicon fa on fa.id = b.favicon_id;
END;
$$ LANGUAGE plpgsql STABLE;
