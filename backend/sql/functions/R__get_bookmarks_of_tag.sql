CREATE OR REPLACE FUNCTION bm.get_bookmarks_of_tag(tag_id uuid)
RETURNS TABLE(
    id uuid, url text, title text, description text, visit_count smallint,
    last_visit_date timestamp, snapshot_url text, date_added timestamp,
    date_modified timestamp, favicon_data bytea, tags jsonb
    ) AS
$$
#variable_conflict use_variable
BEGIN
    return QUERY with bookmarks_of_tag AS (
        SELECT bt.bookmark_id as id
        from bm.bookmark_tag bt
        where bt.tag_id = tag_id
    ),
    bookmarks_with_tags as (
        SELECT
            bot.id,
            jsonb_agg(jsonb_build_object('name', t.name, 'id', t.id)) as tags
        from bookmarks_of_tag bot
        inner join bm.bookmark_tag bt on bt.bookmark_id = bot.id
        inner join bm.tag t on t.id = bt.tag_id
        group by bot.id
    )
    select b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.date_added, b.date_modified,
        fa.data as favicon_data, bwt.tags
    from bm.bookmark b
    inner join bookmarks_with_tags bwt on bwt.id = b.id
    left join bm.favicon fa on fa.id = b.favicon_id;
END;
$$ LANGUAGE plpgsql STABLE;
