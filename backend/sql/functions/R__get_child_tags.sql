CREATE OR REPLACE FUNCTION bm.get_child_tags(tag_id uuid)
RETURNS TABLE(
    id uuid, name text, date_added timestamp,
    date_modified timestamp, tags jsonb
    ) AS
$$
#variable_conflict use_variable
BEGIN
    return QUERY with child_tags AS (
        SELECT ta.tag_id
        from bm.tag_arrow ta
        where ta.parent_tag_id = tag_id
    ),
    child_tags_with_parents as (
        SELECT
            ct.tag_id,
            jsonb_agg(jsonb_build_object('name', t.name, 'id', t.id)) as tags
        from child_tags ct
        inner join bm.tag_arrow ta on ta.tag_id = ct.tag_id
        inner join bm.tag t on t.id = ta.parent_tag_id
        group by ct.tag_id
    )
    select t.id, t.name, t.date_added, t.date_modified, ctwp.tags
    from bm.tag t
    inner join child_tags_with_parents ctwp on ctwp.tag_id = t.id;
END;
$$ LANGUAGE plpgsql STABLE;
