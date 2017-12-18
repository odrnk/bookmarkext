CREATE OR REPLACE FUNCTION bm.get_ascendant_tags(tag_id uuid)
RETURNS TABLE(id uuid, name text) AS
$$
#variable_conflict use_variable
BEGIN
    return QUERY with recursive ascendants AS (
        SELECT ta.parent_tag_id
        from bm.tag_arrow ta
        where ta.tag_id = tag_id
        union
        select ta.parent_tag_id
        from bm.tag_arrow ta
        inner join ascendants a on a.parent_tag_id = ta.tag_id
    )
    select t.id, t.name
    from bm.tag t
    inner join ascendants a on a.parent_tag_id = t.id;
END;
$$ LANGUAGE plpgsql STABLE;
