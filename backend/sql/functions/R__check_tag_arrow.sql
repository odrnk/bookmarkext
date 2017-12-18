-- The function returns `true` if it has not found a cycle,
-- (i.e. that `tag_id` is not an ascendant of `parent_tag_id`)
-- and `parent_tag_id` is not an ascendant of `tag_id`
-- It is assumed that both `tag_id` and `parent_tag_id` exist in `bm.tag`.
CREATE OR REPLACE FUNCTION bm.check_tag_arrow(tag_id uuid, parent_tag_id uuid)
RETURNS boolean AS $$
BEGIN
    return
        not exists (
            select 1 from bm.get_ascendant_tags(parent_tag_id) a
            where a.id = tag_id)
        and
        not exists (
            select 1 from bm.get_ascendant_tags(tag_id) a
            where a.id = parent_tag_id);
END;
$$ LANGUAGE plpgsql;
