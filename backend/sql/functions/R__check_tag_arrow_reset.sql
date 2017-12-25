-- checks that arrow (`tag_id`, `old_parent_tag_id`) can be removed
-- and new arrow (`tag_id`, `old_parent_tag_id`) can be created
CREATE OR REPLACE FUNCTION bm.check_tag_arrow_reset(tag_id uuid, old_parent_tag_id uuid, new_parent_tag_id uuid)
RETURNS boolean AS $$
#variable_conflict use_variable
declare
    date_added timestamp;
    result boolean;
BEGIN
    delete from bm.tag_arrow ta where ta.tag_id = tag_id and ta.parent_tag_id = old_parent_tag_id
    returning ta.date_added into date_added;

    result := bm.check_tag_arrow(tag_id, new_parent_tag_id);

    insert into bm.tag_arrow(tag_id, parent_tag_id, date_added)
    values  (tag_id, old_parent_tag_id, date_added);

    return result;
END;
$$ LANGUAGE plpgsql;
