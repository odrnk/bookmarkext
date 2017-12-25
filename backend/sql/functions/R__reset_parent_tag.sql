CREATE OR REPLACE FUNCTION bm.reset_parent_tag(tag_id uuid, old_parent_tag_id uuid, new_parent_tag_id uuid)
RETURNS boolean AS $$
#variable_conflict use_variable
BEGIN
    IF NOT bm.check_tag_arrow_reset(tag_id, old_parent_tag_id, new_parent_tag_id) THEN
        return false;
    end if;

    perform bm.unset_parent_tag(tag_id, old_parent_tag_id);
    return bm.set_parent_tag(tag_id, new_parent_tag_id);
END;
$$ LANGUAGE plpgsql;
