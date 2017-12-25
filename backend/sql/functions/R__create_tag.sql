-- returns id of the tag
-- returns null if such tag already exists
CREATE OR REPLACE FUNCTION bm.create_tag(name text, parent_tag_id uuid)
RETURNS uuid AS $$
#variable_conflict use_variable
DECLARE
    id uuid := bm.generate_id();
BEGIN
    if exists (select 1 from bm.tag t where t.name = name) then
        return null;
    end if;

    insert into bm.tag(id, name)
    values (id, name);

    insert into bm.tag_arrow(tag_id, parent_tag_id)
    values (id, parent_tag_id);

    return id;
END;
$$ LANGUAGE plpgsql;
