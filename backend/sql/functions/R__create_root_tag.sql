-- returns id of the tag
-- returns null if such tag already exists
CREATE OR REPLACE FUNCTION bm.create_root_tag(name text)
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

    return id;
END;
$$ LANGUAGE plpgsql;
