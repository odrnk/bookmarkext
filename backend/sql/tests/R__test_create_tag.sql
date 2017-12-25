do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.create_tag() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    tag_id uuid;
    tag_name text := bm.generate_id()::text;
    parent_tag_id uuid := bm.generate_id();
begin
    -- arrange
    insert into bm.tag(id, name)
    values (parent_tag_id, parent_tag_id);

    -- act
    tag_id := bm.create_tag(tag_name, parent_tag_id);

    -- assert
    if tag_id is null then
        return assert.fail('The result must not be null!');
    end if;

    if not exists (select 1 from bm.tag t
                    where t.id = tag_id and t.name = tag_name) then
        return assert.fail('The tag must be created!');
    end if;

    if not exists (select 1 from bm.tag_arrow ta
                    where ta.tag_id = tag_id
                            and ta.parent_tag_id = parent_tag_id) then
        return assert.fail('The tag arrow must be created!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
