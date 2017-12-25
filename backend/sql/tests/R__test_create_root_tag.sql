do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.create_root_tag() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    tag_id uuid;
    tag_name text := bm.generate_id()::text;
begin
    -- act
    tag_id := bm.create_root_tag(tag_name);

    -- assert
    if tag_id is null then
        return assert.fail('The result must not be null!');
    end if;

    if not exists (select 1 from bm.tag t
                    where t.name = tag_name) then
        return assert.fail('The tag must be created!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
