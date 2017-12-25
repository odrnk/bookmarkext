do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.reset_parent_tag() returns test_result as $$
#variable_conflict use_variable
declare
    tag_id uuid := bm.generate_id();
    old_parent_tag_id uuid := bm.generate_id();
    new_parent_tag_id uuid := bm.generate_id();
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.tag(id, name)
    values  (tag_id, tag_id),
            (old_parent_tag_id, old_parent_tag_id),
            (new_parent_tag_id, new_parent_tag_id);

    insert into bm.tag_arrow(tag_id, parent_tag_id)
    values  (tag_id, old_parent_tag_id);

    -- act
    result := bm.reset_parent_tag(tag_id, old_parent_tag_id, new_parent_tag_id);

    -- assert
    if not result then
        return assert.fail('The result must be `true`!');
    end if;

    if exists  (select 1 from bm.tag_arrow ta
                where   ta.tag_id = tag_id
                        and ta.parent_tag_id = old_parent_tag_id) then
        return assert.fail('The arrow must be deleted!');
    end if;

    if not exists (select 1 from bm.tag_arrow ta
                    where   ta.tag_id = tag_id
                            and ta.parent_tag_id = new_parent_tag_id) then
        return assert.fail('The arrow must be created!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
