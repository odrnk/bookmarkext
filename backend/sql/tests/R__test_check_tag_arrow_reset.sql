do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.check_tag_arrow_reset() returns test_result as $$
#variable_conflict use_variable
declare
    tag_id uuid := bm.generate_id();
    old_parent_tag_id uuid := bm.generate_id();
    new_parent_tag_id uuid := bm.generate_id();
    date_added timestamp := bm.get_utc_now();
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.tag(id, name)
    values  (tag_id, tag_id),
            (old_parent_tag_id, old_parent_tag_id),
            (new_parent_tag_id, new_parent_tag_id);

    insert into bm.tag_arrow(tag_id, parent_tag_id, date_added)
    values  (tag_id, old_parent_tag_id, date_added);

    -- act
    result := bm.check_tag_arrow_reset(tag_id, old_parent_tag_id, new_parent_tag_id);

    -- assert
    if not result then
        return assert.fail('The result must be `true`!');
    end if;

    if not exists (select 1 from bm.tag_arrow ta
                    where   ta.tag_id = tag_id
                            and ta.date_added = date_added
                            and ta.parent_tag_id = old_parent_tag_id) then
        return assert.fail('The arrow must be restored!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
