do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.delete_bookmark() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark_id uuid := bm.generate_id();
    tag1_id uuid := bm.generate_id();
    tag2_id uuid := bm.generate_id();
begin
    -- arrange
    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values (bookmark_id, bookmark_id, '', '', 1::smallint, bm.get_utc_now(), '');

    insert into bm.tag(id, name)
    values  (tag1_id, tag1_id),
            (tag2_id, tag2_id);

    insert into bm.bookmark_tag(bookmark_id, tag_id)
    values  (bookmark_id, tag1_id),
            (bookmark_id, tag2_id);

    -- act
    perform bm.delete_bookmark(bookmark_id);

    -- assert
    if exists (select 1 from bm.bookmark b where b.id = bookmark_id) then
        return assert.fail('The bookmark must be deleted!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
