do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.update_visit_info() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark1_id uuid := bm.generate_id();
    bookmark2_id uuid := bm.generate_id();
    url1 text := bm.generate_id()::text;
    url2 text := bm.generate_id()::text;
begin
    -- arrange
    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark1_id, url1, 't', 'd', 1::smallint, '2000-01-01'::timestamp, ''),
            (bookmark2_id, url2, 't', 'd', 1::smallint, '2000-01-01'::timestamp, '');

    -- act
    perform bm.update_visit_info(url1, '2000-01-02'::timestamp, 42::smallint);

    -- assert
    if not exists (select 1 from bm.bookmark b
                    where   b.id = bookmark1_id and
                            b.last_visit_date = '2000-01-02'::timestamp and
                            b.visit_count = 42) then
        return assert.fail('The bookmark is not updated!');
    end if;

    if not exists (select 1 from bm.bookmark b
                    where   b.id = bookmark2_id and
                            b.last_visit_date = '2000-01-01'::timestamp and
                            b.visit_count = 1) then
        return assert.fail('Other bookmarks should not be affected!');

    end if;
    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
