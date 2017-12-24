do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.update_snapshot_url() returns test_result as $$
#variable_conflict use_variable
declare
    id uuid := bm.generate_id();
    snapshot_url text;
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values (id, id, 't', 'd', 1::smallint, bm.get_utc_now(), '');

    -- act
    perform bm.update_snapshot_url(id, 'http://snapshot_url/text');

    -- assert
    select b.snapshot_url into snapshot_url
    from bm.bookmark b where b.id = id;
    
    select * from assert.is_equal(snapshot_url, 'http://snapshot_url/text') into message, result;
    IF not result THEN
        RETURN message;
    END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
