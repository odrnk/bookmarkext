do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.delete_tag() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    count_bookmarks int;
    tag_id uuid := bm.generate_id();
    parent_tag_id uuid := bm.generate_id();
    bookmark1_id uuid := bm.generate_id(); -- of `tag_id`
    bookmark2_id uuid := bm.generate_id(); -- of `tag_id` and `parent_tag_id`
begin
    -- arrange
    insert into bm.tag(id, name)
    values  (tag_id, tag_id),
            (parent_tag_id, parent_tag_id);

    insert into bm.tag_arrow(tag_id, parent_tag_id)
    values  (tag_id, parent_tag_id);

    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark1_id, bookmark1_id, '', '', 1::smallint, bm.get_utc_now(), ''),
            (bookmark2_id, bookmark2_id, '', '', 1::smallint, bm.get_utc_now(), '');

    insert into bm.bookmark_tag(tag_id, bookmark_id)
    values  (tag_id, bookmark1_id),
            (tag_id, bookmark2_id),
            (parent_tag_id, bookmark2_id);

    -- act
    perform bm.delete_tag(tag_id);

    -- assert
    if exists (select 1 from bm.tag t where t.id = tag_id) then
        return assert.fail('The tag must be deleted!');
    end if;

    if exists (select 1 from bm.tag_arrow ta
                where ta.tag_id = tag_id
                        and ta.parent_tag_id = parent_tag_id) then
        return assert.fail('The tag arrow must be deleted!');
    end if;

    if exists (select 1 from bm.bookmark b where b.id in (bookmark1_id, bookmark2_id)) then
        return assert.fail('The bookmarks must be deleted!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
