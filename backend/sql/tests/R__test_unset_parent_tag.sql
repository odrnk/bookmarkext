﻿do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

-- tests that the function correctly works when there is no such arrow
create or replace function unit_tests.unset_parent_tag_no_arrow() returns test_result as $$
#variable_conflict use_variable
declare
    tag_id uuid := bm.generate_id();
    parent_tag_id uuid := bm.generate_id();
    bookmark1_id uuid := bm.generate_id(); -- of `tag_id` and `parent_tag_id`
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.tag(id, name)
    values  (tag_id, tag_id),
            (parent_tag_id, parent_tag_id);

    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark1_id, bookmark1_id, '', '', 1::smallint, bm.get_utc_now(), '');

    insert into bm.bookmark_tag(tag_id, bookmark_id)
    values  (tag_id, bookmark1_id),
            (parent_tag_id, bookmark1_id);

    -- act
    perform bm.unset_parent_tag(tag_id, parent_tag_id);

    -- assert
    if not exists (select 1 from bm.bookmark_tag bt
                    where   bt.tag_id = parent_tag_id
                            and bt.bookmark_id = bookmark1_id) THEN
        return assert.fail('the parent tag must have the bookmark!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

create or replace function unit_tests.unset_parent_tag() returns test_result as $$
#variable_conflict use_variable
declare
    tag_id uuid := bm.generate_id();
    parent_tag_id uuid := bm.generate_id();
    ascendant1_id uuid := bm.generate_id(); -- parent of `parent_tag_id`
    ascendant2_id uuid := bm.generate_id(); -- parent of `ascendant1_id`
    bookmark1_id uuid := bm.generate_id(); -- of `tag_id`
    bookmark2_id uuid := bm.generate_id(); -- of `tag_id` and `parent_tag_id`
    bookmark3_id uuid := bm.generate_id(); -- of `tag_id` and `parent_tag_id` and `ascendant1_id`
    bookmark4_id uuid := bm.generate_id(); -- of all the tags
    bookmark_tag_count int;
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.tag(id, name)
    values  (tag_id, tag_id),
            (parent_tag_id, parent_tag_id),
            (ascendant1_id, ascendant1_id),
            (ascendant2_id, ascendant2_id);

    insert into bm.tag_arrow(tag_id, parent_tag_id)
    values  (tag_id, parent_tag_id),
            (parent_tag_id, ascendant1_id),
            (ascendant1_id, ascendant2_id);

    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark1_id, bookmark1_id, '', '', 1::smallint, bm.get_utc_now(), ''),
            (bookmark2_id, bookmark2_id, '', '', 1::smallint, bm.get_utc_now(), ''),
            (bookmark3_id, bookmark3_id, '', '', 1::smallint, bm.get_utc_now(), ''),
            (bookmark4_id, bookmark4_id, '', '', 1::smallint, bm.get_utc_now(), '');

    insert into bm.bookmark_tag(tag_id, bookmark_id)
    values  (tag_id, bookmark1_id),
            (tag_id, bookmark2_id),
            (tag_id, bookmark3_id),
            (tag_id, bookmark4_id),
            (parent_tag_id, bookmark2_id),
            (parent_tag_id, bookmark3_id),
            (parent_tag_id, bookmark4_id),
            (ascendant1_id, bookmark3_id),
            (ascendant1_id, bookmark4_id),
            (ascendant2_id, bookmark4_id);

    -- act
    perform bm.unset_parent_tag(tag_id, parent_tag_id);

    -- assert
    if exists (select 1 from bm.tag_arrow ta
                where   ta.tag_id = tag_id
                        and ta.parent_tag_id = parent_tag_id) then
        return assert.fail('The tag arrow must be deleted!');
    end if;

    if exists (select 1 from bm.bookmark_tag bt
                where   bt.bookmark_id in (
                            bookmark1_id,
                            bookmark2_id,
                            bookmark3_id,
                            bookmark4_id)
                        and
                        bt.tag_id in (
                            parent_tag_id,
                            ascendant1_id,
                            ascendant2_id)) then
        return assert.fail('Bookmarks 1-4 must have only `tag_id` tag!');
    end if;

    select count(*) into bookmark_tag_count
    from bm.bookmark_tag bt
    where bt.tag_id = tag_id;

    select * from assert.is_equal(bookmark_tag_count, 4) into message, result;
    IF not result THEN
        return assert.fail('All four bookmarks must have `tag_id` tag!');
    END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
