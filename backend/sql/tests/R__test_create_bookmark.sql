do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

-- test create_bookmark with empty "tags" array
create or replace function unit_tests.create_bookmark_no_tags() returns test_result as $$
#variable_conflict use_variable
declare
    id uuid;
    test_url text := bm.generate_id()::text;
    now timestamp;
    bookmark_tag_count int;
    message test_result;
    result boolean;
begin
    -- arrange
    now := bm.get_utc_now();

    -- act
    select bm.create_bookmark(test_url, 'asdfasdf123 title', 'asdfasdf123 desc',
        11::smallint, now, array[]::text[]) into id;

    -- assert
    select * from assert.is_not_null(id) into message, result;
    IF not result THEN
        RETURN message;
    END IF;

    select count(*) into bookmark_tag_count
    from bm.bookmark_tag where bookmark_id = id;

    SELECT * FROM assert.is_equal(bookmark_tag_count, 0) INTO message, result;
    IF not result THEN
        RETURN assert.fail('bookmark must not have tags');
    END IF;

    if not exists (select 1 from bm.bookmark b 
                    where b.id = id and b.url = test_url
                        and b.title = 'asdfasdf123 title'
                        and b.description = 'asdfasdf123 desc'
                        and b.visit_count = 11
                        and b.last_visit_date = now) then
        RETURN assert.fail('some values of the bookmark does not match');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;


-- test create_bookmark with one new tag
create or replace function unit_tests.create_bookmark_new_tag() returns test_result as $$
#variable_conflict use_variable
declare
    bookmark_id uuid;
    test_url text := bm.generate_id()::text;
    tag_id uuid;
    bookmark_tag_count int;
    message test_result;
    result boolean;
begin
    -- act
    select bm.create_bookmark(test_url, 'asdfasdf123 title', 'asdfasdf123 desc',
        11::smallint, bm.get_utc_now(), array['tag12345']) into bookmark_id;

    -- assert
    select t.id into tag_id
    from bm.tag t where t.name = 'tag12345';

    select * from assert.is_not_null(tag_id) into message, result;
    IF not result THEN
        RETURN message;
    END IF;

    if not exists (select 1 from bm.bookmark_tag bt
                    where bt.tag_id = tag_id and bt.bookmark_id = bookmark_id) then
        return assert.fail('bookmark has no tags but should!');
    end if;

    select count(*) into bookmark_tag_count
    from bm.bookmark_tag bt where bt.bookmark_id = bookmark_id;

    SELECT * FROM assert.is_equal(bookmark_tag_count, 1) INTO message, result;
    IF not result THEN
        RETURN assert.fail('bookmark must have exactly one tag!');
    END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;


-- test create_bookmark with two new tags and two existing tags
create or replace function unit_tests.create_bookmark_mix_tags() returns test_result as $$
#variable_conflict use_variable
declare
    bookmark_id uuid;
    test_url text := bm.generate_id()::text;
    tag_id uuid;
    bookmark_tag_count int;
    message test_result;
    result boolean;
begin
    -- arrange
    insert into bm.tag(id, name)
    values (bm.generate_id(), 'tag1234567'),(bm.generate_id(), 'tag12345678');

    -- act
    select bm.create_bookmark(test_url, 'asdfasdf123 title', 'asdfasdf123 desc',
        11::smallint, bm.get_utc_now(),
        array['tag12345','tag123456','tag1234567','tag12345678']) into bookmark_id;

    -- assert
    select t.id into tag_id
    from bm.tag t where t.name = 'tag12345';

    select * from assert.is_not_null(tag_id) into message, result;
    IF not result THEN
        RETURN message;
    END IF;

    select t.id into tag_id
    from bm.tag t where t.name = 'tag123456';

    select * from assert.is_not_null(tag_id) into message, result;
    IF not result THEN
        RETURN message;
    END IF;

    if not exists (select 1 from bm.bookmark_tag bt
                    inner join bm.tag t on bt.tag_id = t.id
                    where bt.bookmark_id = bookmark_id
                        and t.name in ('tag12345','tag123456','tag1234567','tag12345678')) then
        return assert.fail('bookmark has no tags it must have!');
    end if;

    select count(*) into bookmark_tag_count
    from bm.bookmark_tag bt where bt.bookmark_id = bookmark_id;

    SELECT * FROM assert.is_equal(bookmark_tag_count, 4) INTO message, result;
    IF not result THEN
        RETURN assert.fail('bookmark must have exactly four tags!');
    END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
