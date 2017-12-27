do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.get_bookmarks() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark1_id uuid := bm.generate_id();
    bookmark2_id uuid := bm.generate_id(); -- has no bookmarks
    tag1_id uuid := bm.generate_id(); -- of `bookmark1_id`
    tag2_id uuid := bm.generate_id(); -- of `bookmark2_id`
    favicon_id uuid := bm.generate_id();
    favicon_data bytea;
    returned_count int;
    returned_id uuid;
    returned_url text;
    returned_title text;
    returned_description text;
    returned_visit_count smallint;
    returned_last_visit_date timestamp;
    returned_snapshot_url text;
    returned_favicon_data bytea;
    returned_tags jsonb;
    expected_tags jsonb;
begin
    -- arrange
    favicon_data := favicon_id::text::bytea;

    insert into bm.favicon(id, data, checksum)
    values  (favicon_id, favicon_data, md5(favicon_data));

    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url, favicon_id)
    values  (bookmark1_id, bookmark1_id, 't1', 'd1', 42::smallint, '2000-01-01'::timestamp, 's1', favicon_id),
            (bookmark2_id, bookmark2_id, 't2', 'd2', 1::smallint, '2000-01-02'::timestamp, 's2', null);

    insert into bm.tag(id, name)
    values  (tag1_id, 'n' || tag1_id),
            (tag2_id, 'n' || tag2_id);

    insert into bm.bookmark_tag(tag_id, bookmark_id)
    values  (tag1_id, bookmark1_id),
            (tag2_id, bookmark1_id);

    expected_tags := jsonb_build_array(
                        jsonb_build_object('name', 'n' || tag1_id, 'id', tag1_id),
                        jsonb_build_object('name', 'n' || tag2_id, 'id', tag2_id));

    -- act 1
    select count(*) into returned_count
    from bm.get_bookmarks() b
    where b.id in (bookmark1_id, bookmark2_id);

    -- assert 1
    SELECT * FROM assert.is_equal(returned_count, 2) INTO message, result;
    IF not result THEN RETURN message; END IF;

    -- act 2
    select
        b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.favicon_data, b.tags
    into
        returned_id, returned_url, returned_title, returned_description,
        returned_visit_count, returned_last_visit_date, returned_snapshot_url,
        returned_favicon_data, returned_tags
    from bm.get_bookmarks() b
    where b.id = bookmark1_id;

    -- assert 2
    SELECT * FROM assert.is_equal(returned_id, bookmark1_id) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_url, bookmark1_id::text) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_title, 't1') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_description, 'd1') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_visit_count, 42::smallint) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_last_visit_date, '2000-01-01'::timestamp) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_snapshot_url, 's1') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_favicon_data, favicon_data) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_tags, expected_tags) INTO message, result;
    IF not result THEN RETURN message; END IF;

    -- act 3
    select
        b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.favicon_data, b.tags
    into
        returned_id, returned_url, returned_title, returned_description,
        returned_visit_count, returned_last_visit_date, returned_snapshot_url,
        returned_favicon_data, returned_tags
    from bm.get_bookmarks() b
    where b.id = bookmark2_id;

    -- assert 3
    SELECT * FROM assert.is_equal(returned_id, bookmark2_id) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_url, bookmark2_id::text) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_title, 't2') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_description, 'd2') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_visit_count, 1::smallint) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_last_visit_date, '2000-01-02'::timestamp) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_snapshot_url, 's2') INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_null(returned_favicon_data) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_tags, '[]'::jsonb) INTO message, result;
    IF not result THEN RETURN message; END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
