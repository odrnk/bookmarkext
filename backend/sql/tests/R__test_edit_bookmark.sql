do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.edit_bookmark() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark_id uuid := bm.generate_id();
    tag1_id uuid := bm.generate_id(); -- exising tag
    tag2_id uuid := bm.generate_id(); -- of `bookmark_id`
    new_tag_id uuid;
    test_tags text[];
    new_url text;
    new_title text := 'nt';
    new_description text := 'nd';
begin
    -- arrange
    test_tags := array[tag1_id, bm.generate_id()];
    new_url := 'n' || bookmark_id;

    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark_id, bookmark_id, 't', 'd', 1::smallint, bm.get_utc_now(), '');

    insert into bm.tag(id, name)
    values  (tag1_id, tag1_id),
            (tag2_id, tag2_id);

    insert into bm.bookmark_tag(bookmark_id, tag_id)
    values  (bookmark_id, tag2_id);

    -- act
    perform bm.edit_bookmark(bookmark_id, new_url, new_title, new_description, test_tags);

    -- assert
    if not exists (select 1 from bm.bookmark b
                    where   b.id = bookmark_id and
                            b.url = new_url and
                            b.title = new_title and
                            b.description = new_description) then
        return assert.fail('The bookmark is not fully updated!');
    end if;

    select t.id into new_tag_id
    from bm.tag t where t.name = test_tags[2];

    if new_tag_id is null then
        return assert.fail('The tag must be created!');
    end if;

    if not exists (select 1 from bm.bookmark_tag bt
                    where   bt.bookmark_id = bookmark_id
                            and bt.tag_id = new_tag_id) then
        return assert.fail('The bookmark must have that new tag!');
    end if;

    if not exists (select 1 from bm.bookmark_tag bt
                    where   bt.bookmark_id = bookmark_id
                            and bt.tag_id = tag1_id) then
        return assert.fail('The bookmark must have `tag1_id`!');
    end if;

    if exists (select 1 from bm.bookmark_tag bt
                    where   bt.bookmark_id = bookmark_id
                            and bt.tag_id = tag2_id) then
        return assert.fail('The bookmark must not have `tag2_id`!');
    end if;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
