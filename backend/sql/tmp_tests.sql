-- select ('{"aa","bbb"}':: text[])[3]
-- select array_length(('{"aa","bbb"}':: text[]), 1)

-- select array_agg(tag_name) from unnest(array['tag a','a sfd']) tag_name
-- left join bm.tag t on t.name = tag_name
-- where t.Id is null

select t.name, t.Id from bm.tag t;
select b.title, b.url, b.Id from bm.bookmark b;

SELECT 
  t.name, pt.name
FROM bm.tag_arrow ta
inner join bm.tag t on t.id = ta.tag_id
inner join bm.tag pt on pt.id = ta.parent_tag_id



-- 'programming' <- 'programming language' <- 'php','haskell'
-- 'programming' <- 'functional programming' <- 'haskell'
insert into bm.tag(id, name)
values
    (bm.generate_id(),'php'),
    (bm.generate_id(),'haskell'),
    (bm.generate_id(),'programming language'),
    (bm.generate_id(),'functional programming'),
    (bm.generate_id(),'programming');

insert into bm.tag_arrow(tag_id, parent_tag_id)
values ('34ae52fa-e263-11e7-a199-e722cd250b13','6ac4bdc2-e26b-11e7-a19d-eb51d267e1cc');



insert into bm.tag(id, name)
values (bm.generate_id(),'computer science');

-- 'php';'34ad8fa0-e263-11e7-a196-6fae0a5ad98a'
-- 'haskell';'34adb6ba-e263-11e7-a197-5b7f85a61983'
-- 'programming language';'34ae2bea-e263-11e7-a198-1ffbe6aa0449'
-- 'functional programming';'34ae52fa-e263-11e7-a199-e722cd250b13'
-- 'programming';'34ae7a0a-e263-11e7-a19a-6746008cb44a'
insert into bm.tag_arrow(tag_id, parent_tag_id)
values
    ('34ad8fa0-e263-11e7-a196-6fae0a5ad98a','34ae2bea-e263-11e7-a198-1ffbe6aa0449'),
    ('34adb6ba-e263-11e7-a197-5b7f85a61983','34ae2bea-e263-11e7-a198-1ffbe6aa0449'),
    ('34adb6ba-e263-11e7-a197-5b7f85a61983','34ae52fa-e263-11e7-a199-e722cd250b13'),
    ('34ae2bea-e263-11e7-a198-1ffbe6aa0449','34ae7a0a-e263-11e7-a19a-6746008cb44a'),
    ('34ae52fa-e263-11e7-a199-e722cd250b13','34ae7a0a-e263-11e7-a19a-6746008cb44a');


select bm.create_bookmark('http://haskell.org', 'haskell title', '', 
    1::smallint, bm.get_utc_now(), array['haskell']);
-- clear
delete from bm.bookmark_tag bt where bt.bookmark_id 
    in (select b.Id from bm.bookmark b where b.title = 'haskell title');
delete from bm.tag;
delete from bm.bookmark where title = 'haskell title';



-- test fix_bookmark_tags
select bm.fix_bookmark_tags('29a742f8-e264-11e7-a19c-cbd5778cd6df');



select b.url, t.name, t.id from bm.bookmark_tag bt
inner join bm.tag t on t.id = bt.tag_id
inner join bm.bookmark b on b.id = bt.bookmark_id;

delete from bm.bookmark_tag where tag_id in ('34ae52fa-e263-11e7-a199-e722cd250b13',
'34ae7a0a-e263-11e7-a19a-6746008cb44a','34ae2bea-e263-11e7-a198-1ffbe6aa0449',
'6ac4bdc2-e26b-11e7-a19d-eb51d267e1cc')



-- test bm.update_favicon
select bm.update_favicon('29a742f8-e264-11e7-a19c-cbd5778cd6df', E'Th\\000omas'::bytea);
select * from bm.favicon;
select md5(E'Th\\000omas'::bytea);



--test bm.root_tag
select * from bm.root_tag rt
order by rt.name asc, rt.date_added desc
limit 20 offset 1;


-- test bm.get_bookmarks_of_tag
select * from bm.get_bookmarks_of_tag('34ae2bea-e263-11e7-a198-1ffbe6aa0449');


-- test bm.get_child_tags
select * from bm.get_child_tags('34ae7a0a-e263-11e7-a19a-6746008cb44a');


-- create a root bookmark
select bm.create_bookmark('http://root.com', 'root title', '', 1::smallint, bm.get_utc_now(), array[]::text[]);
--'38bfef0e-e423-11e7-b589-6767f35bea96'

--test bm.root_bookmark
select * from bm.root_bookmark


-- test bm.get_ascendant_tags()
select * from bm.tag t
select * from bm.get_ascendant_tags('34adb6ba-e263-11e7-a197-5b7f85a61983');


-- test bm.check_tag_arrow
select * from bm.tag
select * from bm.check_tag_arrow('34ad8fa0-e263-11e7-a196-6fae0a5ad98a', '34ae7a0a-e263-11e7-a19a-6746008cb44a');
select * from bm.check_tag_arrow('34ad8fa0-e263-11e7-a196-6fae0a5ad98a', '6ac4bdc2-e26b-11e7-a19d-eb51d267e1cc');
select * from bm.check_tag_arrow('34adb6ba-e263-11e7-a197-5b7f85a61983', '6ac4bdc2-e26b-11e7-a19d-eb51d267e1cc');


--- I decided that this function is not needed after I have written it
/*
CREATE OR REPLACE FUNCTION bm.get_bookmark_by_id(bookmark_id uuid)
RETURNS TABLE(
    id uuid, url text, title text, description text, visit_count smallint,
    last_visit_date timestamp, snapshot_url text, date_added timestamp,
    date_modified timestamp, favicon_data bytea, tags jsonb
    ) AS $$
#variable_conflict use_variable
BEGIN
    return QUERY with tags_of_bookmark as (
        SELECT
            jsonb_agg(jsonb_build_object('name', t.name, 'id', t.id)) as tags
        from bm.bookmark_tag bt
        inner join bm.tag t on t.id = bt.tag_id
        where bt.bookmark_id = bookmark_id
        group by bt.bookmark_id
    )
    select b.id, b.url, b.title, b.description,
        b.visit_count, b.last_visit_date, b.snapshot_url,
        b.date_added, b.date_modified,
        fa.data as favicon_data, tob.tags
    from bm.bookmark b
    left join tags_of_bookmark tob on 1 = 1
    left join bm.favicon fa on fa.id = b.favicon_id
    where b.id = bookmark_id;
END;
$$ LANGUAGE plpgsql stable;

do $tests$ begin
if EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = 'unit_tests') then

create or replace function unit_tests.get_bookmark_by_id() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark1_id uuid := bm.generate_id();
    bookmark2_id uuid := bm.generate_id();
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
    values  (bookmark1_id, bookmark1_id, 't1', 'd1', 42::smallint, '2000-01-01'::timestamp, bookmark1_id, favicon_id),
            (bookmark2_id, bookmark2_id, 't2', 'd2', 1::smallint, '2000-01-02'::timestamp, '', null);

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
    from bm.get_bookmark_by_id(bookmark1_id);

    -- assert 1
    SELECT * FROM assert.is_equal(returned_count, 1) INTO message, result;
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
    from bm.get_bookmark_by_id(bookmark1_id) b;

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

    SELECT * FROM assert.is_equal(returned_snapshot_url, bookmark1_id::text) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_favicon_data, favicon_data) INTO message, result;
    IF not result THEN RETURN message; END IF;

    SELECT * FROM assert.is_equal(returned_tags, expected_tags) INTO message, result;
    IF not result THEN RETURN message; END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

-- tests the case when the bookmark has no tags
create or replace function unit_tests.get_bookmark_by_id_no_tags() returns test_result as $$
#variable_conflict use_variable
declare
    message test_result;
    result boolean;
    bookmark1_id uuid := bm.generate_id();
    bookmark2_id uuid := bm.generate_id();
    returned_count int;
begin
    -- arrange
    insert into bm.bookmark(id, url, title, description, visit_count, last_visit_date, snapshot_url)
    values  (bookmark1_id, bookmark1_id, 't1', 'd1', 42::smallint, '2000-01-01'::timestamp, ''),
            (bookmark2_id, bookmark2_id, 't2', 'd2', 1::smallint, '2000-01-02'::timestamp, '');

    -- act
    select count(*) into returned_count
    from bm.get_bookmark_by_id(bookmark1_id);

    -- assert
    SELECT * FROM assert.is_equal(returned_count, 1) INTO message, result;
    IF not result THEN RETURN message; END IF;

    return assert.ok('End of test.');
end;
$$ language plpgsql;

end if;
end $tests$;
*/



BEGIN TRANSACTION;
--SELECT * FROM unit_tests.create_root_tag(); 
SELECT * FROM unit_tests.begin();
ROLLBACK TRANSACTION;



select * from bm.bookmark_tag bt
where bt.tag_id = '34ae2bea-e263-11e7-a198-1ffbe6aa0449'


insert into bm.tag(id, name)
values
    (bm.generate_id(),'aaa'), --"42657d18-14e7-11e8-bcf2-8fbe6493b9ed"
    (bm.generate_id(),'bbb'); --"4265cb38-14e7-11e8-bcf3-fb8959e89ebf"
delete from bm.tag where name in ('aaaa','bbbb');

select * from bm.tag_arrow
select * from bm.tag
select * from bm.bookmark


