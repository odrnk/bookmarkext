-- select ('{"aa","bbb"}':: text[])[3]
-- select array_length(('{"aa","bbb"}':: text[]), 1)

-- select array_agg(tag_name) from unnest(array['tag a','a sfd']) tag_name
-- left join bm.tag t on t.name = tag_name
-- where t.Id is null

select t.name, t.Id from bm.tag t;
select b.title, b.Id from bm.bookmark b;

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









BEGIN TRANSACTION;
--SELECT * FROM unit_tests.create_root_tag(); 
SELECT * FROM unit_tests.begin();
ROLLBACK TRANSACTION;
