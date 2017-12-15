SELECT 
  t.name, pt.name
FROM bm.tag_arrow ta
inner join bm.tag t on t.id = ta.tag_id
inner join bm.tag pt on pt.id = ta.parent_tag_id

-- select ('{"aa","bbb"}':: text[])[3]
-- select array_length(('{"aa","bbb"}':: text[]), 1)

-- select array_agg(tag_name) from unnest(array['tag a','a sfd']) tag_name
-- left join bm.tag t on t.name = tag_name
-- where t.Id is null

select t.name from bm.tag t;
select b.title from bm.bookmark b;

SELECT 
  t.name, pt.name
FROM bm.tag_arrow ta
inner join bm.tag t on t.id = ta.tag_id
inner join bm.tag pt on pt.id = ta.parent_tag_id





-- test
select bm.create_bookmark('http://example.com', 'example title', '', 1::smallint, bm.get_utc_now(), array['tag a','asdf']);
-- clear
delete from bm.bookmark_tag bt where bt.tag_id in (select t.Id from bm.tag t where t.name in ('tag a', 'asdf'));
delete from bm.tag where name = 'asdf';
delete from bm.bookmark where title = 'example title';

select id, snapshot_url from bm.bookmark;
select bm.update_snapshot_url('e58e0230-e1e8-11e7-94d1-6b2f80fe41a2', 'http://snapshot_url/text');
