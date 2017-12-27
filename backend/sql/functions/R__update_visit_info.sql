CREATE OR REPLACE FUNCTION bm.update_visit_info(
    url text, last_visit_date timestamp, visit_count smallint)
RETURNS void AS $$
#variable_conflict use_variable
BEGIN
    update bm.bookmark b
    set last_visit_date = last_visit_date,
        visit_count = visit_count,
        date_modified = bm.get_utc_now()
    where b.url = url;
END;
$$ LANGUAGE plpgsql;
