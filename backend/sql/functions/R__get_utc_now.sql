CREATE OR REPLACE FUNCTION bm.get_utc_now()
returns timestamp AS $$
    select (now() at time zone 'utc')
$$ LANGUAGE SQL STABLE;

