CREATE OR REPLACE FUNCTION bm.get_utc_now()
returns timestamp AS $$
begin
    return (now() at time zone 'utc');
END
$$ LANGUAGE plpgsql;
