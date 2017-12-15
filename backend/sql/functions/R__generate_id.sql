CREATE OR REPLACE FUNCTION bm.generate_id()
returns uuid AS $$
begin
    return uuid_generate_v1mc();
end
$$ LANGUAGE plpgsql;
