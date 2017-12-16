CREATE OR REPLACE FUNCTION bm.generate_id()
returns uuid AS $$ select uuid_generate_v1mc() $$ LANGUAGE SQL STABLE;
