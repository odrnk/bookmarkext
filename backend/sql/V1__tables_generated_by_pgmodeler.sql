-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.0
-- PostgreSQL version: 9.6
-- Project Site: pgmodeler.com.br
-- Model Author: ---


-- Database creation must be done outside an multicommand file.
-- These commands were put in this file only for convenience.
-- -- object: bm | type: DATABASE --
-- -- DROP DATABASE IF EXISTS bm;
-- CREATE DATABASE bm
-- ;
-- -- ddl-end --
-- 

-- object: bm | type: SCHEMA --
-- DROP SCHEMA IF EXISTS bm CASCADE;
CREATE SCHEMA bm;
-- ddl-end --
ALTER SCHEMA bm OWNER TO postgres;
-- ddl-end --

SET search_path TO pg_catalog,public,bm;
-- ddl-end --

-- object: bm.bookmark | type: TABLE --
-- DROP TABLE IF EXISTS bm.bookmark CASCADE;
CREATE TABLE bm.bookmark(
	id uuid NOT NULL,
	url text NOT NULL,
	title text NOT NULL,
	description text NOT NULL,
	favicon_id uuid,
	visit_count smallint NOT NULL,
	last_visit_date timestamp NOT NULL,
	snapshot_url text NOT NULL,
	date_added timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	date_modified timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	CONSTRAINT bookmark_pk PRIMARY KEY (id),
	CONSTRAINT ak_bookmark_url UNIQUE (url)

);
-- ddl-end --
ALTER TABLE bm.bookmark OWNER TO postgres;
-- ddl-end --

-- object: bm.favicon | type: TABLE --
-- DROP TABLE IF EXISTS bm.favicon CASCADE;
CREATE TABLE bm.favicon(
	id uuid NOT NULL,
	data bytea NOT NULL,
	date_added timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	date_modified timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	checksum text NOT NULL,
	CONSTRAINT favicon_pk PRIMARY KEY (id),
	CONSTRAINT ak_favicon_checksum UNIQUE (checksum)

);
-- ddl-end --
ALTER TABLE bm.favicon OWNER TO postgres;
-- ddl-end --

-- object: bm.tag | type: TABLE --
-- DROP TABLE IF EXISTS bm.tag CASCADE;
CREATE TABLE bm.tag(
	id uuid NOT NULL,
	name text NOT NULL,
	date_added timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	date_modified timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	CONSTRAINT tags_pk PRIMARY KEY (id),
	CONSTRAINT ak_tag_name UNIQUE (name)

);
-- ddl-end --
ALTER TABLE bm.tag OWNER TO postgres;
-- ddl-end --

-- object: bm.tag_arrow | type: TABLE --
-- DROP TABLE IF EXISTS bm.tag_arrow CASCADE;
CREATE TABLE bm.tag_arrow(
	tag_id uuid NOT NULL,
	parent_tag_id uuid NOT NULL,
	date_added timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	CONSTRAINT tag_arrow_pk PRIMARY KEY (tag_id,parent_tag_id)

);
-- ddl-end --
ALTER TABLE bm.tag_arrow OWNER TO postgres;
-- ddl-end --

-- object: bm.bookmark_tag | type: TABLE --
-- DROP TABLE IF EXISTS bm.bookmark_tag CASCADE;
CREATE TABLE bm.bookmark_tag(
	bookmark_id uuid NOT NULL,
	tag_id uuid NOT NULL,
	date_added timestamp NOT NULL DEFAULT (now() at time zone 'utc'),
	CONSTRAINT bookmark_tag_pk PRIMARY KEY (bookmark_id,tag_id)

);
-- ddl-end --
ALTER TABLE bm.bookmark_tag OWNER TO postgres;
-- ddl-end --

-- object: fk_bookmark_favicon_id | type: CONSTRAINT --
-- ALTER TABLE bm.bookmark DROP CONSTRAINT IF EXISTS fk_bookmark_favicon_id CASCADE;
ALTER TABLE bm.bookmark ADD CONSTRAINT fk_bookmark_favicon_id FOREIGN KEY (favicon_id)
REFERENCES bm.favicon (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: fk_tag_arrow_tag_id | type: CONSTRAINT --
-- ALTER TABLE bm.tag_arrow DROP CONSTRAINT IF EXISTS fk_tag_arrow_tag_id CASCADE;
ALTER TABLE bm.tag_arrow ADD CONSTRAINT fk_tag_arrow_tag_id FOREIGN KEY (tag_id)
REFERENCES bm.tag (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: fk_tag_arrow_parent_tag_id | type: CONSTRAINT --
-- ALTER TABLE bm.tag_arrow DROP CONSTRAINT IF EXISTS fk_tag_arrow_parent_tag_id CASCADE;
ALTER TABLE bm.tag_arrow ADD CONSTRAINT fk_tag_arrow_parent_tag_id FOREIGN KEY (parent_tag_id)
REFERENCES bm.tag (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: fk_bookmark_tag_bookmark_id | type: CONSTRAINT --
-- ALTER TABLE bm.bookmark_tag DROP CONSTRAINT IF EXISTS fk_bookmark_tag_bookmark_id CASCADE;
ALTER TABLE bm.bookmark_tag ADD CONSTRAINT fk_bookmark_tag_bookmark_id FOREIGN KEY (bookmark_id)
REFERENCES bm.bookmark (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: fk_bookmark_tag_tag_id | type: CONSTRAINT --
-- ALTER TABLE bm.bookmark_tag DROP CONSTRAINT IF EXISTS fk_bookmark_tag_tag_id CASCADE;
ALTER TABLE bm.bookmark_tag ADD CONSTRAINT fk_bookmark_tag_tag_id FOREIGN KEY (tag_id)
REFERENCES bm.tag (id) MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --


