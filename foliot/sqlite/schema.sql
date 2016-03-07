-- -*- mode: sql; coding: utf-8 -*-

---- Copyright (C) 2011, 2012, 2013, 2016
---- Free Software Foundation, Inc.

---- This file is part of GNU Foliot.

---- GNU Foliot is free software: you can redistribute it and/or
---- modify it under the terms of the GNU General Public License as
---- published by the Free Software Foundation, either version 3 of
---- the License, or (at your option) any later version.

---- GNU Foliot is distributed in the hope that it will be useful, but
---- WITHOUT ANY WARRANTY; without even the implied warranty of
---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
---- General Public License for more details.

---- You should have received a copy of the GNU General Public License
---- along with GNU Foliot.  If not, see
---- <http://www.gnu.org/licenses/>.
----

--- Commentary:

-- this file is actually not 'called' nor 'used' but given here as the
-- effective GNU Foliot database schema 'in use' by the application.

--- Code:

drop table foliot;

create table foliot (
  id             integer primary key not null,
  date_          integer,
  who            text,
  for_whom       text,
  duration       float,
  to_be_charged  text,
  charging_type  text,
  what           text,
  description    text,
  created_the    integer,
  created_by     text,
  modified_the   integer,
  modified_by    text,
  imported_id    integer,
  imported_db    integer
);

--  alter table foliot add imported_id integer default '-1' not null;
--  alter table foliot add imported_db integer default '-1' not null;

drop table foliot_printing_templates;

create table foliot_printing_templates (
  id               integer primary key not null,
  name             text,
  items            text,
  mode             text,
  group_and_sort   text
);


---
--- Import
---

drop table if exists foliot_imported_db;

create table foliot_imported_db (
  id               integer primary key not null,
  name             text, -- actually it is the filename
  imported_the     integer,
  imported_by      text,
  colour_set       integer
);

-- 2013/09/04
-- alter table foliot_imported_db add column colour_set text;


---
--- Db infos
---

drop table if exists foliot_shinning;

create table foliot_shinning (
  id         integer primary key not null,
  room_237   text,
);
