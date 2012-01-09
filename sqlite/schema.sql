-- -*- mode: sql; coding: utf-8 -*-

---- Copyright (C) 2011, 2012
---- Free Software Foundation, Inc.

---- This file is part of Kisê.

---- Kisê is free software: you can redistribute it and/or modify it
---- under the terms of the GNU General Public License as published by
---- the Free Software Foundation, either version 3 of the License, or
---- (at your option) any later version.

---- Kisê is distributed in the hope that it will be useful, but
---- WITHOUT ANY WARRANTY; without even the implied warranty of
---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
---- General Public License for more details.

---- You should have received a copy of the GNU General Public License
---- along with Kisê.  If not, see <http://www.gnu.org/licenses/>.
----

--- Commentary:

-- this file is actually not 'called' nor 'used' but given here as the
-- effective Kisê database schema 'in use' by the application.

--- Code:

drop table kise;

create table kise (
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
  modified_by    text
);


drop table kise_printing_templates;

create table kise_printing_templates (
  id               integer primary key not null,
  name             text,
  items            text,
  mode             text,
  group_and_sort   text
);
