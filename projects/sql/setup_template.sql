/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- Drop template database if it exists
drop database if exists oresdb_template;

-- Create template database with clean template0 as base
create database oresdb_template with template = template0;

-- Grant permissions to ores user
grant all privileges on database oresdb_template to ores;

-- Connect to template database to create schema
\c oresdb_template

-- Run all schema creation scripts
\ir ./create_all.sql

-- Mark database as template to prevent accidental direct connections
-- NOTE: This requires superuser privileges, so it's commented out
-- The database will still work as a template without this
-- update pg_database set datistemplate = true where datname = 'oresdb_template';
