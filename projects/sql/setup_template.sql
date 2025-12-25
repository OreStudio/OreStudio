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

/**
 * Template Database Setup
 *
 * Creates the ores_template database which serves as a blueprint for
 * creating new ORES database instances quickly.
 *
 * USAGE:
 *   psql -U postgres -f setup_template.sql
 *
 * PREREQUISITES:
 *   - PostgreSQL superuser access
 *   - The 'ores' user must exist (run setup.sql first)
 *
 * After running this script, create new instances using:
 *   psql -U postgres -f create_instance.sql
 *
 * Or for a specific name:
 *   psql -U postgres -v db_name='my_database' -f create_instance.sql
 */

-- Drop template database if it exists (requires superuser)
-- Uncomment these lines to recreate the template:
-- update pg_database set datistemplate = false where datname = 'ores_template';
-- drop database if exists ores_template;

-- Create template database with clean template0 as base
create database ores_template with template = template0;

-- Grant permissions to ores user
grant all privileges on database ores_template to ores;

-- Connect to template database to create schema
\c ores_template

-- Create the complete schema (tables, functions, reference data)
\ir ./template/create_schema.sql

-- NOTE: Instance-specific initialization (feature flags) is NOT included here.
-- Each instance created from this template should run instance/init_instance.sql

\echo ''
\echo '=========================================='
\echo 'Template database created successfully!'
\echo '=========================================='
\echo ''
\echo 'To create a new instance with a whimsical name:'
\echo '  psql -U postgres -f create_instance.sql'
\echo ''
\echo 'To create an instance with a specific name:'
\echo '  psql -U postgres -v db_name=my_database -f create_instance.sql'
\echo ''
