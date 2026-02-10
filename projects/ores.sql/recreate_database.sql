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
 * Recreate Database from Scratch (Postgres Superuser Phase)
 *
 * Full wipe and rebuild for development environments. Tears down all ORES
 * components and recreates users and an empty database. The schema setup
 * phase runs separately as ores_ddl_user (see recreate_database.sh).
 *
 * USAGE:
 *   Typically called via recreate_database.sh (handles confirmation and DDL phase):
 *     ./recreate_database.sh -p postgres_pass -d ddl_pass -c cli_pass -w wt_pass -m comms_pass -h http_pass -t test_ddl_pass -T test_dml_pass -r ro_pass [-y] [--no-sql-validation]
 *
 *   Direct psql usage (no confirmation prompt):
 *     psql -U postgres -v ddl_password='...' -v cli_password='...' -v wt_password='...' -v comms_password='...' -v http_password='...' -v test_ddl_password='...' -v test_dml_password='...' -v ro_password='...' -v db_name='...' -f recreate_database.sql
 *
 * Variables:
 *   :ddl_password - password for DDL operations
 *   :cli_password - password for CLI service
 *   :wt_password - password for Web Toolkit service
 *   :comms_password - password for Communications service
 *   :http_password - password for HTTP service
 *   :test_ddl_password - password for test DDL operations
 *   :test_dml_password - password for test DML operations
 *   :ro_password - password for read-only access
 *   :db_name - database name to create
 */
\pset pager off
\pset tuples_only on
\timing off

-- Skip confirmation in teardown_all.sql - shell script handles this
\set skip_confirm 1

\ir teardown_all.sql
\ir setup_user.sql
\ir create_database.sql
