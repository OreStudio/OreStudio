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
 * ORES User Setup
 *
 * Creates the 'ores' application user. This is the first step in setting up
 * the ORES database infrastructure.
 *
 * USAGE:
 *   psql -U postgres -f setup_user.sql
 *
 * NEXT STEPS:
 *   1. admin/setup_admin.sql  - Create admin database
 *   2. setup_template.sql     - Create template database
 *   3. create_instance.sql    - Create database instance
 *
 * NOTE: For production, generate a secure password:
 *   pwgen -c 25 1
 */

\set ON_ERROR_STOP on

-- Create the application user
-- For production, change 'ores' to a secure password
CREATE USER ores WITH PASSWORD 'ores';

\echo ''
\echo '=========================================='
\echo 'ORES user created successfully!'
\echo '=========================================='
\echo ''
\echo 'Next step: Create admin database'
\echo '  psql -U postgres -f admin/setup_admin.sql'
\echo ''
