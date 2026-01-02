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
 * System Population Script
 *
 * Seeds the database with essential system data required for the
 * application to function. All scripts are idempotent and can be
 * safely re-run without creating duplicate data.
 *
 * This script is run automatically during template creation and seeds:
 * 1. RBAC: Permissions, roles, role-permission assignments
 * 2. System Flags: Bootstrap mode, user signups, etc.
 *
 * NOTE: Reference data (currencies, flags, images) is NOT included here.
 * Reference data should be imported via the CLI after database creation:
 *   ores.cli import currencies <file.xml>
 *
 * Or use the separate reference data script:
 *   psql -U ores -d your_database -f populate/reference_data.sql
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/populate.sql
 */

\echo '=== Starting System Population ==='
\echo ''

-- RBAC (Role-Based Access Control)
\echo '--- RBAC Data ---'
\ir permissions_populate.sql
\ir roles_populate.sql

-- System Flags
\echo ''
\echo '--- System Flags ---'
\ir system_flags_populate.sql

\echo ''
\echo '=== System Population Complete ==='

-- Summary
\echo ''
\echo '--- Summary ---'

select 'Permissions' as entity, count(*) as count
from ores.permissions where valid_to = ores.infinity_timestamp()
union all
select 'Roles', count(*)
from ores.roles where valid_to = ores.infinity_timestamp()
union all
select 'System Flags', count(*)
from ores.feature_flags where name like 'system.%' and valid_to = ores.infinity_timestamp()
order by entity;
