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
 * Tenant Statuses Population Script
 *
 * Seeds the database with tenant lifecycle status definitions.
 * This script is idempotent.
 */

\echo '--- Tenant Statuses ---'

insert into ores_iam_tenant_statuses_tbl (status, name, description, display_order) values
    ('active', 'Active', 'Tenant is active and fully operational', 0),
    ('suspended', 'Suspended', 'Tenant is temporarily suspended - users cannot log in', 10),
    ('terminated', 'Terminated', 'Tenant has been permanently terminated', 20)
on conflict (status) do nothing;

-- Summary
select 'Tenant Statuses' as entity, count(*) as count
from ores_iam_tenant_statuses_tbl;
