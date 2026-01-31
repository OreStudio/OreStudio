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
 * Tenant Types Population Script
 *
 * Seeds the database with tenant type definitions.
 * This script is idempotent.
 */

\echo '--- Tenant Types ---'

insert into ores_iam_tenant_types_tbl (type, name, description, display_order) values
    ('platform', 'Platform', 'System-level tenant for platform administration and shared governance data', 0),
    ('organisation', 'Organisation', 'Customer organisation tenant for isolated business operations', 10)
on conflict (type) do nothing;

-- Summary
select 'Tenant Types' as entity, count(*) as count
from ores_iam_tenant_types_tbl;
