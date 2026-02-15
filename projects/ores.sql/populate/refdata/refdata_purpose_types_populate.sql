/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Purpose Types Population Script
 *
 * Populates the classification of portfolio purpose types.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Purpose Types ---'

insert into ores_refdata_purpose_types_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'Risk', 0, 'Risk',
     'Portfolio used for risk aggregation and risk management reporting.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of purpose types'),
    (ores_iam_system_tenant_id_fn(), 'Regulatory', 0, 'Regulatory',
     'Portfolio used for regulatory capital and compliance reporting.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of purpose types'),
    (ores_iam_system_tenant_id_fn(), 'ClientReporting', 0, 'Client Reporting',
     'Portfolio used for client-facing reporting and statements.',
     3, current_user, current_user, 'system.initial_load', 'Initial population of purpose types'),
    (ores_iam_system_tenant_id_fn(), 'Internal', 0, 'Internal',
     'Portfolio used for internal management reporting and P&L attribution.',
     4, current_user, current_user, 'system.initial_load', 'Initial population of purpose types')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_purpose_types' as entity, count(*) as count
from ores_refdata_purpose_types_tbl;
