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
 * Party Identifier Schemes Population Script
 *
 * Populates the classification of external identifier types used to
 * identify parties and counterparties across systems and jurisdictions.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Party Identifier Schemes ---'

insert into ores_refdata_party_id_schemes_tbl (
    tenant_id, code, version, name, description, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'LEI', 0, 'Legal Entity Identifier',
     'ISO 17442 Legal Entity Identifier - a 20-character alphanumeric code assigned by Local Operating Units of the Global LEI System.',
     1, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'BIC', 0, 'Business Identifier Code',
     'ISO 9362 Business Identifier Code (SWIFT code) - an 8 or 11 character code identifying financial institutions.',
     2, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'NationalId', 0, 'National Identifier',
     'A national registration or company identifier issued by a government authority (e.g., Companies House number, SEC CIK).',
     3, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'TaxId', 0, 'Tax Identifier',
     'A tax identification number issued by a revenue authority (e.g., EIN, TIN, VAT number).',
     4, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'InternalCode', 0, 'Internal Code',
     'An organisation-specific internal identifier used within proprietary systems.',
     5, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_party_id_schemes' as entity, count(*) as count
from ores_refdata_party_id_schemes_tbl;
