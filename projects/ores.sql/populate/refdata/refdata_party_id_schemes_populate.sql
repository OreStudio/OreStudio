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
 * Codes are aligned with ores_dq_coding_schemes_tbl (subject_area 'Parties')
 * and cross-referenced via coding_scheme_code.
 *
 * This script is idempotent - uses INSERT ON CONFLICT.
 */

\echo '--- Party Identifier Schemes ---'

insert into ores_refdata_party_id_schemes_tbl (
    tenant_id, code, version, name, description, coding_scheme_code, display_order,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_iam_system_tenant_id_fn(), 'LEI', 0, 'Legal Entity Identifier',
     'Legal Entity Identifier (ISO 17442, 20-char alphanumeric). Global standard for legal entities.',
     'LEI', 1, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'BIC', 0, 'Business Identifier Code',
     'Business Identifier Code (SWIFT/BIC, ISO 9362). Used for banks and financial institutions.',
     'BIC', 2, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'MIC', 0, 'Market Identifier Code',
     'Market Identifier Code (ISO 10383). Identifies trading venues (e.g., XNYS, XLON). Often linked to party context in trade reports.',
     'MIC', 3, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'NATIONAL_ID', 0, 'National Identifier',
     'National identifiers (e.g., passport number, tax ID, SIREN, ORI, national ID card). Covers MiFID II client identification requirements.',
     'NATIONAL_ID', 4, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'CEDB', 0, 'CFTC Entity Directory',
     'CFTC Entity Directory (US-specific). Used in CFTC swap data reporting for non-LEI entities.',
     'CEDB', 5, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'NATURAL_PERSON', 0, 'Natural Person',
     'Generic identifier for individuals (e.g., employee ID, trader ID). Not standardized; value interpreted contextually.',
     'NATURAL_PERSON', 6, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'ACER', 0, 'EU Agency for Energy Regulation',
     'ACER (EU Agency for Energy Regulation) code. Required for REMIT reporting by non-LEI energy market participants. Officially supported in FpML energy extensions.',
     'ACER', 7, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'DTCC_PARTICIPANT_ID', 0, 'DTCC Participant ID',
     'DTCC Participant ID: A unique numeric identifier (typically 4-6 digits) assigned by the Depository Trust & Clearing Corporation (DTCC) to member firms authorized to participate in U.S. clearing and settlement systems.',
     'DTCC_PARTICIPANT_ID', 8, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'MPID', 0, 'Market Participant ID',
     'Also known as AII (ATS Identification Indicator). A four-character alphanumeric code assigned by FINRA to broker-dealers and alternative trading systems (ATSs) operating in U.S. equities markets.',
     'MPID', 9, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes'),
    (ores_iam_system_tenant_id_fn(), 'INTERNAL', 0, 'Internal',
     'Proprietary/internal system identifiers (e.g., client ID in your OMS, CRM, or clearing system).',
     'INTERNAL', 10, current_user, current_user, 'system.initial_load', 'Initial population of party identifier schemes')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

-- Summary
select 'refdata_party_id_schemes' as entity, count(*) as count
from ores_refdata_party_id_schemes_tbl;
