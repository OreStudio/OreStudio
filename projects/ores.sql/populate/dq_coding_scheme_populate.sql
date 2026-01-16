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
 * Data Quality Scheme Population Script
 *
 * Seeds the database with coding/identification schemes based on FPML concepts.
 * This script is idempotent.
 *
 * Scheme types by subject area:
 * - Parties: LEI, BIC, and other party identification schemes
 * - Currencies: ISO 4217 currency code scheme
 * - Countries: ISO 3166 country code scheme
 */

set schema 'ores';

-- =============================================================================
-- Helper Functions
-- =============================================================================

-- Helper function to insert a scheme if it doesn't exist
create or replace function ores.upsert_dq_coding_scheme(
    p_code text,
    p_name text,
    p_subject_area_name text,
    p_domain_name text,
    p_uri text,
    p_description text
) returns void as $$
begin
    if not exists (
        select 1 from ores.dq_coding_scheme_tbl
        where code = p_code and valid_to = ores.utility_infinity_timestamp_fn()
    ) then
        insert into ores.dq_coding_scheme_tbl (
            code, version, name, subject_area_name, domain_name, uri, description,
            modified_by, change_reason_code, change_commentary, valid_from, valid_to
        )
        values (
            p_code, 0, p_name, p_subject_area_name, p_domain_name, p_uri, p_description,
            'system', 'system.new_record', 'System seed data - data quality scheme',
            current_timestamp, ores.utility_infinity_timestamp_fn()
        );
        raise notice 'Created data quality scheme: %', p_code;
    else
        raise notice 'Data quality scheme already exists: %', p_code;
    end if;
end;
$$ language plpgsql;

-- =============================================================================
-- Data Quality Schemes
-- =============================================================================

\echo '--- Data Quality Schemes ---'

-- Party identification schemes
select ores.upsert_dq_coding_scheme(
    'LEI',
    'Legal Entity Identifier',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso17442',
    'Legal Entity Identifier (ISO 17442, 20-char alphanumeric). Global standard for legal entities.'
);

select ores.upsert_dq_coding_scheme(
    'BIC',
    'Business Identifier Code',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso9362',
    'Business Identifier Code (SWIFT/BIC, ISO 9362). Used for banks and financial institutions.'
);

select ores.upsert_dq_coding_scheme(
    'MIC',
    'Market Identifier Code',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso10383',
    'Market Identifier Code (ISO 10383). Identifies trading venues (e.g., XNYS, XLON). Note: Technically a venue, but often linked to party context in trade reports.'
);

select ores.upsert_dq_coding_scheme(
    'CEDB',
    'CFTC Entity Directory',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/cedb',
    'CFTC Entity Directory (US-specific). Used in CFTC swap data reporting for non-LEI entities.'
);

select ores.upsert_dq_coding_scheme(
    'NATURAL_PERSON',
    'Natural Person',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/person-id',
    'Generic identifier for individuals (e.g., employee ID, trader ID). Not standardized; value interpreted contextually.'
);

select ores.upsert_dq_coding_scheme(
    'NATIONAL_ID',
    'National Identifier',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/national-id',
    'National identifiers (e.g., passport number, tax ID, SIREN, ORI, national ID card). Covers MiFID II client identification requirements.'
);

select ores.upsert_dq_coding_scheme(
    'INTERNAL',
    'Internal',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/party-id-internal',
    'Proprietary/internal system identifiers (e.g., client ID in your OMS, CRM, or clearing system).'
);

select ores.upsert_dq_coding_scheme(
    'ACER',
    'EU Agency for Energy Regulation',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/acer-code',
    'ACER (EU Agency for Energy Regulation) code. Required for REMIT reporting by non-LEI energy market participants. Officially supported in FpML energy extensions.'
);

select ores.upsert_dq_coding_scheme(
    'DTCC',
    'Depository Trust & Clearing Corporation Participant ID',
    'Parties',
    'Reference Data',
    null,
    'DTCC Participant ID: A unique numeric identifier (typically 4-6 digits) assigned by the Depository Trust & Clearing Corporation (DTCC) to member firms authorized to participate in U.S. clearing and settlement systems, including DTC, NSCC, and FICC. Used in post-trade processing, trade reporting, and regulatory submissions in U.S. capital markets.'
);

select ores.upsert_dq_coding_scheme(
    'MPID',
    'Market Participant Identifier',
    'Parties',
    'Reference Data',
    null,
    'Also known as AII (ATS Identification Indicator). A four-character alphanumeric code assigned by FINRA to broker-dealers and alternative trading systems (ATSs) operating in U.S. equities markets. Used to identify the originating or executing firm in trade reports (e.g., in OATS, TRACE, or consolidated tape reporting). Required for all SEC-registered trading venues and participants in U.S. equity and options markets.'
);

-- Country identification schemes
select ores.upsert_dq_coding_scheme(
    'ISO_3166_1_ALPHA_2',
    'ISO 3166-1 Alpha-2 Country Code',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-2',
    'ISO 3166-1 alpha-2 country codes. Two-letter codes (e.g., US, GB, DE) for countries and dependent territories. The most commonly used country code format in financial messaging.'
);

select ores.upsert_dq_coding_scheme(
    'ISO_3166_1_ALPHA_3',
    'ISO 3166-1 Alpha-3 Country Code',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-3',
    'ISO 3166-1 alpha-3 country codes. Three-letter codes (e.g., USA, GBR, DEU) for countries and dependent territories. More descriptive than alpha-2 codes.'
);

-- Currency identification schemes
select ores.upsert_dq_coding_scheme(
    'ISO_4217',
    'ISO 4217 Currency Code',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso4217',
    'ISO 4217 currency codes. Three-letter alphabetic codes (e.g., USD, EUR, GBP) and three-digit numeric codes for currencies. The universal standard for currency identification in financial transactions.'
);

-- =============================================================================
-- Cleanup
-- =============================================================================

drop function ores.upsert_dq_coding_scheme(text, text, text, text, text, text);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Data Quality Schemes' as entity, count(*) as count
from ores.dq_coding_scheme_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
