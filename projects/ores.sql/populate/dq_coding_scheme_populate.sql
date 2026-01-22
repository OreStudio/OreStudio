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
 * Data Quality Coding Scheme Population Script
 *
 * Seeds the database with coding/identification schemes based on FPML concepts.
 * This script is idempotent.
 *
 * Scheme types by subject area:
 * - Parties: LEI, BIC, and other party identification schemes
 * - Currencies: ISO 4217 currency code scheme
 * - Countries: ISO 3166 country code scheme
 *
 * Authority types:
 * - official: ISO and other formal standards bodies
 * - industry: De facto standards from industry bodies (SWIFT, DTCC, FINRA)
 * - internal: Proprietary identifiers
 */

set schema 'ores';

-- =============================================================================
-- Data Quality Coding Schemes
-- =============================================================================

\echo '--- Data Quality Coding Schemes ---'

-- Party identification schemes (official - ISO standards)
select ores.upsert_dq_coding_schemes(
    'LEI',
    'Legal Entity Identifier',
    'official',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso17442',
    'Legal Entity Identifier (ISO 17442, 20-char alphanumeric). Global standard for legal entities.'
);

select ores.upsert_dq_coding_schemes(
    'BIC',
    'Business Identifier Code',
    'official',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso9362',
    'Business Identifier Code (SWIFT/BIC, ISO 9362). Used for banks and financial institutions.'
);

select ores.upsert_dq_coding_schemes(
    'MIC',
    'Market Identifier Code',
    'official',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso10383',
    'Market Identifier Code (ISO 10383). Identifies trading venues (e.g., XNYS, XLON). Note: Technically a venue, but often linked to party context in trade reports.'
);

select ores.upsert_dq_coding_schemes(
    'NATIONAL_ID',
    'National Identifier',
    'official',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/national-id',
    'National identifiers (e.g., passport number, tax ID, SIREN, ORI, national ID card). Covers MiFID II client identification requirements.'
);

-- Party identification schemes (industry - regulatory/consortium standards)
select ores.upsert_dq_coding_schemes(
    'CEDB',
    'CFTC Entity Directory',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/cedb',
    'CFTC Entity Directory (US-specific). Used in CFTC swap data reporting for non-LEI entities.'
);

select ores.upsert_dq_coding_schemes(
    'ACER',
    'EU Agency for Energy Regulation',
    'industry',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/acer-code',
    'ACER (EU Agency for Energy Regulation) code. Required for REMIT reporting by non-LEI energy market participants. Officially supported in FpML energy extensions.'
);

select ores.upsert_dq_coding_schemes(
    'DTCC_PARTICIPANT_ID',
    'Depository Trust & Clearing Corporation Participant ID',
    'industry',
    'Parties',
    'Reference Data',
    null,
    'DTCC Participant ID: A unique numeric identifier (typically 4-6 digits) assigned by the Depository Trust & Clearing Corporation (DTCC) to member firms authorized to participate in U.S. clearing and settlement systems, including DTC, NSCC, and FICC. Used in post-trade processing, trade reporting, and regulatory submissions in U.S. capital markets.'
);

select ores.upsert_dq_coding_schemes(
    'MPID',
    'Market Participant Identifier',
    'industry',
    'Parties',
    'Reference Data',
    null,
    'Also known as AII (ATS Identification Indicator). A four-character alphanumeric code assigned by FINRA to broker-dealers and alternative trading systems (ATSs) operating in U.S. equities markets. Used to identify the originating or executing firm in trade reports (e.g., in OATS, TRACE, or consolidated tape reporting). Required for all SEC-registered trading venues and participants in U.S. equity and options markets.'
);

-- Party identification schemes (internal - proprietary)
select ores.upsert_dq_coding_schemes(
    'NATURAL_PERSON',
    'Natural Person',
    'internal',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/person-id',
    'Generic identifier for individuals (e.g., employee ID, trader ID). Not standardized; value interpreted contextually.'
);

select ores.upsert_dq_coding_schemes(
    'INTERNAL',
    'Internal',
    'internal',
    'Parties',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/party-id-internal',
    'Proprietary/internal system identifiers (e.g., client ID in your OMS, CRM, or clearing system).'
);

-- Country identification schemes (official - ISO standards)
select ores.upsert_dq_coding_schemes(
    'ISO_3166_1_ALPHA_2',
    'ISO 3166-1 Alpha-2 Country Code',
    'official',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-2',
    'ISO 3166-1 alpha-2 country codes. Two-letter codes (e.g., US, GB, DE) for countries and dependent territories. The most commonly used country code format in financial messaging.'
);

select ores.upsert_dq_coding_schemes(
    'ISO_3166_1_ALPHA_3',
    'ISO 3166-1 Alpha-3 Country Code',
    'official',
    'Countries',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso3166-1-alpha-3',
    'ISO 3166-1 alpha-3 country codes. Three-letter codes (e.g., USA, GBR, DEU) for countries and dependent territories. More descriptive than alpha-2 codes.'
);

-- Currency identification schemes (official - ISO standards)
select ores.upsert_dq_coding_schemes(
    'ISO_4217',
    'ISO 4217 Currency Code',
    'official',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/external/iso4217',
    'ISO 4217 currency codes. Three-letter alphabetic codes (e.g., USD, EUR, GBP) and three-digit numeric codes for currencies. The universal standard for currency identification in financial transactions.'
);

-- Currency identification schemes (industry - FPML extensions)
select ores.upsert_dq_coding_schemes(
    'FPML_NON_ISO_CURRENCY',
    'FpML Non-ISO Currency',
    'industry',
    'Currencies',
    'Reference Data',
    'http://www.fpml.org/coding-scheme/non-iso-currency',
    'FpML non-ISO currency codes. Expands the ISO 4217 currency list to include offshore currencies (CNH, CNT) and historical currencies (MCF, SML, VAL) not covered by the standard. Defined by FpML for derivatives trading.'
);

-- General schemes (internal - placeholder)
select ores.upsert_dq_coding_schemes(
    'NONE',
    'No Coding Scheme',
    'internal',
    'General',
    'Reference Data',
    null,
    'Placeholder for datasets that do not follow a formal coding scheme. Used when data uses ad-hoc or proprietary identifiers without a standardized format.'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'Coding Schemes' as entity, count(*) as count
from ores.dq_coding_schemes_tbl where valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
