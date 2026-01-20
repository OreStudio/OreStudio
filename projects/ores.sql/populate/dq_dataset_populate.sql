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

set schema 'ores';

-- =============================================================================
-- Seed Data
-- =============================================================================

select ores.upsert_dq_datasets(
    'iso.countries',
    'ISO Standards',
    'Countries',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Primary',
    'Actual',
    'Raw',
    'Wikipedia ISO 3166 Extraction',
    'ISO 3166 Country Codes',
    'ISO 3166-1 alpha-2 country codes and official names.',
    'WIKIPEDIA',
    'Reference data for country codes',
    current_date,
    'CC BY-SA 3.0'
);

select ores.upsert_dq_datasets(
    'assets.country_flags',
    'Visual Assets',
    'Country Flags',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Flag Icons Download',
    'Country Flag Images',
    'SVG flag images for each ISO 3166-1 country.',
    'GITHUB',
    'Visual assets for countries',
    '2025-12-20'::date,
    'MIT'
);

select ores.upsert_dq_tag(
    'Country Flag Images',
    'Country Flags',
    'Reference Data',
    'flag',
    'Country and region flag images'
);

select ores.upsert_dq_datasets(
    'assets.crypto_icons',
    'Visual Assets',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Cryptocurrency Icons Download',
    'Cryptocurrency Icon Images',
    'SVG icon images for major cryptocurrencies.',
    'GITHUB',
    'Visual assets for cryptocurrencies',
    '2025-01-15'::date,
    'CC0 1.0 Universal'
);

select ores.upsert_dq_tag(
    'Cryptocurrency Icon Images',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency icon images'
);

select ores.upsert_dq_datasets(
    'iso.currencies',
    'ISO Standards',
    'Currencies',
    'Reference Data',
    'ISO_4217',
    'Primary',
    'Actual',
    'Raw',
    'Wikipedia ISO 4217 Extraction',
    'ISO 4217 Currency Codes',
    'ISO 4217 alphabetic and numeric currency codes.',
    'WIKIPEDIA',
    'Reference data for currency codes',
    current_date,
    'CC BY-SA 3.0'
);

select ores.upsert_dq_tag(
    'ISO 4217 Currency Codes',
    'Currencies',
    'Reference Data',
    'currency',
    'Currency reference data'
);

select ores.upsert_dq_datasets(
    'fpml.currencies',
    'FpML Standards',
    'Currencies',
    'Reference Data',
    'FPML_NON_ISO_CURRENCY',
    'Primary',
    'Actual',
    'Raw',
    'FpML Genericode Download',
    'FpML Non-ISO Currency Codes',
    'Non-standard currency codes for derivatives (offshore CNH/CNT, historical MCF/SML/VAL).',
    'FPML',
    'Reference data for non-ISO currency codes used in derivatives trading',
    '2023-11-03'::date,
    'FpML Public License 2.0'
);

select ores.upsert_dq_tag(
    'FpML Non-ISO Currency Codes',
    'Currencies',
    'Reference Data',
    'currency',
    'Non-ISO currency reference data'
);

select ores.upsert_dq_datasets(
    'crypto.reference',
    'Cryptocurrency',
    'Cryptocurrencies',
    'Reference Data',
    'NONE',
    'Primary',
    'Actual',
    'Raw',
    'GitHub Cryptocurrencies JSON Download',
    'Cryptocurrency Reference Data',
    'Cryptocurrency symbols, names, and metadata.',
    'GITHUB',
    'Reference data for cryptocurrency codes',
    current_date,
    'MIT'
);

select ores.upsert_dq_tag(
    'Cryptocurrency Reference Data',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency reference data'
);

select ores.upsert_dq_datasets(
    'geo.ip2country',
    'IP Geolocation',
    'IP Address to Country maps',
    'Reference Data',
    'ISO_3166_1_ALPHA_2',
    'Primary',
    'Actual',
    'Raw',
    'iptoasn.com IP to Country Download',
    'IPv4 to Country Mapping',
    'IPv4 address ranges mapped to ISO 3166-1 alpha-2 country codes. Contains ~512k IP ranges covering the full IPv4 address space.',
    'IPTOASN',
    'Geographic IP lookup for IPv4 addresses',
    '2025-12-30'::date,
    'PDDL v1.0'
);

select ores.upsert_dq_tag(
    'IPv4 to Country Mapping',
    'IP Address to Country maps',
    'Reference Data',
    'geolocation',
    'IP address geolocation reference data'
);

-- =============================================================================
-- Summary
-- =============================================================================

select 'dq_datasets' as entity, count(*) as count
from ores.dq_datasets_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'dq_tags_artefact', count(*)
from ores.dq_tags_artefact_tbl;
