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
    'CC BY-SA 3.0',
    'countries',
    'refdata_countries_tbl',
    'dq_populate_countries'
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
    'CC BY-SA 3.0',
    'currencies',
    'refdata_currencies_tbl',
    'dq_populate_currencies'
);

-- =============================================================================
-- Summary
-- =============================================================================

\echo ''
\echo '--- Summary ---'

select 'dq_datasets' as entity, count(*) as count
from ores.dq_datasets_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'dq_tags_artefact', count(*)
from ores.dq_tags_artefact_tbl;
