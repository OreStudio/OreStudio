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
select ores.upsert_dq_tag(
    'Country Flag Images',
    'Country Flags',
    'Reference Data',
    'flag',
    'Country and region flag images'
);

select ores.upsert_dq_tag(
    'Cryptocurrency Icon Images',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency icon images'
);

select ores.upsert_dq_tag(
    'ISO 4217 Currency Codes',
    'Currencies',
    'Reference Data',
    'currency',
    'Currency reference data'
);

select ores.upsert_dq_tag(
    'ISO 3166 Country Codes',
    'Countries',
    'Reference Data',
    'country',
    'Country reference data'
);

select ores.upsert_dq_tag(
    'Country Flag Images',
    'Country Flags',
    'Reference Data',
    'country',
    'Country reference data'
);

select ores.upsert_dq_tag(
    'Cryptocurrencies Top 12243 Coins',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Cryptocurrency reference data for top 12243 coins'
);

select ores.upsert_dq_tag(
    'Cryptocurrencies Top 100 Coins',
    'Cryptocurrencies',
    'Reference Data',
    'cryptocurrency',
    'Top 100 cryptocurrencies by market cap'
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

\echo ''
\echo '--- Summary ---'

select 'dq_datasets' as entity, count(*) as count
from ores.dq_datasets_tbl
where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'dq_tags_artefact', count(*)
from ores.dq_tags_artefact_tbl;
