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
 * DQ Production Population Script
 *
 * Populates production tables from DQ staging datasets using the
 * population functions. This replaces the legacy direct population scripts.
 *
 * Order of population:
 * 1. Images (flags and crypto icons) - must be first for FK references
 * 2. Countries (references flag images)
 * 3. Currencies - fiat (references flag images)
 * 4. Currencies - crypto (references crypto icons)
 *
 * Usage:
 *   psql -U ores -d your_database -f populate/dq_populate_production.sql
 */

set schema 'ores';

DO $$
declare
    v_flags_dataset_id uuid;
    v_crypto_icons_dataset_id uuid;
    v_countries_dataset_id uuid;
    v_currencies_dataset_id uuid;
    v_cryptocurrencies_dataset_id uuid;
    v_result record;
    v_total_inserted bigint := 0;
    v_total_updated bigint := 0;
begin
    raise notice '=== Populating Production Tables from DQ Datasets ===';
    raise notice '';

    -- ==========================================================================
    -- Step 1: Get dataset IDs
    -- ==========================================================================

    -- Country flags dataset
    select id into v_flags_dataset_id
    from ores.dq_dataset_tbl
    where name = 'Country Flags from lipis/flag-icons'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flags from lipis/flag-icons';
    end if;

    -- Cryptocurrency icons dataset
    select id into v_crypto_icons_dataset_id
    from ores.dq_dataset_tbl
    where name = 'Cryptocurrency Icons from spothq/cryptocurrency-icons'
      and subject_area_name = 'Cryptocurrencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_crypto_icons_dataset_id is null then
        raise exception 'Dataset not found: Cryptocurrency Icons from spothq/cryptocurrency-icons';
    end if;

    -- Countries dataset
    select id into v_countries_dataset_id
    from ores.dq_dataset_tbl
    where name = 'ISO 3166 Countries from Wikipedia'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_countries_dataset_id is null then
        raise exception 'Dataset not found: ISO 3166 Countries from Wikipedia';
    end if;

    -- Fiat currencies dataset
    select id into v_currencies_dataset_id
    from ores.dq_dataset_tbl
    where name = 'ISO 4217 Currencies from Wikipedia'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_currencies_dataset_id is null then
        raise exception 'Dataset not found: ISO 4217 Currencies from Wikipedia';
    end if;

    -- Cryptocurrencies dataset
    select id into v_cryptocurrencies_dataset_id
    from ores.dq_dataset_tbl
    where name = 'Cryptocurrencies from crypti/cryptocurrencies'
      and subject_area_name = 'Cryptocurrencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_cryptocurrencies_dataset_id is null then
        raise exception 'Dataset not found: Cryptocurrencies from crypti/cryptocurrencies';
    end if;

    raise notice 'All datasets found successfully';
    raise notice '';

    -- ==========================================================================
    -- Step 2: Populate Images (must be first for FK references)
    -- ==========================================================================

    raise notice '--- Populating Flag Images ---';
    for v_result in select * from ores.dq_populate_images(v_flags_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    raise notice '';
    raise notice '--- Populating Cryptocurrency Icons ---';
    for v_result in select * from ores.dq_populate_images(v_crypto_icons_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    -- ==========================================================================
    -- Step 3: Populate Countries (references flag images)
    -- ==========================================================================

    raise notice '';
    raise notice '--- Populating Countries ---';
    for v_result in select * from ores.dq_populate_countries(v_countries_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    -- ==========================================================================
    -- Step 4: Populate Currencies (fiat with flags, crypto with icons)
    -- ==========================================================================

    raise notice '';
    raise notice '--- Populating Fiat Currencies ---';
    for v_result in select * from ores.dq_populate_currencies(v_currencies_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    raise notice '';
    raise notice '--- Populating Cryptocurrencies ---';
    for v_result in select * from ores.dq_populate_currencies(v_cryptocurrencies_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    -- ==========================================================================
    -- Summary
    -- ==========================================================================

    raise notice '';
    raise notice '=== Production Population Complete ===';
    raise notice 'Total inserted: %', v_total_inserted;
    raise notice 'Total updated: %', v_total_updated;
end $$;

-- Summary query
\echo ''
\echo '--- Production Tables Summary ---'

\pset tuples_only off

select 'Images' as entity, count(*) as count
from ores.assets_images_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Countries', count(*)
from ores.refdata_countries_tbl where valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Countries with Images', count(*)
from ores.refdata_countries_tbl where image_id is not null and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (Fiat)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (Crypto)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'crypto' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies with Images', count(*)
from ores.refdata_currencies_tbl where image_id is not null and valid_to = ores.utility_infinity_timestamp_fn()
order by entity;
