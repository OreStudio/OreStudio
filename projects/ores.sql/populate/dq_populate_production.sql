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
    v_fpml_currencies_dataset_id uuid;
    v_cryptocurrencies_dataset_id uuid;
    v_ip2country_dataset_id uuid;
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
    from ores.dq_datasets_tbl
    where name = 'Country Flag Images'
      and subject_area_name = 'Country Flags'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_flags_dataset_id is null then
        raise exception 'Dataset not found: Country Flag Images';
    end if;

    -- Cryptocurrency icons dataset
    select id into v_crypto_icons_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Cryptocurrency Icon Images'
      and subject_area_name = 'Cryptocurrencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_crypto_icons_dataset_id is null then
        raise exception 'Dataset not found: Cryptocurrency Icon Images';
    end if;

    -- Countries dataset
    select id into v_countries_dataset_id
    from ores.dq_datasets_tbl
    where name = 'ISO 3166 Country Codes'
      and subject_area_name = 'Countries'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_countries_dataset_id is null then
        raise exception 'Dataset not found: ISO 3166 Country Codes';
    end if;

    -- Fiat currencies dataset
    select id into v_currencies_dataset_id
    from ores.dq_datasets_tbl
    where name = 'ISO 4217 Currency Codes'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_currencies_dataset_id is null then
        raise exception 'Dataset not found: ISO 4217 Currency Codes';
    end if;

    -- FpML non-ISO currencies dataset
    select id into v_fpml_currencies_dataset_id
    from ores.dq_datasets_tbl
    where name = 'FpML Non-ISO Currency Codes'
      and subject_area_name = 'Currencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_fpml_currencies_dataset_id is null then
        raise exception 'Dataset not found: FpML Non-ISO Currency Codes';
    end if;

    -- Cryptocurrencies dataset (all coins)
    select id into v_cryptocurrencies_dataset_id
    from ores.dq_datasets_tbl
    where name = 'Cryptocurrencies Top 12243 Coins'
      and subject_area_name = 'Cryptocurrencies'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_cryptocurrencies_dataset_id is null then
        raise exception 'Dataset not found: Cryptocurrencies Top 12243 Coins';
    end if;

    -- IP to Country dataset
    select id into v_ip2country_dataset_id
    from ores.dq_datasets_tbl
    where name = 'IPv4 to Country Mapping'
      and subject_area_name = 'IP Address to Country maps'
      and domain_name = 'Reference Data'
      and valid_to = ores.utility_infinity_timestamp_fn();

    if v_ip2country_dataset_id is null then
        raise exception 'Dataset not found: IPv4 to Country Mapping';
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
    raise notice '--- Populating Fiat Currencies (ISO 4217) ---';
    for v_result in select * from ores.dq_populate_currencies(v_currencies_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    raise notice '';
    raise notice '--- Populating Non-ISO Currencies (FpML) ---';
    for v_result in select * from ores.dq_populate_currencies(v_fpml_currencies_dataset_id, 'upsert') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    raise notice '';
    raise notice '--- Populating Major Cryptocurrencies (crypto.major only) ---';
    for v_result in select * from ores.dq_populate_currencies(v_cryptocurrencies_dataset_id, 'upsert', 'crypto.major') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
        if v_result.action = 'updated' then v_total_updated := v_total_updated + v_result.record_count; end if;
    end loop;

    -- ==========================================================================
    -- Step 5: Populate IP to Country (bulk replace)
    -- ==========================================================================

    raise notice '';
    raise notice '--- Populating IP to Country Mapping ---';
    for v_result in select * from ores.dq_populate_ip2country(v_ip2country_dataset_id, 'replace_all') loop
        raise notice '  %: %', v_result.action, v_result.record_count;
        if v_result.action = 'inserted' then v_total_inserted := v_total_inserted + v_result.record_count; end if;
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
select 'Currencies (fiat.major)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.major' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.emerging)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.emerging' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (commodity)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'commodity' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (supranational)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'supranational' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.offshore)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.offshore' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (fiat.historical)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'fiat.historical' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies (crypto.major)', count(*)
from ores.refdata_currencies_tbl where currency_type = 'crypto.major' and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'Currencies with Images', count(*)
from ores.refdata_currencies_tbl where image_id is not null and valid_to = ores.utility_infinity_timestamp_fn()
union all
select 'IP to Country Ranges', count(*)
from ores.geo_ip2country_tbl
order by entity;
