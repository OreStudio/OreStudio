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
 * Data Quality Population Functions
 *
 * These functions copy data from DQ staging tables (system tenant) to
 * production tables (target tenant). Each function uses SECURITY DEFINER
 * to bypass RLS and explicitly control tenant context.
 *
 * Usage pattern:
 *   1. List available datasets: SELECT * FROM ores_dq_datasets_list_publishable_fn();
 *   2. Preview what will be copied: SELECT * FROM ores_dq_*_preview_fn(dataset_id);
 *   3. Execute the copy: SELECT * FROM ores_dq_*_publish_fn(dataset_id, target_tenant_id, mode);
 *
 * Modes:
 *   - 'upsert': Insert new records, update existing (default)
 *   - 'insert_only': Only insert new records, skip existing
 *   - 'replace_all': Delete all existing, insert fresh (use with caution)
 */

-- =============================================================================
-- Discovery Function
-- =============================================================================

/**
 * Lists all DQ datasets that can be populated into production tables.
 * Returns dataset metadata and record counts for UI display.
 */
create or replace function ores_dq_datasets_list_publishable_fn()
returns table (
    dataset_id uuid,
    dataset_name text,
    subject_area_name text,
    domain_name text,
    artefact_type text,
    record_count bigint,
    source_system_id text,
    as_of_date date,
    license_info text
) as $$
begin
    return query
    -- Images datasets
    select
        d.id,
        d.name,
        d.subject_area_name,
        d.domain_name,
        'images'::text as artefact_type,
        count(i.image_id)::bigint as record_count,
        d.source_system_id,
        d.as_of_date,
        d.license_info
    from ores_dq_datasets_tbl d
    join ores_dq_images_artefact_tbl i on i.dataset_id = d.id
    where d.valid_to = ores_utility_infinity_timestamp_fn()
    group by d.id, d.name, d.subject_area_name, d.domain_name,
             d.source_system_id, d.as_of_date, d.license_info

    union all

    -- Countries datasets
    select
        d.id,
        d.name,
        d.subject_area_name,
        d.domain_name,
        'countries'::text as artefact_type,
        count(c.alpha2_code)::bigint as record_count,
        d.source_system_id,
        d.as_of_date,
        d.license_info
    from ores_dq_datasets_tbl d
    join ores_dq_countries_artefact_tbl c on c.dataset_id = d.id
    where d.valid_to = ores_utility_infinity_timestamp_fn()
    group by d.id, d.name, d.subject_area_name, d.domain_name,
             d.source_system_id, d.as_of_date, d.license_info

    union all

    -- Currencies datasets
    select
        d.id,
        d.name,
        d.subject_area_name,
        d.domain_name,
        'currencies'::text as artefact_type,
        count(c.iso_code)::bigint as record_count,
        d.source_system_id,
        d.as_of_date,
        d.license_info
    from ores_dq_datasets_tbl d
    join ores_dq_currencies_artefact_tbl c on c.dataset_id = d.id
    where d.valid_to = ores_utility_infinity_timestamp_fn()
    group by d.id, d.name, d.subject_area_name, d.domain_name,
             d.source_system_id, d.as_of_date, d.license_info

    union all

    -- IP to Country datasets
    select
        d.id,
        d.name,
        d.subject_area_name,
        d.domain_name,
        'ip2country'::text as artefact_type,
        count(ip.range_start)::bigint as record_count,
        d.source_system_id,
        d.as_of_date,
        d.license_info
    from ores_dq_datasets_tbl d
    join ores_dq_ip2country_artefact_tbl ip on ip.dataset_id = d.id
    where d.valid_to = ores_utility_infinity_timestamp_fn()
    group by d.id, d.name, d.subject_area_name, d.domain_name,
             d.source_system_id, d.as_of_date, d.license_info

    union all

    -- Coding Schemes datasets
    select
        d.id,
        d.name,
        d.subject_area_name,
        d.domain_name,
        'coding_schemes'::text as artefact_type,
        count(cs.code)::bigint as record_count,
        d.source_system_id,
        d.as_of_date,
        d.license_info
    from ores_dq_datasets_tbl d
    join ores_dq_coding_schemes_artefact_tbl cs on cs.dataset_id = d.id
    where d.valid_to = ores_utility_infinity_timestamp_fn()
    group by d.id, d.name, d.subject_area_name, d.domain_name,
             d.source_system_id, d.as_of_date, d.license_info

    order by artefact_type, dataset_name;
end;
$$ language plpgsql;

-- =============================================================================
-- Images Population Functions
-- =============================================================================

/**
 * Preview what images would be copied from a DQ dataset.
 * Shows the action that would be taken for each record.
 */
create or replace function ores_dq_image_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    image_key text,
    description text,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.image_id is not null then 'update'
            else 'insert'
        end as action,
        dq.key as image_key,
        dq.description,
        case
            when existing.image_id is not null then 'Image with this key already exists'
            else 'New image'
        end as reason
    from ores_dq_images_artefact_tbl dq
    left join ores_assets_images_tbl existing
        on existing.key = dq.key
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.key;
end;
$$ language plpgsql;

-- =============================================================================
-- Countries Population Functions
-- =============================================================================

/**
 * Preview what countries would be copied from a DQ dataset.
 */
create or replace function ores_dq_country_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    alpha2_code text,
    country_name text,
    has_image boolean,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.alpha2_code is not null then 'update'
            else 'insert'
        end as action,
        dq.alpha2_code,
        dq.name as country_name,
        dq.image_id is not null as has_image,
        case
            when existing.alpha2_code is not null then 'Country already exists'
            else 'New country'
        end as reason
    from ores_dq_countries_artefact_tbl dq
    left join ores_refdata_countries_tbl existing
        on existing.alpha2_code = dq.alpha2_code
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.alpha2_code;
end;
$$ language plpgsql;

-- =============================================================================
-- Currencies Population Functions
-- =============================================================================

/**
 * Preview what currencies would be copied from a DQ dataset.
 */
create or replace function ores_dq_currency_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    iso_code text,
    currency_name text,
    monetary_nature text,
    market_tier text,
    has_image boolean,
    reason text
) as $$
begin
    return query
    select
        case
            when existing.iso_code is not null then 'update'
            else 'insert'
        end as action,
        dq.iso_code,
        dq.name as currency_name,
        dq.monetary_nature,
        dq.market_tier,
        dq.image_id is not null as has_image,
        case
            when existing.iso_code is not null then 'Currency already exists'
            else 'New currency'
        end as reason
    from ores_dq_currencies_artefact_tbl dq
    left join ores_refdata_currencies_tbl existing
        on existing.iso_code = dq.iso_code
        and existing.valid_to = ores_utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.iso_code;
end;
$$ language plpgsql;

-- =============================================================================
-- IP to Country Population Functions
-- =============================================================================

/**
 * Preview what IP ranges would be copied from a DQ dataset.
 * Shows summary statistics rather than individual ranges (too many rows).
 */
create or replace function ores_dq_preview_ip2country_population_fn(p_dataset_id uuid)
returns table (
    metric text,
    value bigint
) as $$
begin
    return query
    select 'total_ranges'::text, count(*)::bigint
    from ores_dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id
    union all
    select 'unique_countries'::text, count(distinct country_code)::bigint
    from ores_dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id
    union all
    select 'unrouted_ranges'::text, count(*)::bigint
    from ores_dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id and country_code = 'None'
    union all
    select 'existing_ranges'::text, count(*)::bigint
    from ores_geo_ip2country_tbl;
end;
$$ language plpgsql;

/**
 * Populate geo_ip2country_tbl from a DQ IP to Country dataset.
 *
 * This function uses SECURITY DEFINER to bypass RLS. It reads artefacts from
 * the system tenant and writes to the target tenant specified by p_target_tenant_id.
 *
 * NOTE: This function always uses 'replace_all' behavior due to the nature
 * of IP range data. The entire table is truncated and reloaded because:
 * 1. IP ranges change frequently (iptoasn.com updates hourly)
 * 2. Ranges are not individually identifiable (no unique key)
 * 3. Partial updates would leave stale/overlapping ranges
 *
 * @param p_dataset_id       The DQ dataset to populate from
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             Ignored - always performs replace_all
 */
create or replace function ores_dq_ip2country_publish_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'replace_all',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
begin
    -- Validate dataset exists
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Always truncate existing data (IP ranges must be fully replaced)
    select count(*) into v_deleted from ores_geo_ip2country_tbl;
    truncate table ores_geo_ip2country_tbl;

    -- Insert from DQ artefact table (read from system tenant), converting to int8range
    insert into ores_geo_ip2country_tbl (tenant_id, ip_range, country_code)
    select
        p_target_tenant_id,
        int8range(range_start, range_end + 1, '[)'),
        country_code
    from ores_dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id
      and tenant_id = ores_utility_system_tenant_id_fn();

    get diagnostics v_inserted = row_count;

    -- Analyze table for query optimization
    analyze ores_geo_ip2country_tbl;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql security definer;

/**
 * Publishes coding schemes from DQ artefact table to production.
 * Writes to ores_dq_coding_schemes_tbl (DQ's own table — no SECURITY DEFINER needed).
 *
 * @param p_dataset_id       Dataset to publish from
 * @param p_target_tenant_id The tenant to publish data to
 * @param p_mode             'replace_all' truncates and reloads; 'upsert' merges
 * @param p_params           Unused; reserved for future filtering
 */
create or replace function ores_dq_coding_schemes_publish_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated  bigint := 0;
    v_deleted  bigint := 0;
    v_now      timestamp with time zone := now();
    v_inf      timestamp with time zone := ores_utility_infinity_timestamp_fn();
    v_dataset_name text;
begin
    select name into v_dataset_name
    from ores_dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = v_inf;

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Expire existing active records (both modes)
    update ores_dq_coding_schemes_tbl
    set valid_to = v_now
    where tenant_id = p_target_tenant_id
      and valid_to = v_inf;
    get diagnostics v_deleted = row_count;

    -- Insert new records from artefact table
    insert into ores_dq_coding_schemes_tbl (
        code, tenant_id, version, name, authority_type,
        subject_area_name, domain_name, uri, description,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    select
        a.code, p_target_tenant_id, a.version, a.name, a.authority_type,
        a.subject_area_name, a.domain_name, a.uri, a.description,
        'system', 'system', 'INITIAL_LOAD', 'Published from DQ artefact',
        v_now, v_inf
    from ores_dq_coding_schemes_artefact_tbl a
    where a.dataset_id = p_dataset_id;
    get diagnostics v_inserted = row_count;

    return query
    select 'inserted'::text, v_inserted
    union all select 'updated'::text, v_updated  where v_updated  > 0
    union all select 'deleted'::text, v_deleted  where v_deleted  > 0;
end;
$$ language plpgsql;
