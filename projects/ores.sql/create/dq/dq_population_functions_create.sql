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
 * production tables (target tenant). Each publish function uses SECURITY
 * DEFINER to bypass RLS and explicitly control tenant context.
 *
 * Usage pattern:
 *   1. List available datasets: SELECT * FROM ores_dq_datasets_list_publishable_fn();
 *   2. Execute the copy: SELECT * FROM ores_dq_*_publish_fn(dataset_id, target_tenant_id, mode);
 *
 * A preview function once sat between the two steps. It had no caller and
 * the artefact-type registry stopped dispatching to functions when its
 * populate_function column was replaced by target_subject, so the previews
 * were removed rather than kept.
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

