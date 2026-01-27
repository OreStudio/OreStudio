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
 * These functions copy data from DQ staging tables to production tables.
 * Designed to support a future UI for dataset management.
 *
 * Usage pattern:
 *   1. List available datasets: SELECT * FROM metadata.dq_datasets_list_publishable_fn();
 *   2. Preview what will be copied: SELECT * FROM metadata.dq_preview_*_population(dataset_id);
 *   3. Execute the copy: SELECT * FROM metadata.dq_populate_*(dataset_id, mode);
 *
 * Modes:
 *   - 'upsert': Insert new records, update existing (default)
 *   - 'insert_only': Only insert new records, skip existing
 *   - 'replace_all': Delete all existing, insert fresh (use with caution)
 */

set schema 'metadata';

-- =============================================================================
-- Discovery Function
-- =============================================================================

/**
 * Lists all DQ datasets that can be populated into production tables.
 * Returns dataset metadata and record counts for UI display.
 */
create or replace function metadata.dq_datasets_list_publishable_fn()
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
    from metadata.dq_datasets_tbl d
    join metadata.dq_images_artefact_tbl i on i.dataset_id = d.id
    where d.valid_to = public.utility_infinity_timestamp_fn()
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
    from metadata.dq_datasets_tbl d
    join metadata.dq_countries_artefact_tbl c on c.dataset_id = d.id
    where d.valid_to = public.utility_infinity_timestamp_fn()
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
    from metadata.dq_datasets_tbl d
    join metadata.dq_currencies_artefact_tbl c on c.dataset_id = d.id
    where d.valid_to = public.utility_infinity_timestamp_fn()
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
    from metadata.dq_datasets_tbl d
    join metadata.dq_ip2country_artefact_tbl ip on ip.dataset_id = d.id
    where d.valid_to = public.utility_infinity_timestamp_fn()
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
    from metadata.dq_datasets_tbl d
    join metadata.dq_coding_schemes_artefact_tbl cs on cs.dataset_id = d.id
    where d.valid_to = public.utility_infinity_timestamp_fn()
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
create or replace function metadata.dq_image_preview_fn(p_dataset_id uuid)
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
    from metadata.dq_images_artefact_tbl dq
    left join production.assets_images_tbl existing
        on existing.key = dq.key
        and existing.valid_to = public.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.key;
end;
$$ language plpgsql;

/**
 * Populate assets_images_tbl from a DQ images dataset.
 * Returns summary of actions taken.
 *
 * IMPORTANT: Images must be populated before countries/currencies
 * to ensure image_id references are valid.
 *
 * The trigger on assets_images_tbl automatically handles versioning:
 * - New records get version = 1
 * - Existing records are closed (valid_to set) and new version inserted
 *
 * For upsert mode with existing keys, we use the existing image_id to
 * preserve referential integrity with countries/currencies tables.
 */
create or replace function metadata.dq_images_publish_fn(
    p_dataset_id uuid,
    p_mode text default 'upsert'
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    r record;
    v_existing_image_id uuid;
    v_new_version integer;
begin
    -- Validate dataset exists
    select name into v_dataset_name
    from metadata.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode: delete all existing images first
    if p_mode = 'replace_all' then
        -- Soft delete by setting valid_to
        update production.assets_images_tbl
        set valid_to = current_timestamp
        where valid_to = public.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each image from DQ dataset
    for r in
        select
            dq.image_id,
            dq.key,
            dq.description,
            dq.svg_data
        from metadata.dq_images_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if an image with this key already exists
        select image_id into v_existing_image_id
        from production.assets_images_tbl existing
        where existing.key = r.key
          and existing.valid_to = public.utility_infinity_timestamp_fn();

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_existing_image_id is not null then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Insert image - trigger handles versioning automatically
        -- Use existing image_id if updating to preserve referential integrity,
        -- otherwise use the DQ image_id for new images
        insert into production.assets_images_tbl (
            image_id, version, key, description, svg_data,
            modified_by, change_reason_code, change_commentary
        ) values (
            coalesce(v_existing_image_id, r.image_id), 0, r.key, r.description, r.svg_data,
            'data_importer', 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        -- Track whether this was an insert (version=1) or update (version>1)
        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql;

-- =============================================================================
-- Countries Population Functions
-- =============================================================================

/**
 * Preview what countries would be copied from a DQ dataset.
 */
create or replace function metadata.dq_country_preview_fn(p_dataset_id uuid)
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
    from metadata.dq_countries_artefact_tbl dq
    left join production.refdata_countries_tbl existing
        on existing.alpha2_code = dq.alpha2_code
        and existing.valid_to = public.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.alpha2_code;
end;
$$ language plpgsql;

/**
 * Populate refdata_countries_tbl from a DQ countries dataset.
 *
 * IMPORTANT: Ensure images are populated first if you want image_id
 * references to resolve correctly.
 *
 * The trigger on refdata_countries_tbl automatically handles versioning:
 * - New records get version = 1
 * - Existing records are closed (valid_to set) and new version inserted
 *
 * The coding_scheme_code is copied from the dataset to track data provenance.
 */
create or replace function metadata.dq_countries_publish_fn(
    p_dataset_id uuid,
    p_mode text default 'upsert'
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_coding_scheme_code text;
    r record;
    v_resolved_image_id uuid;
    v_exists boolean;
    v_new_version integer;
begin
    -- Validate dataset exists and get metadata
    select name, coding_scheme_code into v_dataset_name, v_coding_scheme_code
    from metadata.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update production.refdata_countries_tbl
        set valid_to = current_timestamp
        where valid_to = public.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each country from DQ dataset
    for r in
        select
            dq.alpha2_code,
            dq.alpha3_code,
            dq.numeric_code,
            dq.name,
            dq.official_name,
            dq.image_id
        from metadata.dq_countries_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
    loop
        -- Check if record already exists (for mode handling and reporting)
        select exists (
            select 1 from production.refdata_countries_tbl existing
            where existing.alpha2_code = r.alpha2_code
              and existing.valid_to = public.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Resolve image_id: check if image exists in assets_images_tbl
        if r.image_id is not null then
            select image_id into v_resolved_image_id
            from production.assets_images_tbl
            where image_id = r.image_id
              and valid_to = public.utility_infinity_timestamp_fn();

            -- If not found by ID, image hasn't been populated yet
            if v_resolved_image_id is null then
                raise warning 'Image % not found in assets_images_tbl for country %. Populate images first.',
                    r.image_id, r.alpha2_code;
            end if;
        else
            v_resolved_image_id := null;
        end if;

        -- Insert country - trigger handles versioning automatically
        -- version=0 tells trigger to auto-increment from current version
        insert into production.refdata_countries_tbl (
            alpha2_code, version, alpha3_code, numeric_code, name, official_name,
            coding_scheme_code, image_id,
            modified_by, change_reason_code, change_commentary
        ) values (
            r.alpha2_code, 0, r.alpha3_code, r.numeric_code, r.name, r.official_name,
            v_coding_scheme_code, v_resolved_image_id,
            'data_importer', 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        -- Track whether this was an insert (version=1) or update (version>1)
        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql;

-- =============================================================================
-- Currencies Population Functions
-- =============================================================================

/**
 * Preview what currencies would be copied from a DQ dataset.
 */
create or replace function metadata.dq_currency_preview_fn(p_dataset_id uuid)
returns table (
    action text,
    iso_code text,
    currency_name text,
    currency_type text,
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
        dq.currency_type,
        dq.image_id is not null as has_image,
        case
            when existing.iso_code is not null then 'Currency already exists'
            else 'New currency'
        end as reason
    from metadata.dq_currencies_artefact_tbl dq
    left join production.refdata_currencies_tbl existing
        on existing.iso_code = dq.iso_code
        and existing.valid_to = public.utility_infinity_timestamp_fn()
    where dq.dataset_id = p_dataset_id
    order by dq.iso_code;
end;
$$ language plpgsql;

/**
 * Populate refdata_currencies_tbl from a DQ currencies dataset.
 *
 * @param p_dataset_id      The DQ dataset to populate from.
 * @param p_mode            Population mode: 'upsert', 'insert_only', or 'replace_all'.
 * @param p_currency_type_filter  Optional filter to only populate currencies with
 *                                matching currency_type (e.g., 'crypto.major').
 *                                If NULL, all currencies from the dataset are processed.
 *
 * IMPORTANT: Ensure images are populated first if you want image_id
 * references to resolve correctly.
 *
 * The trigger on refdata_currencies_tbl automatically handles versioning:
 * - New records get version = 1
 * - Existing records are closed (valid_to set) and new version inserted
 *
 * The coding_scheme_code is copied from the dataset to track data provenance.
 */
create or replace function metadata.dq_currencies_publish_fn(
    p_dataset_id uuid,
    p_mode text default 'upsert',
    p_currency_type_filter text default null
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_updated bigint := 0;
    v_skipped bigint := 0;
    v_deleted bigint := 0;
    v_dataset_name text;
    v_coding_scheme_code text;
    r record;
    v_resolved_image_id uuid;
    v_exists boolean;
    v_new_version integer;
begin
    -- Validate dataset exists and get metadata
    select name, coding_scheme_code into v_dataset_name, v_coding_scheme_code
    from metadata.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Handle replace_all mode
    if p_mode = 'replace_all' then
        update production.refdata_currencies_tbl
        set valid_to = current_timestamp
        where valid_to = public.utility_infinity_timestamp_fn();

        get diagnostics v_deleted = row_count;
    end if;

    -- Process each currency from DQ dataset
    -- Apply currency_type filter if specified
    for r in
        select
            dq.iso_code,
            dq.name,
            dq.numeric_code,
            dq.symbol,
            dq.fraction_symbol,
            dq.fractions_per_unit,
            dq.rounding_type,
            dq.rounding_precision,
            dq.format,
            dq.currency_type,
            dq.image_id
        from metadata.dq_currencies_artefact_tbl dq
        where dq.dataset_id = p_dataset_id
          and (p_currency_type_filter is null or dq.currency_type = p_currency_type_filter)
    loop
        -- Check if record already exists (for mode handling and reporting)
        select exists (
            select 1 from production.refdata_currencies_tbl existing
            where existing.iso_code = r.iso_code
              and existing.valid_to = public.utility_infinity_timestamp_fn()
        ) into v_exists;

        -- In insert_only mode, skip existing records
        if p_mode = 'insert_only' and v_exists then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        -- Resolve image_id
        if r.image_id is not null then
            select image_id into v_resolved_image_id
            from production.assets_images_tbl
            where image_id = r.image_id
              and valid_to = public.utility_infinity_timestamp_fn();

            if v_resolved_image_id is null then
                raise warning 'Image % not found in assets_images_tbl for currency %. Populate images first.',
                    r.image_id, r.iso_code;
            end if;
        else
            v_resolved_image_id := null;
        end if;

        -- Insert currency - trigger handles versioning automatically
        -- version=0 tells trigger to auto-increment from current version
        insert into production.refdata_currencies_tbl (
            iso_code, version, name, numeric_code, symbol, fraction_symbol,
            fractions_per_unit, rounding_type, rounding_precision, format, currency_type,
            coding_scheme_code, image_id,
            modified_by, change_reason_code, change_commentary
        ) values (
            r.iso_code, 0, r.name, r.numeric_code, r.symbol, r.fraction_symbol,
            r.fractions_per_unit, r.rounding_type, r.rounding_precision, r.format, r.currency_type,
            v_coding_scheme_code, v_resolved_image_id,
            'data_importer', 'system.external_data_import',
            'Imported from DQ dataset: ' || v_dataset_name
        )
        returning version into v_new_version;

        -- Track whether this was an insert (version=1) or update (version>1)
        if v_new_version = 1 then
            v_inserted := v_inserted + 1;
        else
            v_updated := v_updated + 1;
        end if;
    end loop;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'updated'::text, v_updated
    where v_updated > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql;

-- =============================================================================
-- IP to Country Population Functions
-- =============================================================================

/**
 * Preview what IP ranges would be copied from a DQ dataset.
 * Shows summary statistics rather than individual ranges (too many rows).
 */
create or replace function metadata.dq_preview_ip2country_population_fn(p_dataset_id uuid)
returns table (
    metric text,
    value bigint
) as $$
begin
    return query
    select 'total_ranges'::text, count(*)::bigint
    from metadata.dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id
    union all
    select 'unique_countries'::text, count(distinct country_code)::bigint
    from metadata.dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id
    union all
    select 'unrouted_ranges'::text, count(*)::bigint
    from metadata.dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id and country_code = 'None'
    union all
    select 'existing_ranges'::text, count(*)::bigint
    from production.geo_ip2country_tbl;
end;
$$ language plpgsql;

/**
 * Populate geo_ip2country_tbl from a DQ IP to Country dataset.
 *
 * NOTE: This function always uses 'replace_all' behavior due to the nature
 * of IP range data. The entire table is truncated and reloaded because:
 * 1. IP ranges change frequently (iptoasn.com updates hourly)
 * 2. Ranges are not individually identifiable (no unique key)
 * 3. Partial updates would leave stale/overlapping ranges
 *
 * @param p_dataset_id  The DQ dataset to populate from.
 * @param p_mode        Ignored - always performs replace_all.
 */
create or replace function metadata.dq_ip2country_publish_fn(
    p_dataset_id uuid,
    p_mode text default 'replace_all'
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
    from metadata.dq_datasets_tbl
    where id = p_dataset_id
      and valid_to = public.utility_infinity_timestamp_fn();

    if v_dataset_name is null then
        raise exception 'Dataset not found: %', p_dataset_id;
    end if;

    -- Always truncate existing data (IP ranges must be fully replaced)
    select count(*) into v_deleted from production.geo_ip2country_tbl;
    truncate table production.geo_ip2country_tbl;

    -- Insert from DQ artefact table, converting to int8range
    insert into production.geo_ip2country_tbl (ip_range, country_code)
    select
        int8range(range_start, range_end + 1, '[)'),
        country_code
    from metadata.dq_ip2country_artefact_tbl
    where dataset_id = p_dataset_id;

    get diagnostics v_inserted = row_count;

    -- Analyze table for query optimization
    analyze production.geo_ip2country_tbl;

    -- Return summary
    return query
    select 'inserted'::text, v_inserted
    union all select 'deleted'::text, v_deleted
    where v_deleted > 0;
end;
$$ language plpgsql;
