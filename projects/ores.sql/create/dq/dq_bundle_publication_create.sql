/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
 * Dataset Bundle Publication Functions
 *
 * Provides functionality to publish all datasets in a bundle to production.
 * The bundle is processed in display_order to respect dependencies.
 *
 * Usage:
 *   -- List available bundles
 *   SELECT * FROM ores.dq_list_bundles_fn();
 *
 *   -- Preview what will be published
 *   SELECT * FROM ores.dq_preview_bundle_publication_fn('base');
 *
 *   -- Publish a bundle
 *   SELECT * FROM ores.dq_populate_bundle_fn('base', 'upsert', 'admin');
 */

set schema 'ores';

-- =============================================================================
-- Bundle Publication Audit Table
-- =============================================================================

create table if not exists "ores"."dq_bundle_publications_tbl" (
    "id" uuid not null default gen_random_uuid(),
    "bundle_code" text not null,
    "bundle_name" text not null,
    "mode" text not null,
    "datasets_processed" integer not null default 0,
    "datasets_succeeded" integer not null default 0,
    "datasets_failed" integer not null default 0,
    "datasets_skipped" integer not null default 0,
    "total_records_inserted" bigint not null default 0,
    "total_records_updated" bigint not null default 0,
    "total_records_skipped" bigint not null default 0,
    "total_records_deleted" bigint not null default 0,
    "published_by" text not null,
    "published_at" timestamp with time zone not null default current_timestamp,
    "completed_at" timestamp with time zone,
    primary key (id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("bundle_code" <> ''),
    check ("mode" in ('upsert', 'insert_only', 'replace_all')),
    check ("published_by" <> '')
);

comment on table ores.dq_bundle_publications_tbl is
    'Audit table for tracking bundle publication history.';

-- Index for querying by bundle
create index if not exists dq_bundle_publications_bundle_code_idx
    on ores.dq_bundle_publications_tbl(bundle_code);

-- Index for querying by time
create index if not exists dq_bundle_publications_published_at_idx
    on ores.dq_bundle_publications_tbl(published_at);

-- =============================================================================
-- Discovery Functions
-- =============================================================================

/**
 * Lists all available dataset bundles with their member counts.
 */
create or replace function ores.dq_list_bundles_fn()
returns table (
    bundle_code text,
    bundle_name text,
    description text,
    dataset_count bigint
) as $$
begin
    return query
    select
        b.code,
        b.name,
        b.description,
        count(m.dataset_code)::bigint
    from ores.dq_dataset_bundles_tbl b
    left join ores.dq_dataset_bundle_members_tbl m
        on m.bundle_code = b.code
        and m.valid_to = ores.utility_infinity_timestamp_fn()
    where b.valid_to = ores.utility_infinity_timestamp_fn()
    group by b.code, b.name, b.description
    order by b.code;
end;
$$ language plpgsql;

/**
 * Lists datasets in a bundle with their publication readiness status.
 */
create or replace function ores.dq_list_bundle_datasets_fn(p_bundle_code text)
returns table (
    display_order integer,
    dataset_code text,
    dataset_name text,
    artefact_type text,
    populate_function text,
    has_artefacts boolean,
    is_publishable boolean
) as $$
begin
    return query
    select
        m.display_order,
        m.dataset_code,
        coalesce(d.name, '(not found)')::text,
        coalesce(d.artefact_type, 'none')::text,
        coalesce(at.populate_function, '(none)')::text,
        (d.id is not null)::boolean as has_artefacts,
        (d.id is not null and at.populate_function is not null)::boolean as is_publishable
    from ores.dq_dataset_bundle_members_tbl m
    left join ores.dq_datasets_tbl d
        on d.code = m.dataset_code
        and d.valid_to = ores.utility_infinity_timestamp_fn()
    left join ores.dq_artefact_types_tbl at
        on at.code = d.artefact_type
    where m.bundle_code = p_bundle_code
      and m.valid_to = ores.utility_infinity_timestamp_fn()
    order by m.display_order;
end;
$$ language plpgsql;

/**
 * Preview what would be published for a bundle.
 * Returns summary statistics without actually publishing.
 */
create or replace function ores.dq_preview_bundle_publication_fn(p_bundle_code text)
returns table (
    metric text,
    value bigint
) as $$
declare
    v_bundle_exists boolean;
    v_total_datasets bigint;
    v_publishable_datasets bigint;
    v_missing_datasets bigint;
    v_no_function_datasets bigint;
begin
    -- Check bundle exists
    select exists (
        select 1 from ores.dq_dataset_bundles_tbl
        where code = p_bundle_code
        and valid_to = ores.utility_infinity_timestamp_fn()
    ) into v_bundle_exists;

    if not v_bundle_exists then
        raise exception 'Bundle not found: %', p_bundle_code;
    end if;

    -- Count datasets
    select count(*) into v_total_datasets
    from ores.dq_dataset_bundle_members_tbl
    where bundle_code = p_bundle_code
    and valid_to = ores.utility_infinity_timestamp_fn();

    -- Count publishable datasets
    select count(*) into v_publishable_datasets
    from ores.dq_dataset_bundle_members_tbl m
    join ores.dq_datasets_tbl d on d.code = m.dataset_code
        and d.valid_to = ores.utility_infinity_timestamp_fn()
    join ores.dq_artefact_types_tbl at on at.code = d.artefact_type
    where m.bundle_code = p_bundle_code
    and m.valid_to = ores.utility_infinity_timestamp_fn()
    and at.populate_function is not null;

    -- Count missing datasets (in bundle but not in dq_datasets_tbl)
    select count(*) into v_missing_datasets
    from ores.dq_dataset_bundle_members_tbl m
    left join ores.dq_datasets_tbl d on d.code = m.dataset_code
        and d.valid_to = ores.utility_infinity_timestamp_fn()
    where m.bundle_code = p_bundle_code
    and m.valid_to = ores.utility_infinity_timestamp_fn()
    and d.id is null;

    -- Count datasets without populate function
    select count(*) into v_no_function_datasets
    from ores.dq_dataset_bundle_members_tbl m
    join ores.dq_datasets_tbl d on d.code = m.dataset_code
        and d.valid_to = ores.utility_infinity_timestamp_fn()
    left join ores.dq_artefact_types_tbl at on at.code = d.artefact_type
    where m.bundle_code = p_bundle_code
    and m.valid_to = ores.utility_infinity_timestamp_fn()
    and (at.populate_function is null or d.artefact_type = 'none');

    return query
    select 'total_datasets'::text, v_total_datasets
    union all
    select 'publishable_datasets', v_publishable_datasets
    union all
    select 'missing_datasets', v_missing_datasets
    union all
    select 'no_populate_function', v_no_function_datasets;
end;
$$ language plpgsql;

-- =============================================================================
-- Bundle Publication Function
-- =============================================================================

/**
 * Publishes all datasets in a bundle to production tables.
 *
 * Processes datasets in display_order to respect dependencies.
 * Each dataset is published individually, and failures are logged but don't
 * stop the overall bundle publication.
 *
 * @param p_bundle_code  The bundle to publish (e.g., 'base', 'solvaris', 'crypto')
 * @param p_mode         Publication mode: 'upsert', 'insert_only', or 'replace_all'
 * @param p_published_by User/system publishing the bundle
 *
 * @returns Summary of publication results per dataset
 */
create or replace function ores.dq_populate_bundle_fn(
    p_bundle_code text,
    p_mode text default 'upsert',
    p_published_by text default current_user
)
returns table (
    dataset_code text,
    dataset_name text,
    status text,
    records_inserted bigint,
    records_updated bigint,
    records_skipped bigint,
    records_deleted bigint,
    error_message text
) as $$
declare
    v_bundle_id uuid;
    v_bundle_name text;
    v_bundle_pub_id uuid;
    v_dataset record;
    v_artefact_type record;
    v_populate_result record;
    v_result_row record;
    v_sql text;
    v_datasets_processed integer := 0;
    v_datasets_succeeded integer := 0;
    v_datasets_failed integer := 0;
    v_datasets_skipped integer := 0;
    v_total_inserted bigint := 0;
    v_total_updated bigint := 0;
    v_total_skipped bigint := 0;
    v_total_deleted bigint := 0;
    v_row_inserted bigint;
    v_row_updated bigint;
    v_row_skipped bigint;
    v_row_deleted bigint;
    v_error_msg text;
begin
    -- Validate mode
    if p_mode not in ('upsert', 'insert_only', 'replace_all') then
        raise exception 'Invalid mode: %. Use upsert, insert_only, or replace_all', p_mode;
    end if;

    -- Validate bundle exists
    select id, name into v_bundle_id, v_bundle_name
    from ores.dq_dataset_bundles_tbl
    where code = p_bundle_code
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_bundle_id is null then
        raise exception 'Bundle not found: %', p_bundle_code;
    end if;

    -- Create bundle publication audit record
    insert into ores.dq_bundle_publications_tbl (
        bundle_code, bundle_name, mode, published_by
    ) values (
        p_bundle_code, v_bundle_name, p_mode, p_published_by
    ) returning id into v_bundle_pub_id;

    raise notice 'Starting bundle publication: % (%)', p_bundle_code, v_bundle_name;

    -- Process each dataset in display_order
    for v_dataset in
        select
            m.dataset_code,
            m.display_order,
            d.id as dataset_id,
            d.name as dataset_name,
            d.artefact_type
        from ores.dq_dataset_bundle_members_tbl m
        left join ores.dq_datasets_tbl d
            on d.code = m.dataset_code
            and d.valid_to = ores.utility_infinity_timestamp_fn()
        where m.bundle_code = p_bundle_code
        and m.valid_to = ores.utility_infinity_timestamp_fn()
        order by m.display_order
    loop
        v_datasets_processed := v_datasets_processed + 1;
        v_row_inserted := 0;
        v_row_updated := 0;
        v_row_skipped := 0;
        v_row_deleted := 0;
        v_error_msg := null;

        -- Check if dataset exists
        if v_dataset.dataset_id is null then
            v_datasets_skipped := v_datasets_skipped + 1;
            dataset_code := v_dataset.dataset_code;
            dataset_name := '(not found)';
            status := 'skipped';
            records_inserted := 0;
            records_updated := 0;
            records_skipped := 0;
            records_deleted := 0;
            error_message := 'Dataset not found in dq_datasets_tbl';
            return next;
            continue;
        end if;

        -- Check if dataset has no artefact type or 'none'
        if v_dataset.artefact_type is null or v_dataset.artefact_type = 'none' then
            v_datasets_skipped := v_datasets_skipped + 1;
            dataset_code := v_dataset.dataset_code;
            dataset_name := v_dataset.dataset_name;
            status := 'skipped';
            records_inserted := 0;
            records_updated := 0;
            records_skipped := 0;
            records_deleted := 0;
            error_message := 'Dataset has no artefact type (metadata only)';
            return next;
            continue;
        end if;

        -- Look up the artefact type
        select * into v_artefact_type
        from ores.dq_artefact_types_tbl
        where code = v_dataset.artefact_type;

        if v_artefact_type is null then
            v_datasets_failed := v_datasets_failed + 1;
            dataset_code := v_dataset.dataset_code;
            dataset_name := v_dataset.dataset_name;
            status := 'failed';
            records_inserted := 0;
            records_updated := 0;
            records_skipped := 0;
            records_deleted := 0;
            error_message := 'Unknown artefact_type: ' || v_dataset.artefact_type;
            return next;
            continue;
        end if;

        -- Check if populate function exists
        if v_artefact_type.populate_function is null then
            v_datasets_skipped := v_datasets_skipped + 1;
            dataset_code := v_dataset.dataset_code;
            dataset_name := v_dataset.dataset_name;
            status := 'skipped';
            records_inserted := 0;
            records_updated := 0;
            records_skipped := 0;
            records_deleted := 0;
            error_message := 'No populate_function for artefact_type: ' || v_dataset.artefact_type;
            return next;
            continue;
        end if;

        -- Call the populate function dynamically
        begin
            v_sql := format(
                'SELECT * FROM ores.%I(%L::uuid, %L::text)',
                v_artefact_type.populate_function,
                v_dataset.dataset_id,
                p_mode
            );

            raise notice 'Publishing dataset: % using %', v_dataset.dataset_code, v_artefact_type.populate_function;

            -- Execute and aggregate results
            for v_result_row in execute v_sql
            loop
                case v_result_row.action
                    when 'inserted' then v_row_inserted := v_result_row.record_count;
                    when 'updated' then v_row_updated := v_result_row.record_count;
                    when 'skipped' then v_row_skipped := v_result_row.record_count;
                    when 'deleted' then v_row_deleted := v_result_row.record_count;
                end case;
            end loop;

            -- Record individual dataset publication
            insert into ores.dq_publications_tbl (
                dataset_id, dataset_code, mode, target_table,
                records_inserted, records_updated, records_skipped, records_deleted,
                published_by
            ) values (
                v_dataset.dataset_id, v_dataset.dataset_code, p_mode,
                coalesce(v_artefact_type.target_table, 'unknown'),
                v_row_inserted, v_row_updated, v_row_skipped, v_row_deleted,
                p_published_by
            );

            v_datasets_succeeded := v_datasets_succeeded + 1;
            v_total_inserted := v_total_inserted + v_row_inserted;
            v_total_updated := v_total_updated + v_row_updated;
            v_total_skipped := v_total_skipped + v_row_skipped;
            v_total_deleted := v_total_deleted + v_row_deleted;

            dataset_code := v_dataset.dataset_code;
            dataset_name := v_dataset.dataset_name;
            status := 'success';
            records_inserted := v_row_inserted;
            records_updated := v_row_updated;
            records_skipped := v_row_skipped;
            records_deleted := v_row_deleted;
            error_message := null;
            return next;

        exception when others then
            v_datasets_failed := v_datasets_failed + 1;
            v_error_msg := SQLERRM;

            dataset_code := v_dataset.dataset_code;
            dataset_name := v_dataset.dataset_name;
            status := 'failed';
            records_inserted := 0;
            records_updated := 0;
            records_skipped := 0;
            records_deleted := 0;
            error_message := v_error_msg;
            return next;

            raise warning 'Failed to publish dataset %: %', v_dataset.dataset_code, v_error_msg;
        end;
    end loop;

    -- Update bundle publication audit record
    update ores.dq_bundle_publications_tbl
    set datasets_processed = v_datasets_processed,
        datasets_succeeded = v_datasets_succeeded,
        datasets_failed = v_datasets_failed,
        datasets_skipped = v_datasets_skipped,
        total_records_inserted = v_total_inserted,
        total_records_updated = v_total_updated,
        total_records_skipped = v_total_skipped,
        total_records_deleted = v_total_deleted,
        completed_at = current_timestamp
    where id = v_bundle_pub_id;

    raise notice 'Bundle publication complete: % datasets processed, % succeeded, % failed, % skipped',
        v_datasets_processed, v_datasets_succeeded, v_datasets_failed, v_datasets_skipped;
end;
$$ language plpgsql;

/**
 * Gets publication history for a specific bundle.
 */
create or replace function ores.dq_get_bundle_publication_history_fn(
    p_bundle_code text,
    p_limit integer default 10
)
returns table (
    publication_id uuid,
    bundle_code text,
    bundle_name text,
    mode text,
    datasets_processed integer,
    datasets_succeeded integer,
    datasets_failed integer,
    total_inserted bigint,
    total_updated bigint,
    published_by text,
    published_at timestamp with time zone,
    completed_at timestamp with time zone
) as $$
begin
    return query
    select
        bp.id,
        bp.bundle_code,
        bp.bundle_name,
        bp.mode,
        bp.datasets_processed,
        bp.datasets_succeeded,
        bp.datasets_failed,
        bp.total_records_inserted,
        bp.total_records_updated,
        bp.published_by,
        bp.published_at,
        bp.completed_at
    from ores.dq_bundle_publications_tbl bp
    where bp.bundle_code = p_bundle_code
    order by bp.published_at desc
    limit p_limit;
end;
$$ language plpgsql;
