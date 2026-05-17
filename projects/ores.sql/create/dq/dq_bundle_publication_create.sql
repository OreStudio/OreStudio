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
 * Audit table and read-only list/preview functions for bundle publications.
 * Actual publication is handled by the workflow engine: the dq service
 * dispatches each dataset to its target_subject NATS endpoint, and each
 * target service handles publish-from-dq in its own SQL tree.
 *
 * Usage:
 *   -- List available bundles
 *   SELECT * FROM ores_dq_bundles_list_fn();
 *
 *   -- Preview what will be published
 *   SELECT * FROM ores_dq_bundle_preview_fn('base');
 */

-- =============================================================================
-- Bundle Publication Audit Table
-- =============================================================================

create table if not exists "ores_dq_bundle_publications_tbl" (
    "id" uuid not null default gen_random_uuid(),
    "tenant_id" uuid not null,
    "bundle_code" text not null,
    "bundle_name" text not null,
    "mode" text not null,
    "atomic" boolean not null default false,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("bundle_code" <> ''),
    check ("mode" in ('upsert', 'insert_only', 'replace_all')),
    check ("published_by" <> '')
);

comment on table ores_dq_bundle_publications_tbl is
    'Audit table for tracking bundle publication history.';

-- Index for querying by bundle
create index if not exists bundle_publications_bundle_code_idx
    on ores_dq_bundle_publications_tbl(bundle_code);

-- Index for querying by time
create index if not exists bundle_publications_published_at_idx
    on ores_dq_bundle_publications_tbl(published_at);

-- =============================================================================
-- Discovery Functions
-- =============================================================================

/**
 * Lists all available dataset bundles with their member counts.
 */
create or replace function ores_dq_bundles_list_fn()
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
    from ores_dq_dataset_bundles_tbl b
    left join ores_dq_dataset_bundle_members_tbl m
        on m.bundle_code = b.code
        and m.valid_to = ores_utility_infinity_timestamp_fn()
    where b.valid_to = ores_utility_infinity_timestamp_fn()
    group by b.code, b.name, b.description
    order by b.code;
end;
$$ language plpgsql;

/**
 * Lists datasets in a bundle with their publication readiness status.
 */
create or replace function ores_dq_bundle_datasets_list_fn(p_bundle_code text)
returns table (
    display_order integer,
    dataset_id uuid,
    dataset_code text,
    dataset_name text,
    artefact_type text,
    target_subject text,
    has_artefacts boolean,
    is_publishable boolean
) as $$
begin
    return query
    select
        m.display_order,
        d.id as dataset_id,
        m.dataset_code,
        coalesce(d.name, '(not found)')::text,
        coalesce(d.artefact_type, 'none')::text,
        coalesce(at.target_subject, '(none)')::text,
        (d.id is not null)::boolean as has_artefacts,
        (d.id is not null and at.target_subject is not null)::boolean as is_publishable
    from ores_dq_dataset_bundle_members_tbl m
    left join ores_dq_datasets_tbl d
        on d.code = m.dataset_code
        and d.valid_to = ores_utility_infinity_timestamp_fn()
    left join ores_dq_artefact_types_tbl at
        on at.code = d.artefact_type
    where m.bundle_code = p_bundle_code
      and m.valid_to = ores_utility_infinity_timestamp_fn()
    order by m.display_order;
end;
$$ language plpgsql;

/**
 * Preview what would be published for a bundle.
 * Returns summary statistics without actually publishing.
 */
create or replace function ores_dq_bundle_preview_fn(p_bundle_code text)
returns table (
    metric text,
    value bigint
) as $$
begin
    -- Check bundle exists
    if not exists (
        select 1 from ores_dq_dataset_bundles_tbl
        where code = p_bundle_code
        and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Bundle not found: %', p_bundle_code;
    end if;

    -- Use CTE to compute all counts in a single pass
    return query
    with bundle_members as (
        select
            m.dataset_code,
            d.id as dataset_id,
            d.artefact_type,
            at.target_subject
        from ores_dq_dataset_bundle_members_tbl m
        left join ores_dq_datasets_tbl d
            on d.code = m.dataset_code
            and d.valid_to = ores_utility_infinity_timestamp_fn()
        left join ores_dq_artefact_types_tbl at
            on at.code = d.artefact_type
        where m.bundle_code = p_bundle_code
          and m.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select 'total_datasets'::text, count(*)::bigint from bundle_members
    union all
    select 'publishable_datasets', count(*)::bigint from bundle_members
        where dataset_id is not null and target_subject is not null
    union all
    select 'missing_datasets', count(*)::bigint from bundle_members
        where dataset_id is null
    union all
    select 'no_target_subject', count(*)::bigint from bundle_members
        where dataset_id is not null and (target_subject is null or artefact_type = 'none');
end;
$$ language plpgsql;

/**
 * Gets publication history for a specific bundle.
 */
create or replace function ores_dq_get_bundle_publication_history_fn(
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
    from ores_dq_bundle_publications_tbl bp
    where bp.bundle_code = p_bundle_code
    order by bp.published_at desc
    limit p_limit;
end;
$$ language plpgsql;
