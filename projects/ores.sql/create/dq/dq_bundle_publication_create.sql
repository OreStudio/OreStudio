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
 * Dataset Bundle Discovery Functions
 *
 * Read-only list and preview functions over the bundles and their members.
 * No table holds a publication history: a bundle is a run and not a
 * transaction, so a run leaves no audit row and the record of what it wrote is
 * the provenance on each published row. Actual publication is handled by the
 * workflow engine: the dq service dispatches each dataset to its target_subject
 * NATS endpoint, and each target service handles publish-from-dq in its own SQL
 * tree.
 *
 * Usage:
 *   -- List available bundles
 *   SELECT * FROM ores_dq_bundles_list_fn();
 *
 *   -- Preview what will be published
 *   SELECT * FROM ores_dq_bundle_preview_fn('base');
 */

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
    is_publishable boolean,
    optional boolean
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
        (d.id is not null and at.target_subject is not null)::boolean as is_publishable,
        m.optional
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
