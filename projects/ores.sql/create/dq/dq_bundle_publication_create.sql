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

/** The row type the roster returns, which a replace cannot change in place. */
drop function if exists ores_dq_bundles_list_fn();

/**
 * Lists all available dataset bundles with their level and member counts.
 *
 * The level is what decides which tenant holds the rows a publish writes, so a
 * caller that shows a bundle without its level is showing half the answer.
 */
create or replace function ores_dq_bundles_list_fn()
returns table (
    bundle_code text,
    bundle_name text,
    description text,
    level text,
    dataset_count bigint
) as $$
begin
    return query
    select
        b.code,
        b.name,
        b.description,
        b.level,
        count(m.dataset_code)::bigint
    from ores_dq_dataset_bundles_tbl b
    left join ores_dq_dataset_bundle_members_tbl m
        on m.bundle_code = b.code
        and m.valid_to = ores_utility_infinity_timestamp_fn()
    where b.valid_to = ores_utility_infinity_timestamp_fn()
    group by b.code, b.name, b.description, b.level
    order by b.code;
end;
$$ language plpgsql;

/**
 * Lists datasets in a bundle with their publication readiness status.
 */
drop function if exists ores_dq_bundle_datasets_list_fn(text);

/**
 * Lists datasets in a bundle with their publication readiness status.
 *
 * The staged count is the number of rows the member's own artefact table holds
 * for that dataset, which is what a person means by "is there anything to
 * publish?". It costs one count per member, so it is taken for the bundle a
 * caller names and never for the whole roster.
 */
create or replace function ores_dq_bundle_datasets_list_fn(p_bundle_code text)
returns table (
    display_order integer,
    dataset_id uuid,
    dataset_code text,
    dataset_name text,
    artefact_type text,
    artefact_table text,
    target_subject text,
    has_artefacts boolean,
    is_publishable boolean,
    optional boolean,
    staged_count bigint
) as $$
declare
    v_member record;
    v_staged bigint;
    v_table text;
    v_rows bigint;
begin
    for v_member in
        select
            m.display_order,
            d.id as dataset_id,
            m.dataset_code,
            coalesce(d.name, '(not found)')::text as dataset_name,
            coalesce(d.artefact_type, 'none')::text as artefact_type,
            at.artefact_table,
            coalesce(at.target_subject, '(none)')::text as target_subject,
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
        order by m.display_order
    loop
        v_staged := 0;
        -- An artefact type names the table or tables a dataset stages into, and
        -- names them without the product prefix the create scripts give every
        -- table. A family such as the scripts states several, comma separated,
        -- so the count sums them: a row is staged if it is in any of them.
        if v_member.dataset_id is not null and v_member.artefact_table is not null then
            foreach v_table in array string_to_array(v_member.artefact_table, ',') loop
                v_table := btrim(v_table);
                continue when v_table = '';
                execute format('select count(*) from %I where dataset_id = $1',
                               case when v_table like 'ores\_%'
                                    then v_table
                                    else 'ores_' || v_table end)
                    into v_rows
                    using v_member.dataset_id;
                v_staged := v_staged + v_rows;
            end loop;
        end if;

        display_order := v_member.display_order;
        dataset_id := v_member.dataset_id;
        dataset_code := v_member.dataset_code;
        dataset_name := v_member.dataset_name;
        artefact_type := v_member.artefact_type;
        artefact_table := v_member.artefact_table;
        target_subject := v_member.target_subject;
        has_artefacts := v_member.has_artefacts;
        is_publishable := v_member.is_publishable;
        optional := v_member.optional;
        staged_count := v_staged;
        return next;
    end loop;
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
