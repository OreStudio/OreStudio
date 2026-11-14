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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
/**
 * Publishes the platform and world bundles into the system tenant.
 *
 * This is the bring-up caller of the publish functions, and the only one that
 * runs without the bus. It takes bundle codes, orders their members by the
 * dataset dependencies, and calls each member's publish function with the
 * system tenant as the target.
 *
 * A member is published only when it has a dataset in the system tenant and an
 * artefact type with a target subject. A terminal artefact type, whose rows are
 * the entity's only store, has no subject and is skipped; an optional member is
 * skipped, because a bring-up publishes what every installation holds and an
 * optional member is a tenant's choice.
 *
 * The function is idempotent, as every publish function is, so a second call
 * writes nothing. It fails loudly when a member names a function that does not
 * exist, and warns when a member has no dataset, which is the state a seeding
 * script's typo leaves behind.
 */
create or replace function ores_dq_bootstrap_publish_fn(
    p_bundle_codes text[]
) returns table (
    dataset_code text,
    action text,
    record_count bigint
) as $$
declare
    v_target uuid := ores_utility_system_tenant_id_fn();
    v_code text;
    v_dataset_id uuid;
    v_subject text;
    v_type text;
    v_function text;
    v_signature text := '(uuid, uuid, text, jsonb)';
    v_action text;
    v_count bigint;
    v_missing text[] := '{}';
begin
    if v_target is null then
        raise exception 'The system tenant is not defined, so bring-up has no target to publish into.';
    end if;

    if not exists (
        select 1 from ores_dq_dataset_bundle_members_tbl m
        where m.bundle_code = any(p_bundle_codes)
          and m.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Bring-up was given no bundle that has members: %.',
            coalesce(array_to_string(p_bundle_codes, ', '), '(none)');
    end if;

    for v_code in
        with recursive members(dataset_code) as (
            select distinct m.dataset_code
            from ores_dq_dataset_bundle_members_tbl m
            where m.bundle_code = any(p_bundle_codes)
              and m.valid_to = ores_utility_infinity_timestamp_fn()
              and not m.optional
        ),
        edges(dataset_code, dependency_code) as (
            select d.dataset_code, d.dependency_code
            from ores_dq_dataset_dependencies_tbl d
            where d.valid_to = ores_utility_infinity_timestamp_fn()
              and d.dataset_code in (select members.dataset_code from members)
              and d.dependency_code in (select members.dataset_code from members)
        ),
        walk(dataset_code, depth, path) as (
            select m.dataset_code, 0, array[m.dataset_code]
            from members m
            where not exists (
                select 1 from edges e where e.dataset_code = m.dataset_code)
            union all
            select e.dataset_code, w.depth + 1, w.path || e.dataset_code
            from edges e
            join walk w on w.dataset_code = e.dependency_code
            where not (e.dataset_code = any(w.path))
        ),
        ordered as (
            select w.dataset_code, max(w.depth) as depth
            from walk w
            group by w.dataset_code
        )
        select m.dataset_code
        from members m
        left join ordered o on o.dataset_code = m.dataset_code
        order by coalesce(o.depth, 9999), m.dataset_code
    loop
        select d.id, at.target_subject, d.artefact_type
        into v_dataset_id, v_subject, v_type
        from ores_dq_datasets_tbl d
        join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
        where d.code = v_code
          and d.tenant_id = v_target
          and d.valid_to = ores_utility_infinity_timestamp_fn();

        if v_dataset_id is null then
            v_missing := v_missing || v_code;
            continue;
        end if;

        if v_subject is null or position('v1.ops.' in v_subject) = 0 then
            continue;
        end if;

        -- The subject names the operation, and the operation names the
        -- function, except for the datasets DQ publishes into itself: those
        -- are written by <component>_<artefact type>_publish_fn.
        v_function := 'ores_' || split_part(v_subject, '.', 1) || '_'
            || substr(v_subject, position('v1.ops.' in v_subject) + 7) || '_fn';

        if to_regprocedure(v_function || v_signature) is null then
            v_function := 'ores_' || split_part(v_subject, '.', 1) || '_'
                || v_type || '_publish_fn';
        end if;

        if to_regprocedure(v_function || v_signature) is null then
            raise exception 'Bring-up found no publish function for dataset "%", whose artefact type "%" names subject "%".',
                v_code, v_type, v_subject;
        end if;

        for v_action, v_count in execute format(
            'select action, record_count from %I(%L::uuid, %L::uuid, %L, %L::jsonb)',
            v_function, v_dataset_id, v_target, 'upsert', '{}')
        loop
            dataset_code := v_code;
            action := v_action;
            record_count := v_count;
            return next;
        end loop;
    end loop;

    if cardinality(v_missing) > 0 then
        raise warning 'Bring-up published nothing for % bundle member(s) with no dataset: %',
            cardinality(v_missing), array_to_string(v_missing, ', ');
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
