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
 * Publishes one dataset into one tenant, without the bus.
 *
 * The function is the bring-up caller's unit: it resolves the dataset, its
 * artefact type and the publish function, and calls that function. A dataset
 * the system tenant does not hold publishes nothing, and a terminal artefact
 * type, whose rows are the entity's only store, publishes nothing either.
 *
 * The subject names the operation, and the operation names the function,
 * except for the datasets DQ publishes into itself: those are written by
 * <component>_<artefact type>_publish_fn. Neither name resolving is an error,
 * because a member that publishes nowhere is a bundle that lies about what it
 * carries.
 */
create or replace function ores_dq_publish_dataset_fn(
    p_dataset_code text,
    p_target_tenant_id uuid
) returns table (
    action text,
    record_count bigint
) as $$
declare
    v_dataset_id uuid;
    v_subject text;
    v_type text;
    v_function text;
    v_signature text := '(uuid, uuid, text, jsonb)';
    v_action text;
    v_count bigint;
begin
    select d.id, at.target_subject, d.artefact_type
    into v_dataset_id, v_subject, v_type
    from ores_dq_datasets_tbl d
    join ores_dq_artefact_types_tbl at
      on at.code = d.artefact_type
     and at.tenant_id = ores_utility_system_tenant_id_fn()
     and at.valid_to = ores_utility_infinity_timestamp_fn()
    where d.code = p_dataset_code
      and d.tenant_id = ores_utility_system_tenant_id_fn()
      and d.valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null
       or v_subject is null
       or position('v1.ops.' in v_subject) = 0 then
        return;
    end if;

    v_function := 'ores_' || split_part(v_subject, '.', 1) || '_'
        || substr(v_subject, position('v1.ops.' in v_subject) + 7) || '_fn';

    if to_regprocedure(v_function || v_signature) is null then
        v_function := 'ores_' || split_part(v_subject, '.', 1) || '_'
            || v_type || '_publish_fn';
    end if;

    if to_regprocedure(v_function || v_signature) is null then
        raise exception 'No publish function for dataset "%", whose artefact type "%" names subject "%".',
            p_dataset_code, v_type, v_subject;
    end if;

    for v_action, v_count in execute format(
        'select action, record_count from %I(%L::uuid, %L::uuid, %L, %L::jsonb)',
        v_function, v_dataset_id, p_target_tenant_id, 'upsert', '{}')
    loop
        action := v_action;
        record_count := v_count;
        return next;
    end loop;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes the platform and world bundles into the system tenant.
 *
 * This is the bring-up caller, and the only one that runs without the bus. It
 * takes bundle codes, orders their members by the dataset dependencies, and
 * publishes each one into the system tenant.
 *
 * A member is published only when it has a dataset and an artefact type with a
 * target subject, and an optional member is skipped, because a bring-up
 * publishes what every installation holds and an optional member is a tenant's
 * choice.
 *
 * Ordering by the declared dependencies is not enough on its own: a validator
 * may read a catalogue that no edge declares, as a convention's currency reads
 * the currency catalogue. A dataset that fails is therefore held back and
 * retried once every other member has published, which is enough for a
 * catalogue that another member writes. A dataset that fails twice is raised,
 * with its own error, so a bring-up cannot end with a hole it does not name.
 *
 * The function is idempotent, as every publish function is, so a second call
 * writes nothing.
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
    v_action text;
    v_count bigint;
    v_missing text[] := '{}';
    v_failed text[] := '{}';
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
        if not exists (
            select 1 from ores_dq_datasets_tbl d
            where d.code = v_code
              and d.tenant_id = v_target
              and d.valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_missing := v_missing || v_code;
            continue;
        end if;

        begin
            for v_action, v_count in
                select * from ores_dq_publish_dataset_fn(v_code, v_target)
            loop
                dataset_code := v_code;
                action := v_action;
                record_count := v_count;
                return next;
            end loop;
        exception when others then
            v_failed := v_failed || v_code;
        end;
    end loop;

    for v_code in
        select unnest(v_failed)
    loop
        begin
            for v_action, v_count in
                select * from ores_dq_publish_dataset_fn(v_code, v_target)
            loop
                dataset_code := v_code;
                action := v_action;
                record_count := v_count;
                return next;
            end loop;
        exception when others then
            raise exception 'Bring-up could not publish dataset "%": %', v_code, SQLERRM;
        end;
    end loop;

    if cardinality(v_missing) > 0 then
        raise warning 'Bring-up published nothing for % bundle member(s) with no dataset: %',
            cardinality(v_missing), array_to_string(v_missing, ', ');
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
