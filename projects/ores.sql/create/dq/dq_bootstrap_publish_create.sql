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
 * The function is the unit both bootstraps share: it resolves the dataset, its
 * artefact type and the publish function, and calls that function. A dataset
 * the system tenant does not hold publishes nothing, and a terminal artefact
 * type, whose rows are the entity's only store, publishes nothing either.
 *
 * The subject names the operation, and the operation names the function,
 * except for the datasets DQ publishes into itself: those are written by
 * <component>_<artefact type>_publish_fn. When neither name resolves the
 * function raises, because a member that publishes nowhere is a bundle that
 * lies about what it carries.
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

    if v_dataset_id is null or v_subject is null then
        return;
    end if;

    if position('v1.ops.' in v_subject) = 0 then
        raise exception 'Dataset "%" names subject "%", which names no operation.',
            p_dataset_code, v_subject;
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
 * Publishes the given bundles into one tenant, without the bus.
 *
 * The function orders the members by the dataset dependencies and publishes
 * each one into the target tenant. Both bootstraps in this file are callers of
 * it: the bring-up, which targets the system tenant, and the tenant bootstrap,
 * which targets the tenant being created.
 *
 * A member is published only when it has a dataset and an artefact type with a
 * target subject, and an optional member is skipped, because a bootstrap
 * publishes what every tenant of that level holds and an optional member is a
 * choice the holder makes later.
 *
 * Ordering by the declared dependencies is not enough on its own: a validator
 * may read a catalogue that no edge declares, as a convention's currency reads
 * the currency catalogue. A dataset that fails is therefore held back and
 * retried once every other member has published, which is enough for a
 * catalogue that another member writes. A dataset that fails twice is raised,
 * with its own error, so a bootstrap cannot end with a hole it does not name.
 *
 * A publish function compares its staged row with the row it holds, so a second
 * call reports what it found rather than what it wrote. That makes the
 * function safe to retry, and it is not the same promise as writing nothing.
 */
create or replace function ores_dq_publish_bundle_fn(
    p_target_tenant_id uuid,
    p_bundle_codes text[]
) returns table (
    dataset_code text,
    action text,
    record_count bigint
) as $$
declare
    v_target uuid := p_target_tenant_id;
    v_code text;
    v_action text;
    v_count bigint;
    v_missing text[] := '{}';
    v_failed text[] := '{}';
    v_order text[] := '{}';
begin
    if v_target is null then
        raise exception 'A bundle publish needs a target tenant, and none was given.';
    end if;

    if not exists (
        select 1 from ores_iam_tenants_tbl t
        where t.id = v_target
          and t.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Cannot publish into tenant %: no current tenant carries that id.', v_target;
    end if;

    if not exists (
        select 1 from ores_dq_dataset_bundle_members_tbl m
        where m.bundle_code = any(p_bundle_codes)
          and m.valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'A bundle publish was given no bundle that has members: %.',
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
        -- The dataset is the system tenant's, whatever the target: the staged
        -- artefacts and the row that describes them are held in one copy, and
        -- the target tenant only receives the published rows.
        if not exists (
            select 1 from ores_dq_datasets_tbl d
            where d.code = v_code
              and d.tenant_id = ores_utility_system_tenant_id_fn()
              and d.valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_missing := v_missing || v_code;
            continue;
        end if;

        v_order := v_order || v_code;

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

    -- The retry keeps the order the first pass walked, so a held-back dataset
    -- is retried after the datasets that precede it, not in the order an array
    -- of failures happens to hold.
    foreach v_code in array v_order
    loop
        if not (v_code = any(v_failed)) then
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
            raise exception 'Could not publish dataset "%" into tenant %: %',
                v_code, v_target, SQLERRM;
        end;
    end loop;

    if cardinality(v_missing) > 0 then
        raise warning 'Published nothing for % bundle member(s) with no dataset: %',
            cardinality(v_missing), array_to_string(v_missing, ', ');
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes the platform and world bundles into the system tenant.
 *
 * This is the bring-up caller: the populate scripts run it once a database has
 * a system tenant, and no bus is involved. It is the system tenant's own
 * publication, and every other tenant reads the rows it writes.
 */
create or replace function ores_dq_bootstrap_publish_fn(
    p_bundle_codes text[]
) returns table (
    dataset_code text,
    action text,
    record_count bigint
) as $$
begin
    return query
    select * from ores_dq_publish_bundle_fn(ores_utility_system_tenant_id_fn(), p_bundle_codes);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

/**
 * Publishes the bundles a newly created tenant is seeded with.
 *
 * The bundles are named here and nowhere else, so a tenant's seed rows are
 * stated in one place. A bundle belongs to this list when the bundle map places
 * it at the tenant level with tenant-wide scope and the publication happens
 * when the tenant is created; that is tenant_defaults, the badges, the badge
 * mappings and the code domains a tenant starts with. The rule that assigns the
 * level is stated in
 * doc/knowledge/architecture/reference_data_classification_by_publication_level.org.
 */
create or replace function ores_dq_tenant_bootstrap_fn(
    p_tenant_id uuid
) returns table (
    dataset_code text,
    action text,
    record_count bigint
) as $$
declare
    v_bundles constant text[] := array['tenant_defaults'];
begin
    return query select * from ores_dq_publish_bundle_fn(p_tenant_id, v_bundles);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
