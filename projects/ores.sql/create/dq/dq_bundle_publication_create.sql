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
    has_dataset boolean,
    has_artefact_type boolean,
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
            d.name as dataset_name,
            d.artefact_type as artefact_type,
            at.artefact_table,
            at.target_subject as target_subject,
            (d.id is not null)::boolean as has_dataset,
            (d.id is not null and at.code is not null)::boolean as has_artefact_type,
            -- The same three facts the roster states, so one definition of ready
            -- answers the publish path and the screen that names the reason.
            (d.id is not null and at.code is not null and
             at.target_subject is not null)::boolean as is_publishable,
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
        has_dataset := v_member.has_dataset;
        has_artefact_type := v_member.has_artefact_type;
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

/**
 * Records one publication run.
 *
 * The run log is written by every target service, and none of them holds a grant
 * on the DQ tables it would need to read the dataset's code, its target table or
 * the person who asked. This function reads them once and appends the row, so the
 * caller passes what only it knows: what the run wrote and what it said.
 *
 * A run is recorded whatever its outcome, because a log of the successes alone
 * answers "when was this last published?" with an older run that did write
 * something.
 */
-- compass:except(entity_writes/sql_writer) expires=2027-11-27 approver=marco reason=the run log is appended by the step that publishes a dataset, and the generated repository would need a grant on tables the target service cannot read
create or replace function ores_dq_record_publication_fn(
    p_dataset_id uuid,
    p_instance_id uuid,
    p_mode text,
    p_inserted bigint,
    p_updated bigint,
    p_skipped bigint,
    p_deleted bigint,
    p_outcome text,
    p_messages jsonb
) returns void as $$
declare
    v_code text;
    v_target_table text;
    v_published_by text;
begin
    select d.code, coalesce(at.target_table, '')
      into v_code, v_target_table
      from ores_dq_datasets_tbl d
      left join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
     where d.id = p_dataset_id
       and d.valid_to = ores_utility_infinity_timestamp_fn();

    -- A dataset with no target table cannot publish, so a run that reached the
    -- log has one. An empty one here means the store changed under the run.
    if v_code is null or v_target_table = '' then
        return;
    end if;

    select coalesce(created_by, '')
      into v_published_by
      from ores_workflow_workflow_instances_tbl
     where id = p_instance_id;
    if v_published_by is null or v_published_by = '' then
        v_published_by := 'unknown';
    end if;

    insert into ores_dq_dataset_publications_tbl (
        tenant_id, dataset_id, dataset_code, mode, target_table,
        records_inserted, records_updated, records_skipped, records_deleted,
        published_by, outcome, messages)
    values (
        ores_iam_current_tenant_id_fn(), p_dataset_id, v_code, p_mode, v_target_table,
        p_inserted, p_updated, p_skipped, p_deleted,
        v_published_by, p_outcome, p_messages);
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

revoke execute on function ores_dq_record_publication_fn(
    uuid, uuid, text, bigint, bigint, bigint, bigint, text, jsonb) from public;

-- The step that publishes a dataset runs in the target's service, so every
-- service role records through this function. It takes the tenant from the
-- session and states only the run, so the grant opens no read across a tenant.
grant execute on function ores_dq_record_publication_fn(
    uuid, uuid, text, bigint, bigint, bigint, bigint, text, jsonb)
    to :iam_service_user,
       :refdata_service_user,
       :dq_service_user,
       :variability_service_user,
       :assets_service_user,
       :scheduler_service_user,
       :reporting_service_user,
       :telemetry_service_user,
       :trading_service_user,
       :compute_service_user,
       :synthetic_service_user,
       :workflow_service_user,
       :ore_service_user,
       :marketdata_service_user,
       :analytics_service_user,
       :storage_service_user,
       :inbox_service_user,
       :test_dml_user,
       :test_ddl_user;

/**
 * Lists the tables one dataset stages into, with each table's own columns and
 * the rows it holds for the dataset.
 *
 * A dataset names one artefact type and the type names a table or a family of
 * them, so the dataset itself does not say which tables hold its rows. The count
 * is per table and not the sum the roster states: a family's sum answers "how
 * much is staged" and never "which table is empty".
 */
create or replace function ores_dq_dataset_tables_fn(p_dataset_code text)
returns table (
    table_name text,
    display_order integer,
    columns text,
    staged_count bigint
) as $$
declare
    v_artefact_table text;
    v_dataset_id uuid;
    v_table text;
    v_rows bigint;
    v_columns text;
    v_n integer := 0;
begin
    -- The caller's own dataset row wins when a code is stated in more than one
    -- tenant: a tenant that holds a copy reads its copy, and everyone else reads
    -- the shared one in the system tenant. The read is invoker-rights, so the
    -- datasets read policy admits exactly those two rows.
    select at.artefact_table, d.id
      into v_artefact_table, v_dataset_id
      from ores_dq_datasets_tbl d
      join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
     where d.code = p_dataset_code
       and d.valid_to = ores_utility_infinity_timestamp_fn()
     order by (d.tenant_id = ores_iam_current_tenant_id_fn()) desc
     limit 1;

    if v_artefact_table is null then
        return;
    end if;

    foreach v_table in array string_to_array(v_artefact_table, ',') loop
        v_table := btrim(v_table);
        continue when v_table = '';
        if v_table not like 'ores\_%' then
            v_table := 'ores_' || v_table;
        end if;

        v_n := v_n + 1;
        v_rows := 0;
        -- A staged row belongs to the tenant that published it, and a shared row
        -- to the system tenant. Both are readable; another tenant's copy is not.
        execute format(
            'select count(*) from %I where dataset_id = $1'
            '  and tenant_id in (ores_iam_current_tenant_id_fn(),'
            '                    ores_utility_system_tenant_id_fn())', v_table)
            into v_rows using v_dataset_id;

        select string_agg(c.column_name, ', ' order by c.ordinal_position)
          into v_columns
          from information_schema.columns c
         where c.table_schema = 'public' and c.table_name = v_table
           and c.column_name not in ('dataset_id', 'tenant_id', 'version');

        table_name := v_table;
        display_order := v_n;
        columns := coalesce(v_columns, '');
        staged_count := v_rows;
        return next;
    end loop;
end;
$$ language plpgsql set search_path = public, pg_temp;

/**
 * One page of the rows a dataset stages into one of its own tables.
 *
 * The columns come with the rows, because every artefact family carries its own
 * and no screen knows them. The table is checked against the dataset's own
 * artefact type rather than trusted: the caller names a table it was shown, and
 * this function refuses any other.
 *
 * A dataset that names no table, or a table the dataset does not stage into, is
 * refused with an exception rather than an error object: the answer is either a
 * page or a refusal, and a caller that has to inspect the answer to learn which
 * one it got is a caller that will get it wrong.
 */
create or replace function ores_dq_dataset_rows_fn(
    p_dataset_code text,
    p_table_name text,
    p_limit integer default 15,
    p_offset integer default 0
) returns jsonb as $$
declare
    v_artefact_table text;
    v_dataset_id uuid;
    v_allowed boolean := false;
    v_table text;
    v_columns jsonb;
    v_rows jsonb;
    v_total bigint;
begin
    select at.artefact_table, d.id
      into v_artefact_table, v_dataset_id
      from ores_dq_datasets_tbl d
      join ores_dq_artefact_types_tbl at on at.code = d.artefact_type
     where d.code = p_dataset_code
       and d.valid_to = ores_utility_infinity_timestamp_fn()
     order by (d.tenant_id = ores_iam_current_tenant_id_fn()) desc
     limit 1;

    if v_artefact_table is null then
        raise exception 'No dataset called % is readable here', coalesce(p_dataset_code, '');
    end if;

    foreach v_table in array string_to_array(v_artefact_table, ',') loop
        v_table := btrim(v_table);
        if v_table not like 'ores\_%' then
            v_table := 'ores_' || v_table;
        end if;
        if v_table = p_table_name then
            v_allowed := true;
            exit;
        end if;
    end loop;

    if not v_allowed then
        raise exception 'This dataset stages into no table called %', coalesce(p_table_name, '');
    end if;

    select coalesce(jsonb_agg(jsonb_build_object(
               'name', c.column_name,
               'type', c.data_type) order by c.ordinal_position), '[]'::jsonb)
      into v_columns
      from information_schema.columns c
     where c.table_schema = 'public' and c.table_name = p_table_name
       and c.column_name not in ('dataset_id', 'tenant_id', 'version');

    execute format(
        'select count(*) from %I where dataset_id = $1'
        '  and tenant_id in (ores_iam_current_tenant_id_fn(),'
        '                    ores_utility_system_tenant_id_fn())', p_table_name)
        into v_total using v_dataset_id;

    -- The columns every family shares are dropped from the rows too: they say
    -- which dataset, tenant and version wrote a row, and the request states the
    -- first two while nobody reads a version here.
    --
    -- ctid is the order because an artefact table has no key a reader can name,
    -- and a page needs a total order: without one, LIMIT and OFFSET may repeat a
    -- row on one page and skip it on the next.
    execute format(
        'select coalesce(jsonb_agg(to_jsonb(t) - ''dataset_id'' - ''tenant_id'' - ''version''), '
        '                ''[]''::jsonb) '
        'from ('
        '  select * from %I where dataset_id = $1'
        '    and tenant_id in (ores_iam_current_tenant_id_fn(),'
        '                      ores_utility_system_tenant_id_fn())'
        '  order by ctid limit $2 offset $3'
        ') t', p_table_name)
        into v_rows using v_dataset_id, greatest(1, least(coalesce(p_limit, 15), 1000)),
                           greatest(0, coalesce(p_offset, 0));

    return jsonb_build_object('columns', v_columns, 'rows', v_rows, 'total', v_total);
end;
$$ language plpgsql set search_path = public, pg_temp;
