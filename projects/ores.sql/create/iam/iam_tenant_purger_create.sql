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

-- =============================================================================
-- Tenant Purge Functions
-- =============================================================================
-- These functions permanently delete tenant data (hard delete).
-- Use with caution - data cannot be recovered after purging.

-- -----------------------------------------------------------------------------
-- Purge a single tenant (hard delete all data)
-- -----------------------------------------------------------------------------
-- Permanently deletes all data for a tenant including the tenant record itself.
-- The system tenant cannot be purged.
-- Caller must have system tenant context set.
--
-- Parameters:
--   p_tenant_id: The tenant ID to purge
--
create or replace function ores_iam_purge_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
    v_deleted_count bigint := 0;
    v_row_count bigint;
    v_pending text[];
    v_blocked text[];
    v_table text;
    v_rule text;
    v_rules text[];
    v_column_type text;
    v_pass integer;
begin
    v_system_tenant_id := ores_utility_system_tenant_id_fn();

    -- Cannot purge system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot purge the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to purge a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging (check both active and terminated tenants)
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where id = p_tenant_id
    limit 1;

    if not found then
        raise exception 'Tenant not found: %', p_tenant_id using errcode = '23503';
    end if;

    raise notice 'Purging tenant: % (id: %)', v_tenant_code, p_tenant_id;

    -- =========================================================================
    -- Hard delete all tenant data
    -- =========================================================================
    -- Every table that carries a tenant_id is evidence of a tenant, so the
    -- sweep takes its list from the catalogue rather than from a list here: a
    -- table that gains the column is purged without this function changing,
    -- and a table that is forgotten cannot quietly keep its rows.
    --
    -- Two things make a plain delete wrong. A delete rule closes the row it
    -- matches instead of removing it, so each table's rules are disabled
    -- around its own delete and always put back. And a component references
    -- its anchor with an enforced foreign key, so a table whose rows are still
    -- referenced is retried in a later pass once its referrers are gone.
    --
    -- An anchor is immutable, so the purge turns on the purge signal for this
    -- transaction first: it is the one sanctioned way past the anchor's guard.
    perform ores_utility_allow_immutable_purge_fn();
    select coalesce(array_agg(t.tablename::text order by t.tablename), '{}')
    into v_pending
    from pg_tables t
    where t.schemaname = 'public'
      and exists (
          select 1 from information_schema.columns c
          where c.table_schema = t.schemaname
            and c.table_name = t.tablename
            and c.column_name = 'tenant_id');

    for v_pass in 1 .. cardinality(v_pending) loop
        exit when cardinality(v_pending) = 0;
        v_blocked := '{}';
        foreach v_table in array v_pending loop
            select coalesce(array_agg(r.rulename::text), '{}') into v_rules
            from pg_rewrite r
            join pg_class c on c.oid = r.ev_class
            join pg_namespace n on n.oid = c.relnamespace
            where n.nspname = 'public'
              and c.relname = v_table
              and r.ev_type = '4';
            begin
                foreach v_rule in array v_rules loop
                    execute format('alter table %I disable rule %I', v_table, v_rule);
                end loop;
                -- The comparator follows the column's own type: a uuid
                -- comparison reaches the tenant index, and only the few tables
                -- that record an operator's tenant as the string they
                -- presented are compared as text.
                select c.data_type into v_column_type
                from information_schema.columns c
                where c.table_schema = 'public'
                  and c.table_name = v_table
                  and c.column_name = 'tenant_id';
                if v_column_type = 'uuid' then
                    execute format('delete from %I where tenant_id = $1', v_table)
                        using p_tenant_id;
                else
                    execute format('delete from %I where tenant_id = $1', v_table)
                        using p_tenant_id::text;
                end if;
                get diagnostics v_row_count = row_count;
                v_deleted_count := v_deleted_count + v_row_count;
                foreach v_rule in array v_rules loop
                    execute format('alter table %I enable rule %I', v_table, v_rule);
                end loop;
            exception when foreign_key_violation then
                -- The block's own rollback puts the disabled rules back, so the
                -- table is retried with the rules it started with.
                v_blocked := v_blocked || v_table;
            end;
        end loop;
        -- A pass that frees nothing ends the sweep: every table left is blocked
        -- by a row this tenant does not own, and repeating it changes nothing.
        exit when cardinality(v_blocked) = cardinality(v_pending);
        v_pending := v_blocked;
    end loop;
    perform set_config('ores.utility.allow_immutable_purge', 'off', true);
    if cardinality(v_pending) > 0 then
        raise exception 'Could not purge the tenant rows of %: they are still referenced.',
            v_pending using errcode = '23503';
    end if;

    raise notice 'Purged % row(s) of tenant % (id: %)',
        v_deleted_count, v_tenant_code, p_tenant_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- The sweep disables a table's rules and so runs as the owner, which bypasses
-- the row-level policies. PostgreSQL grants execute to PUBLIC by default, so it
-- is revoked and the application's read-write role is granted it by name.
revoke execute on function ores_iam_purge_tenant_fn(uuid) from public;
grant execute on function ores_iam_purge_tenant_fn(uuid) to :rw_role;

-- -----------------------------------------------------------------------------
-- Hard delete tenant records (bypasses soft-delete rule)
-- -----------------------------------------------------------------------------
-- Called after ores_iam_purge_tenant_fn to remove the tenant record itself.
-- Must be called separately since we need to disable/enable the delete rule.
--
create or replace function ores_iam_hard_delete_tenants_fn(
    p_tenant_ids uuid[]
) returns integer as $$
declare
    v_count integer;
begin
    -- The system tenant is never removable, and this path bypasses both the
    -- soft-delete rule and the table check, so it carries the guard itself. It
    -- also carries the context guard its caller carries: the function runs as
    -- the owner, and only a session acting on the system tenant may ask for it.
    if ores_iam_current_tenant_id_fn() != ores_utility_system_tenant_id_fn() then
        raise exception 'Must be in system tenant context to hard delete tenants. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    if ores_utility_system_tenant_id_fn() = any(p_tenant_ids) then
        raise exception 'Cannot hard delete the system tenant'
            using errcode = '42501';
    end if;

    -- Disable the soft-delete rule
    alter table ores_iam_tenants_tbl disable rule ores_iam_tenants_delete_rule;

    -- Hard delete all specified tenants (by their id, not tenant_id which is always system)
    delete from ores_iam_tenants_tbl where id = any(p_tenant_ids);
    get diagnostics v_count = row_count;

    -- Re-enable the soft-delete rule
    alter table ores_iam_tenants_tbl enable rule ores_iam_tenants_delete_rule;

    return v_count;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

revoke execute on function ores_iam_hard_delete_tenants_fn(uuid[]) from public;
grant execute on function ores_iam_hard_delete_tenants_fn(uuid[]) to :rw_role;

-- -----------------------------------------------------------------------------
-- Purge all test tenants
-- -----------------------------------------------------------------------------
-- Permanently deletes all tenants whose code starts with 'ores.' (test tenants).
-- This is useful for cleaning up after test runs.
--
-- Returns the number of tenants purged.
--
create or replace function ores_iam_purge_test_tenants_fn()
returns integer as $$
declare
    v_tenant record;
    v_tenant_ids uuid[];
    v_count integer := 0;
begin
    -- Ensure we're in system tenant context
    perform set_config('app.current_tenant_id',
        ores_utility_system_tenant_id_fn()::text, false);

    raise notice 'Purging all test tenants...';

    -- First collect all test tenant IDs and purge their data
    -- Matches: ores.* (new format) and test_* (old format)
    for v_tenant in
        select distinct id, code
        from ores_iam_tenants_tbl
        where code like 'ores.%' or code like 'test_%'
        order by code
    loop
        perform ores_iam_purge_tenant_fn(v_tenant.id);
        v_tenant_ids := array_append(v_tenant_ids, v_tenant.id);
        v_count := v_count + 1;
    end loop;

    -- Now hard-delete the tenant records (requires disabling the soft-delete rule)
    if v_count > 0 then
        perform ores_iam_hard_delete_tenants_fn(v_tenant_ids);
    end if;

    raise notice 'Purged % test tenants', v_count;
    return v_count;
end;
$$ language plpgsql;
