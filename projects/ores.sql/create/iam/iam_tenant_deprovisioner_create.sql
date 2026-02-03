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
-- Tenant Deprovisioning Functions
-- =============================================================================
-- These functions handle tenant termination and cleanup.
-- Data is soft-deleted (valid_to set to current timestamp) to preserve history.

-- -----------------------------------------------------------------------------
-- Deprovision a tenant (soft delete)
-- -----------------------------------------------------------------------------
-- Terminates a tenant by:
--   1. Updating tenant status to 'terminated'
--   2. Soft-deleting all tenant data (sets valid_to to current timestamp)
--
-- The system tenant cannot be deprovisioned.
-- Caller must have system tenant context set.
--
-- Uses dynamic SQL to automatically discover all tenant-aware tables:
--   - Tables with tenant_id + valid_to: soft-delete (UPDATE valid_to)
--   - Tables with tenant_id only: hard-delete (DELETE)
--
-- Parameters:
--   p_tenant_id: The tenant ID to deprovision
--
create or replace function ores_iam_deprovision_tenant_fn(
    p_tenant_id uuid
) returns void as $$
declare
    v_system_tenant_id uuid;
    v_tenant_code text;
    v_table_name text;
    v_affected_count integer;
    v_total_soft_deleted integer := 0;
    v_total_hard_deleted integer := 0;
    v_sql text;
begin
    v_system_tenant_id := ores_iam_system_tenant_id_fn();

    -- Cannot deprovision system tenant
    if p_tenant_id = v_system_tenant_id then
        raise exception 'Cannot deprovision the system tenant' using errcode = '42501';
    end if;

    -- Verify we're in system tenant context
    if ores_iam_current_tenant_id_fn() != v_system_tenant_id then
        raise exception 'Must be in system tenant context to deprovision a tenant. Current tenant: %',
            ores_iam_current_tenant_id_fn() using errcode = '42501';
    end if;

    -- Get tenant code for logging
    select code into v_tenant_code
    from ores_iam_tenants_tbl
    where id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise exception 'Tenant not found or already terminated: %', p_tenant_id
            using errcode = '23503';
    end if;

    raise notice 'Deprovisioning tenant: % (id: %)', v_tenant_code, p_tenant_id;

    -- =========================================================================
    -- Handle special cases first
    -- =========================================================================

    -- Sessions: end active sessions (uses end_time, not valid_to)
    update ores_iam_sessions_tbl
    set end_time = to_char(current_timestamp, 'YYYY-MM-DD HH24:MI:SS.US')
    where tenant_id = p_tenant_id
    and end_time = '';

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Ended % active sessions', v_affected_count;
    end if;

    -- Login info: hard delete (non-temporal table)
    delete from ores_iam_login_info_tbl where tenant_id = p_tenant_id;

    get diagnostics v_affected_count = row_count;
    if v_affected_count > 0 then
        raise notice 'Deleted % login info records', v_affected_count;
    end if;

    -- =========================================================================
    -- Dynamically soft-delete all temporal tables with tenant_id + valid_to
    -- =========================================================================
    for v_table_name in
        select distinct c1.table_name
        from information_schema.columns c1
        join information_schema.columns c2
            on c1.table_schema = c2.table_schema
            and c1.table_name = c2.table_name
        where c1.table_schema = 'public'
        and c1.column_name = 'tenant_id'
        and c2.column_name = 'valid_to'
        and c1.table_name like 'ores_%_tbl'
        -- Exclude tables handled specially
        and c1.table_name not in (
            'ores_iam_tenants_tbl',      -- handled at the end
            'ores_iam_sessions_tbl',     -- uses end_time, not valid_to
            'ores_iam_login_info_tbl'    -- non-temporal, handled above
        )
        order by c1.table_name
    loop
        v_sql := format(
            'UPDATE %I SET valid_to = current_timestamp ' ||
            'WHERE tenant_id = $1 AND valid_to = ores_utility_infinity_timestamp_fn()',
            v_table_name
        );

        execute v_sql using p_tenant_id;
        get diagnostics v_affected_count = row_count;

        if v_affected_count > 0 then
            raise notice 'Soft-deleted % rows from %', v_affected_count, v_table_name;
            v_total_soft_deleted := v_total_soft_deleted + v_affected_count;
        end if;
    end loop;

    raise notice 'Total soft-deleted: % rows', v_total_soft_deleted;

    -- =========================================================================
    -- Dynamically hard-delete all non-temporal tables with tenant_id only
    -- =========================================================================
    for v_table_name in
        select distinct c.table_name
        from information_schema.columns c
        where c.table_schema = 'public'
        and c.column_name = 'tenant_id'
        and c.table_name like 'ores_%_tbl'
        -- Only tables without valid_to column
        and not exists (
            select 1 from information_schema.columns c2
            where c2.table_schema = c.table_schema
            and c2.table_name = c.table_name
            and c2.column_name = 'valid_to'
        )
        -- Exclude tables handled specially
        and c.table_name not in (
            'ores_iam_tenants_tbl',
            'ores_iam_sessions_tbl',
            'ores_iam_login_info_tbl'
        )
        order by c.table_name
    loop
        v_sql := format('DELETE FROM %I WHERE tenant_id = $1', v_table_name);

        execute v_sql using p_tenant_id;
        get diagnostics v_affected_count = row_count;

        if v_affected_count > 0 then
            raise notice 'Hard-deleted % rows from %', v_affected_count, v_table_name;
            v_total_hard_deleted := v_total_hard_deleted + v_affected_count;
        end if;
    end loop;

    raise notice 'Total hard-deleted: % rows', v_total_hard_deleted;

    -- =========================================================================
    -- Finally, terminate the tenant itself
    -- =========================================================================
    update ores_iam_tenants_tbl
    set status = 'terminated',
        valid_to = current_timestamp
    where id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn();

    raise notice 'Tenant deprovisioning complete: % (id: %)', v_tenant_code, p_tenant_id;
end;
$$ language plpgsql;
