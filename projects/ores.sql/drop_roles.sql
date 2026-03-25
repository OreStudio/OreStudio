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
 * Drop Environment Roles
 *
 * Removes all cluster-level roles belonging to a specific environment.
 * This script revokes group memberships, drops individual user roles, then
 * drops group roles — in the correct dependency order.
 *
 * The environment database must be dropped BEFORE running this script.
 *
 * Required psql variables:
 *   :env_label    The environment label, e.g. local1, local2
 *
 * USAGE:
 *   psql -U postgres -v env_label=local2 -f drop_roles.sql
 *
 * This script is idempotent — missing roles are silently skipped.
 */

\set ON_ERROR_STOP on

\echo '=== Dropping environment roles for:' :env_label '==='
\echo ''

do $$
declare
    v_label   text := :'env_label';
    v_owner   text;
    v_rw      text;
    v_ro      text;
    v_users   text[];
    v_role    text;
begin
    v_owner := 'ores_' || v_label || '_owner';
    v_rw    := 'ores_' || v_label || '_rw';
    v_ro    := 'ores_' || v_label || '_ro';

    v_users := array[
        'ores_' || v_label || '_ddl_user',
        'ores_' || v_label || '_cli_user',
        'ores_' || v_label || '_wt_user',
        'ores_' || v_label || '_comms_user',
        'ores_' || v_label || '_http_user',
        'ores_' || v_label || '_test_ddl_user',
        'ores_' || v_label || '_test_dml_user',
        'ores_' || v_label || '_readonly_user',
        'ores_' || v_label || '_iam_service',
        'ores_' || v_label || '_refdata_service',
        'ores_' || v_label || '_dq_service',
        'ores_' || v_label || '_variability_service',
        'ores_' || v_label || '_assets_service',
        'ores_' || v_label || '_synthetic_service',
        'ores_' || v_label || '_scheduler_service',
        'ores_' || v_label || '_reporting_service',
        'ores_' || v_label || '_telemetry_service',
        'ores_' || v_label || '_trading_service',
        'ores_' || v_label || '_compute_service'
    ];

    -- Revoke group memberships and drop user roles
    foreach v_role in array v_users loop
        if exists (select 1 from pg_roles where rolname = v_role) then
            -- Revoke from all group roles (no-op if not a member)
            execute format('revoke %I from %I', v_owner, v_role);
            execute format('revoke %I from %I', v_rw,    v_role);
            execute format('revoke %I from %I', v_ro,    v_role);
            execute format('drop role %I', v_role);
            raise notice 'Dropped role: %', v_role;
        else
            raise notice 'Role not found (skipping): %', v_role;
        end if;
    end loop;

    -- Drop group roles (after all members are removed)
    foreach v_role in array array[v_owner, v_rw, v_ro] loop
        if exists (select 1 from pg_roles where rolname = v_role) then
            execute format('drop role %I', v_role);
            raise notice 'Dropped role: %', v_role;
        else
            raise notice 'Role not found (skipping): %', v_role;
        end if;
    end loop;
end $$;

\echo ''
\echo '=== Role removal complete ==='
\echo ''
