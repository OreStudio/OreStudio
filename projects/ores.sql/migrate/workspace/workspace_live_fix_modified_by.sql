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
 * Fix modified_by on the Live workspace.
 *
 * The original bootstrap insert used 'system' as modified_by (a special
 * bypass value allowed when no accounts existed). This migration creates a
 * new bi-temporal version of the Live workspace with current_user as
 * modified_by, preserving the original row in history.
 *
 * Run as: run_sql.sh --user ddl -f migrate/workspace/workspace_live_fix_modified_by.sql
 */

DO $$
DECLARE
    v_live_id uuid := ores_utility_live_workspace_id_fn();
    v_row ores_workspaces_tbl%ROWTYPE;
BEGIN
    select * into v_row
    from ores_workspaces_tbl
    where id = v_live_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        raise notice 'Live workspace not found, skipping.';
        return;
    end if;

    if v_row.modified_by != 'system' and v_row.performed_by != 'system' then
        raise notice 'Live workspace already has valid modified_by=%, skipping.', v_row.modified_by;
        return;
    end if;

    insert into ores_workspaces_tbl
        (id, version, name, description, source_path,
         parent_workspace_id, scope_portfolio_id,
         owner_id, status_code,
         modified_by, performed_by, change_reason_code, change_commentary,
         valid_from, valid_to)
    values
        (v_row.id, 0, v_row.name, v_row.description, v_row.source_path,
         v_row.parent_workspace_id, v_row.scope_portfolio_id,
         v_row.owner_id, v_row.status_code,
         current_user, current_user, 'common.rectification', 'Fix bootstrap modified_by from system to service role',
         now(), 'infinity');

    raise notice 'Live workspace modified_by updated: id=%', v_live_id;
END $$;
