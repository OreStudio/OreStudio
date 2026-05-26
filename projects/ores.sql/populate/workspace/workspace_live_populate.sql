/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software: redistribute under GPLv3 or later.
 *
 */

/**
 * Workspace Population Script
 *
 * Seeds the Live (root) workspace using the sentinel UUID returned by
 * ores_utility_live_workspace_id_fn(). This workspace is the root of all
 * workspace inheritance chains and can never be archived.
 *
 * Run as the ddl service role: current_user resolves to 'ddl', which passes
 * the modified_by validator. Use: run_sql.sh --user ddl -f <this_file>
 */

DO $$
DECLARE
    v_live_id uuid := ores_utility_live_workspace_id_fn();
    v_owner_id uuid;
BEGIN
    -- Resolve the account UUID for the current service role (ddl user)
    select id into v_owner_id
    from ores_iam_accounts_tbl
    where username = current_user
      and valid_to = ores_utility_infinity_timestamp_fn()
    limit 1;

    if v_owner_id is null then
        raise exception 'Cannot seed Live workspace: no account found for current_user=%',
            current_user;
    end if;

    if not exists (
        select 1 from ores_workspaces_tbl
        where id = v_live_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_workspaces_tbl
            (id, version, name, description, source_path,
             parent_workspace_id, scope_portfolio_id,
             owner_id, status_code,
             modified_by, performed_by, change_reason_code, change_commentary,
             valid_from, valid_to)
        values
            (v_live_id, 0, 'Live', 'Live production data space', '',
             null, null,
             v_owner_id, 'active',
             current_user, current_user, 'system.initial_load', 'System initialisation',
             now(), 'infinity');

        raise notice 'Live workspace seeded: id=%', v_live_id;
    else
        raise notice 'Live workspace already present, skipping.';
    end if;
END $$;
