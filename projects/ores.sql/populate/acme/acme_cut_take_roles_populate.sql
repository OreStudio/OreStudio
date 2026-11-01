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
 * Acme Cut Take Roles
 *
 * Gives the Acme accounts who walk the cut journey the Finance role, which is
 * the role that holds reporting::cuts:take. The end of day cut is the finance
 * officer's to take and somebody else's to sign, and the demo's accounts are
 * seeded one role apiece, so the person who takes the close needs a second one.
 *
 * henry.jones takes the end of day cut and rachel.byrd, who holds the Finance
 * role of her own business unit, decides it: the taker never authorises the
 * body they fixed, and the two are two people by construction.
 *
 * The role assignment is additive and idempotent: it writes a row only where
 * the account holds no current arm of that role.
 */

\echo '--- Acme cut take roles ---'

DO $$
declare
    v_tenant_id uuid;
    v_role_id uuid;
    v_account_id uuid;
    v_account text;
    v_accounts text[] := array['henry.jones'];
begin
    -- The tenant is named for the people who read it, and a login names it as a
    -- slug, so the row is found by the slug the two spellings share.
    select id into v_tenant_id
    from ores_iam_tenants_tbl
    where lower(replace(name, ' ', '_')) = 'acme_corporation'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_tenant_id is null then
        raise notice 'No acme_corporation tenant, so no cut take roles are seeded.';
        return;
    end if;

    select id into v_role_id
    from ores_iam_roles_tbl
    where tenant_id = v_tenant_id
      and name = 'Finance'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_role_id is null then
        raise notice 'The acme_corporation tenant holds no Finance role, so no cut take roles are seeded.';
        return;
    end if;

    -- The role arm is written inside the tenant it belongs to: the store checks
    -- that the account exists in the session's tenant, and a populate script
    -- runs as nobody.
    perform set_config('app.current_tenant_id', v_tenant_id::text, true);

    foreach v_account in array v_accounts
    loop
        select id into v_account_id
        from ores_iam_accounts_tbl
        where tenant_id = v_tenant_id
          and username = v_account
          and valid_to = ores_utility_infinity_timestamp_fn();

        continue when v_account_id is null;

        if not exists (
            select 1 from ores_iam_account_roles_tbl
            where tenant_id = v_tenant_id
              and account_id = v_account_id
              and role_id = v_role_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_iam_account_roles_tbl (
                tenant_id, account_id, role_id, version, modified_by, performed_by,
                change_reason_code, change_commentary, valid_from, valid_to
            ) values (
                v_tenant_id, v_account_id, v_role_id, 0, v_account,
                coalesce(ores_iam_current_service_fn(), v_account),
                'system.external_data_import',
                'The finance officer who takes the end of day cut',
                clock_timestamp(), ores_utility_infinity_timestamp_fn()
            );
        end if;
    end loop;
END $$;
