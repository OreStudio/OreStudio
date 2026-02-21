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

-- Returns the UUID of the system party for the given tenant, or NULL if none
-- exists. Every tenant should have exactly one party with party_category =
-- 'System', created during provisioning.
create or replace function ores_iam_account_parties_system_party_id_fn(p_tenant_id uuid)
returns uuid as $$
declare
    v_party_id uuid;
begin
    select id into v_party_id
    from ores_refdata_parties_tbl
    where party_category = 'System'
    and tenant_id = p_tenant_id
    and valid_to = ores_utility_infinity_timestamp_fn()
    limit 1;

    return v_party_id;
end;
$$ language plpgsql stable security definer;
