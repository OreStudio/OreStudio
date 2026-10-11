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
 * The parties whose books a request's lines change.
 *
 * A book belongs to a party, and a party's rows are visible only to those who
 * may see the party. The workflow that applies an approved request runs for the
 * tenant, not for a person, so it has no party to start from. It asks this
 * function for the parties of the request's own lines and scopes itself to
 * them, and to no others.
 *
 * The function reads past the party policy so that it can answer the question,
 * and so it answers nothing else: it returns party ids only, and only for the
 * caller's own tenant.
 */
create or replace function ores_refdata_book_change_parties_fn(p_request_id uuid)
returns setof uuid as $$
    select distinct c.party_id
    from ores_refdata_book_changes_tbl c
    where c.request_id = p_request_id
      and c.tenant_id = ores_iam_current_tenant_id_fn()
      and c.valid_to = ores_utility_infinity_timestamp_fn()
    order by c.party_id;
$$ language sql stable security definer set search_path = public, pg_temp;

-- The function reads past the party policy, so PostgreSQL's default grant to
-- PUBLIC is revoked. The refdata service runs the apply and is the only service
-- granted it (Execute prefixes in service_registry.org). The read-write role the
-- test suites sign in under keeps it, so it can be tested.
revoke execute on function ores_refdata_book_change_parties_fn(uuid) from public;
grant execute on function ores_refdata_book_change_parties_fn(uuid) to :rw_role;
