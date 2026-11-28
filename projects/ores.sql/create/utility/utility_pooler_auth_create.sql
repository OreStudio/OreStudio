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
 * Pooler Authentication Lookup
 *
 * The transaction pooler authenticates each client as itself, so it needs that
 * client's verifier. Two ways give it one: a list of plaintext passwords kept
 * beside the pooler, or a question asked of the server at the moment of the
 * connection. The list is a second home for every password in the deployment,
 * and it has to be rewritten whenever one changes; the question has no such
 * state.
 *
 * The catalog it reads is readable by a superuser alone, so the question is
 * answered by a function that runs as its owner. The pooler's own account is
 * an ordinary role: it may call this and read nothing else.
 */

create or replace function ores_utility_pooler_auth_lookup_fn(p_username text)
returns table (usename text, passwd text) as $$
    select s.usename, s.passwd
    from pg_shadow s
    where s.usename = p_username;
$$ language sql stable security definer set search_path = pg_catalog, pg_temp;

comment on function ores_utility_pooler_auth_lookup_fn(text) is
    'The verifier a transaction pooler authenticates one client with, so the deployment keeps no copy of any password.';

do $$
begin
    if exists (select 1 from pg_roles where rolname = 'ores_pooler_auth') then
        grant execute on function ores_utility_pooler_auth_lookup_fn(text) to ores_pooler_auth;
    end if;
end
$$;
