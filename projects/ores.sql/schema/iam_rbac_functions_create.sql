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
set schema 'ores';

create or replace function ores.iam_get_effective_permissions_fn(p_account_id uuid)
returns table(code text) as $$
begin
    return query
    select distinct p.code
    from ores.iam_permissions_tbl p
    join ores.iam_role_permissions_tbl rp on p.id = rp.permission_id
    join ores.iam_account_roles_tbl ar on rp.role_id = ar.role_id
    where ar.account_id = p_account_id
    and p.valid_to = ores.utility_infinity_timestamp_fn()
    and rp.valid_to = ores.utility_infinity_timestamp_fn()
    and ar.valid_to = ores.utility_infinity_timestamp_fn()
    order by p.code;
end;
$$ language plpgsql stable;

create or replace function ores.iam_get_all_role_permission_codes_fn()
returns table(role_id text, code text) as $$
begin
    return query
    select rp.role_id::text, p.code
    from ores.iam_role_permissions_tbl rp
    join ores.iam_permissions_tbl p on rp.permission_id = p.id
    where rp.valid_to = ores.utility_infinity_timestamp_fn()
    and p.valid_to = ores.utility_infinity_timestamp_fn()
    order by rp.role_id, p.code;
end;
$$ language plpgsql stable;

create or replace function ores.iam_get_role_permission_codes_fn(p_role_ids uuid[])
returns table(role_id text, code text) as $$
begin
    return query
    select rp.role_id::text, p.code
    from ores.iam_role_permissions_tbl rp
    join ores.iam_permissions_tbl p on rp.permission_id = p.id
    where rp.role_id = any(p_role_ids)
    and rp.valid_to = ores.utility_infinity_timestamp_fn()
    and p.valid_to = ores.utility_infinity_timestamp_fn()
    order by rp.role_id, p.code;
end;
$$ language plpgsql stable;

create or replace function ores.iam_get_roles_by_ids_fn(p_role_ids uuid[])
returns table(
    id uuid,
    version integer,
    name text,
    description text,
    modified_by text
) as $$
begin
    return query
    select r.id, r.version, r.name, r.description, r.modified_by
    from ores.iam_roles_tbl r
    where r.id = any(p_role_ids)
    and r.valid_to = ores.utility_infinity_timestamp_fn()
    order by r.name;
end;
$$ language plpgsql stable;

create or replace function ores.iam_get_account_roles_with_permissions_fn(p_account_id uuid)
returns table(
    role_id uuid,
    role_version integer,
    role_name text,
    role_description text,
    role_modified_by text,
    permission_codes text
) as $$
begin
    return query
    select
        r.id,
        r.version,
        r.name,
        r.description,
        r.modified_by,
        coalesce(string_agg(p.code, ',' order by p.code), '') as permission_codes
    from ores.iam_account_roles_tbl ar
    join ores.iam_roles_tbl r on ar.role_id = r.id
    left join ores.iam_role_permissions_tbl rp on r.id = rp.role_id
        and rp.valid_to = ores.utility_infinity_timestamp_fn()
    left join ores.iam_permissions_tbl p on rp.permission_id = p.id
        and p.valid_to = ores.utility_infinity_timestamp_fn()
    where ar.account_id = p_account_id
    and ar.valid_to = ores.utility_infinity_timestamp_fn()
    and r.valid_to = ores.utility_infinity_timestamp_fn()
    group by r.id, r.version, r.name, r.description, r.modified_by
    order by r.name;
end;
$$ language plpgsql stable;

create or replace function ores.iam_account_has_permission_fn(p_username text, p_permission_code text)
returns boolean as $$
declare
    v_account_id uuid;
begin
    select id into v_account_id
    from ores.iam_accounts_tbl
    where username = p_username
    and valid_to = ores.utility_infinity_timestamp_fn();

    if v_account_id is null then
        return false;
    end if;

    return exists (
        select 1
        from ores.iam_account_roles_tbl ar
        join ores.iam_role_permissions_tbl rp on ar.role_id = rp.role_id
        join ores.iam_permissions_tbl p on rp.permission_id = p.id
        where ar.account_id = v_account_id
        and p.code = p_permission_code
        and ar.valid_to = ores.utility_infinity_timestamp_fn()
        and rp.valid_to = ores.utility_infinity_timestamp_fn()
        and p.valid_to = ores.utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable;

create or replace function ores.iam_account_has_permission_by_id_fn(p_account_id uuid, p_permission_code text)
returns boolean as $$
begin
    return exists (
        select 1
        from ores.iam_account_roles_tbl ar
        join ores.iam_role_permissions_tbl rp on ar.role_id = rp.role_id
        join ores.iam_permissions_tbl p on rp.permission_id = p.id
        where ar.account_id = p_account_id
        and p.code = p_permission_code
        and ar.valid_to = ores.utility_infinity_timestamp_fn()
        and rp.valid_to = ores.utility_infinity_timestamp_fn()
        and p.valid_to = ores.utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable;
