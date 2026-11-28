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
 * Service Account Credential Rotation
 *
 * A service proves two things with one value: it logs in to the database role
 * the deployment created for it, and it signs in to IAM, which compares the
 * value against the hash on the account's credential row. Rotating one and not
 * the other leaves a service that connects to its database and cannot obtain a
 * token, or the reverse.
 *
 * The seed function that writes a service account's credential is not the tool
 * for this: it returns as soon as it finds a live credential, which is what
 * makes a seed safe to re-run. This one exists to change the value, and it is
 * the only path that does, so a deployment that rotates a role's password calls
 * it in the same step.
 *
 * It refuses a username it cannot find rather than creating an account: a
 * provisioning step that mistypes a name should not leave a new account behind.
 */

-- compass:except(entity_writes/sql_writer) expires=2027-05-18 approver=marco reason=rotating a credential is a deployment act that has to move the role, the hash and the vault together, and there is no session to carry it
create or replace function ores_iam_service_account_credential_set_fn(
    p_username text,
    p_password text
) returns void as $$
declare
    v_account_id    uuid;
    v_credential_id uuid;
    v_version       integer;
begin
    perform ores_seed_validate_not_empty_fn(p_username, 'Service account username');
    perform ores_seed_validate_not_empty_fn(p_password, 'Service account password');

    select a.id into v_account_id
    from ores_iam_accounts_tbl a
    where a.tenant_id = ores_utility_system_tenant_id_fn()
      and a.username = p_username
      and a.valid_to = ores_utility_infinity_timestamp_fn();

    if v_account_id is null then
        raise exception 'No service account named % holds a current row in the system tenant', p_username
            using errcode = '23503',
                  hint = 'The deployment creates a service account when it provisions the database; a rotation changes one that exists.';
    end if;

    -- The live row's own identity and version, because a credential is
    -- versioned like every other entity: the write carries the id it replaces
    -- and the version it believes that row is at, and the store closes it and
    -- writes the next version. A fresh id would be read as a create and refused
    -- by the one-live-credential-per-account index.
    select c.id, c.version into v_credential_id, v_version
    from ores_iam_account_credentials_tbl c
    where c.tenant_id = ores_utility_system_tenant_id_fn()
      and c.account_id = v_account_id
      and c.valid_to = ores_utility_infinity_timestamp_fn();

    insert into ores_iam_account_credentials_tbl (
        id, tenant_id, version, account_id, service_password_hash,
        modified_by, performed_by, change_reason_code, change_commentary,
        valid_from, valid_to
    )
    values (
        coalesce(v_credential_id, gen_random_uuid()),
        ores_utility_system_tenant_id_fn(),
        coalesce(v_version, 0),
        v_account_id,
        encode(sha256(p_password::bytea), 'hex'),
        current_user,
        current_user,
        'system.update',
        'Credential rotated by the deployment, in step with the database role',
        current_timestamp,
        ores_utility_infinity_timestamp_fn()
    );

    raise debug 'Rotated the credential of service account: %', p_username;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
