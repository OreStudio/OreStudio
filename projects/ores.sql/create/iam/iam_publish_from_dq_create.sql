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

-- =============================================================================
-- IAM Publish-from-DQ Functions
--
-- Called by the generic ores.iam.core publish_from_dq_handler (subject ->
-- function name derived as ores_iam_publish_<entity>_from_dq_fn, same
-- convention as ores.refdata's handler), one per iam.v1.*.publish-from-dq
-- subject.
-- =============================================================================

-- =============================================================================
-- Accounts: iam.v1.accounts.publish-from-dq
-- =============================================================================

/**
 * Accounts Publish-from-DQ Function
 *
 * Reads ores_dq_accounts_artefact_tbl for the given dataset and creates
 * ores_iam_accounts_tbl rows, associating each new account with the
 * target party (params.party_id, required -- accounts are published
 * per legal entity, same as business units/portfolios/books). Idempotent
 * by (tenant_id, username): an account that already exists is skipped.
 *
 * business_unit_code in the artefact is informational only today --
 * ores_iam_accounts_tbl has no business-unit FK (accounts are scoped to
 * a party, not a desk); it is not consumed here.
 */
create or replace function ores_iam_publish_accounts_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_target_party_id uuid;
    v_inserted bigint := 0;
    v_skipped bigint := 0;
    r record;
    v_account_id uuid;
begin
    v_target_party_id := (p_params ->> 'party_id')::uuid;
    if v_target_party_id is null then
        raise exception 'ores_iam_publish_accounts_from_dq_fn requires params.party_id';
    end if;

    for r in
        select * from ores_dq_accounts_artefact_tbl
        where dataset_id = p_dataset_id
    loop
        if exists (
            select 1 from ores_iam_accounts_tbl
            where tenant_id = p_target_tenant_id
              and username = r.username
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        v_account_id := gen_random_uuid();

        insert into ores_iam_accounts_tbl (
            id, tenant_id, version, account_type, username, password_hash,
            password_salt, totp_secret, email,
            modified_by, performed_by, change_reason_code, change_commentary,
            valid_from, valid_to
        ) values (
            v_account_id, p_target_tenant_id, 0, r.account_type, r.username, r.password_hash,
            '', '', r.email,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from organisation dataset',
            current_timestamp, ores_utility_infinity_timestamp_fn()
        );

        insert into ores_iam_account_parties_tbl (
            account_id, tenant_id, party_id, version,
            modified_by, performed_by, change_reason_code, change_commentary,
            valid_from, valid_to
        ) values (
            v_account_id, p_target_tenant_id, v_target_party_id, 0,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from organisation dataset',
            current_timestamp, ores_utility_infinity_timestamp_fn()
        );

        -- Best-effort role assignment by exact role name; a role that
        -- doesn't exist in this tenant's RBAC set is silently skipped
        -- rather than blocking account creation.
        if r.role is not null then
            insert into ores_iam_account_roles_tbl (
                tenant_id, account_id, role_id, assigned_by, assigned_at,
                change_reason_code, change_commentary, valid_from, valid_to
            )
            select p_target_tenant_id, v_account_id, rl.id, current_user, current_timestamp,
                   'system.external_data_import', 'Published from organisation dataset',
                   current_timestamp, ores_utility_infinity_timestamp_fn()
            from ores_iam_roles_tbl rl
            where rl.tenant_id = p_target_tenant_id
              and rl.name = r.role
              and rl.valid_to = ores_utility_infinity_timestamp_fn();
        end if;

        v_inserted := v_inserted + 1;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Account Contact Informations: iam.v1.account-contact-informations.publish-from-dq
-- =============================================================================

/**
 * Account Contact Informations Publish-from-DQ Function
 *
 * Reads ores_dq_account_contact_informations_artefact_tbl for the given
 * dataset and creates ores_iam_account_contact_informations_tbl rows.
 * account_username identifies the owning account by username (not id,
 * since the artefact predates the account's own publish step) -- an
 * artefact row whose account hasn't been published yet (username not
 * found) is skipped, not an error, since accounts and their contact
 * informations publish as separate steps. Idempotent by account_id: an
 * account that already has a contact information row is skipped.
 */
create or replace function ores_iam_publish_account_contact_informations_from_dq_fn(
    p_dataset_id uuid,
    p_target_tenant_id uuid,
    p_mode text default 'upsert',
    p_params jsonb default '{}'::jsonb
)
returns table (
    action text,
    record_count bigint
) as $$
declare
    v_inserted bigint := 0;
    v_skipped bigint := 0;
    r record;
    v_account_id uuid;
begin
    for r in
        select * from ores_dq_account_contact_informations_artefact_tbl
        where dataset_id = p_dataset_id
    loop
        select id into v_account_id
        from ores_iam_accounts_tbl
        where tenant_id = p_target_tenant_id
          and username = r.account_username
          and valid_to = ores_utility_infinity_timestamp_fn();

        if v_account_id is null then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        if exists (
            select 1 from ores_iam_account_contact_informations_tbl
            where tenant_id = p_target_tenant_id
              and account_id = v_account_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            v_skipped := v_skipped + 1;
            continue;
        end if;

        insert into ores_iam_account_contact_informations_tbl (
            id, tenant_id, version, account_id, full_name,
            street_line_1, street_line_2, city, state, country_code,
            postal_code, phone, email, web_page,
            modified_by, performed_by, change_reason_code, change_commentary,
            valid_from, valid_to
        ) values (
            gen_random_uuid(), p_target_tenant_id, 0, v_account_id, r.full_name,
            r.street_line_1, r.street_line_2, r.city, r.state, r.country_code,
            r.postal_code, r.phone, r.email, r.web_page,
            coalesce(ores_iam_current_service_fn(), current_user), current_user,
            'system.external_data_import', 'Published from organisation dataset',
            current_timestamp, ores_utility_infinity_timestamp_fn()
        );

        v_inserted := v_inserted + 1;
    end loop;

    return query
    select 'inserted'::text, v_inserted
    where v_inserted > 0
    union all select 'skipped'::text, v_skipped
    where v_skipped > 0;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
