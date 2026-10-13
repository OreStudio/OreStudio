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

-- =============================================================================
-- Row-Level Security Policies for IAM Tables
-- =============================================================================
-- These policies enforce strict tenant isolation for IAM data.
-- Each tenant can only see and modify their own accounts, roles, permissions,
-- etc. All tenants are fully isolated, including the system tenant.

-- -----------------------------------------------------------------------------
-- Accounts
-- -----------------------------------------------------------------------------
alter table ores_iam_accounts_tbl enable row level security;

-- Shared accounts. The service accounts and the super administrator belong to
-- the system tenant, and the history of every tenant names them as the author
-- of an entry. A system tenant row with shared set is therefore readable by
-- every tenant, and nothing else of the system tenant is. This is the
-- shared-reference shape: reads see the shared rows, and every write is
-- confined to the tenant's own rows. It is one policy per command, because a
-- single `for all` policy uses one USING clause for select, update and delete,
-- and a tenant could then change or remove a shared row.
drop policy if exists accounts_tenant_isolation_policy
    on ores_iam_accounts_tbl;
drop policy if exists accounts_select_policy
    on ores_iam_accounts_tbl;
drop policy if exists accounts_insert_policy
    on ores_iam_accounts_tbl;
drop policy if exists accounts_update_policy
    on ores_iam_accounts_tbl;
drop policy if exists accounts_delete_policy
    on ores_iam_accounts_tbl;

create policy accounts_select_policy on ores_iam_accounts_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or (tenant_id = ores_utility_system_tenant_id_fn() and shared)
);

create policy accounts_insert_policy on ores_iam_accounts_tbl
for insert with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy accounts_update_policy on ores_iam_accounts_tbl
for update using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy accounts_delete_policy on ores_iam_accounts_tbl
for delete using (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party visibility of accounts (RESTRICTIVE, reads only). HAND-WRITTEN
-- EXCEPTION to the party-isolation shape, which keys on a party_id column and
-- on the session's visible party set.
--
-- An account has no party_id column, because a person works in several
-- parties; its parties are the links in ores_iam_account_parties_tbl. And the
-- scope is the parties the caller's account works in, not the party the session
-- acts for, so a head of group linked to several parties still sees all of them.
-- A session sees an account that works in any party the caller's own account
-- works in. A session with no actor (a service acting as itself, or the sign-in
-- lookup before a session exists) is not restricted, as with the visible-party
-- passthrough on party-owned tables. The caller always sees their own account.
--
-- An actor whose account has no party links gets no party set, and sees only its
-- own account. A tenant administrator sees the tenant because its account is
-- linked to every party of the tenant, and not because the policy names it.
-- A shared account works in no party, so the party rule passes it and leaves
-- the decision to the select policy above. A service actor passes too, because
-- a service works in no party and needs the people of the tenant. Everyone in
-- the system party of the tenant is seen by every person, as the product owner
-- decided.
--
-- The function calls are wrapped in a select, so the planner runs each once per
-- query and not once per row.
--
-- Security definer, so that reading the caller's own account to find its links
-- does not apply this policy to itself.
create or replace function ores_iam_actor_party_ids_fn()
returns uuid[] as $$
    select array_agg(ap.party_id)
    from ores_iam_accounts_tbl a
    join ores_iam_account_parties_tbl ap
      on ap.tenant_id = a.tenant_id
     and ap.account_id = a.id
     and ap.valid_to = ores_utility_infinity_timestamp_fn()
    where a.username = ores_iam_current_actor_fn()
      and a.tenant_id = ores_iam_current_tenant_id_fn()
      and a.valid_to = ores_utility_infinity_timestamp_fn();
$$ language sql stable security definer set search_path = public, pg_temp;

-- Whether the session's actor is a service account: one of the session's own
-- tenant, or else a shared one of the system tenant. A service acting inside a
-- tenant works in no party, and it needs the people of the tenant to do its
-- work: the run grant exchange reads the grantor, the role grant applier reads
-- the account it grants to. The party rule is a rule about people, so it passes
-- a service actor.
--
-- A username is unique within a tenant only. A person of a tenant can share a
-- username with a shared service account of the system tenant, so an account of
-- the session's own tenant decides first, and the shared account decides only
-- when the tenant has none of that name.
--
-- Security definer, so that reading the actor's own account does not apply the
-- policy to itself.
create or replace function ores_iam_actor_is_service_fn()
returns boolean as $$
    select coalesce(
        (select a.account_type = 'service'
         from ores_iam_accounts_tbl a
         where a.username = ores_iam_current_actor_fn()
           and a.tenant_id = ores_iam_current_tenant_id_fn()
           and a.valid_to = ores_utility_infinity_timestamp_fn()),
        (select a.account_type = 'service'
         from ores_iam_accounts_tbl a
         where a.username = ores_iam_current_actor_fn()
           and a.tenant_id = ores_utility_system_tenant_id_fn()
           and a.shared
           and a.valid_to = ores_utility_infinity_timestamp_fn()),
        false);
$$ language sql stable security definer set search_path = public, pg_temp;

-- The system party of the session's tenant. Every person sees the people who
-- work in it. The body is plpgsql, so the refdata function is bound when the
-- policy runs and not when this file is loaded.
create or replace function ores_iam_system_party_id_fn()
returns uuid as $$
begin
    return (select id
            from ores_refdata_read_system_party_fn(ores_iam_current_tenant_id_fn())
            limit 1);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

drop policy if exists accounts_party_visibility_policy
    on ores_iam_accounts_tbl;

create policy accounts_party_visibility_policy on ores_iam_accounts_tbl
as restrictive
for select using (
    (select ores_iam_current_actor_fn()) is null
    or username = (select ores_iam_current_actor_fn())
    or (tenant_id = ores_utility_system_tenant_id_fn() and shared)
    or (select ores_iam_actor_is_service_fn())
    or exists (
        select 1
        from ores_iam_account_parties_tbl sp
        where sp.tenant_id = ores_iam_accounts_tbl.tenant_id
          and sp.account_id = ores_iam_accounts_tbl.id
          and sp.valid_to = ores_utility_infinity_timestamp_fn()
          and sp.party_id = (select ores_iam_system_party_id_fn())
    )
    or exists (
        select 1
        from ores_iam_account_parties_tbl ap
        where ap.tenant_id = ores_iam_accounts_tbl.tenant_id
          and ap.account_id = ores_iam_accounts_tbl.id
          and ap.valid_to = ores_utility_infinity_timestamp_fn()
          and ap.party_id = any((select ores_iam_actor_party_ids_fn())::uuid[])
    )
);

-- Existence of an account, for the soft foreign keys that point at one.
--
-- A record that names a person, such as a notice to a recipient or a portfolio
-- right, checks that the account exists. A service writes that record, and the
-- party rule hides every account from a service, because a service works in no
-- party. The check needs the answer yes or no and no column, so it asks this
-- function and not the table.
--
-- The function takes the account id and nothing else. It takes the tenant from
-- the session, so a caller cannot name another tenant and the function is not
-- an oracle across tenants. It answers true for an account of the session's
-- tenant and for a shared account of the system tenant.
--
-- Security definer, with a fixed search_path. The owner is not subject to the
-- row-level policies. Access is revoked from public and granted by name below.
create or replace function ores_iam_account_exists_fn(p_account_id uuid)
returns boolean as $$
    select exists (
        select 1
        from ores_iam_accounts_tbl a
        where a.id = p_account_id
          and a.valid_to = ores_utility_infinity_timestamp_fn()
          and (a.tenant_id = ores_iam_current_tenant_id_fn()
               or (a.tenant_id = ores_utility_system_tenant_id_fn() and a.shared))
    );
$$ language sql stable security definer set search_path = public, pg_temp;

revoke execute on function ores_iam_account_exists_fn(uuid) from public;

-- -----------------------------------------------------------------------------
-- Account Credentials
-- -----------------------------------------------------------------------------
alter table ores_iam_account_credentials_tbl enable row level security;

drop policy if exists account_credentials_tenant_isolation_policy
    on ores_iam_account_credentials_tbl;

create policy account_credentials_tenant_isolation_policy
on ores_iam_account_credentials_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Contact Informations
-- -----------------------------------------------------------------------------
alter table ores_iam_account_contact_informations_tbl enable row level security;

drop policy if exists account_contact_informations_tenant_isolation_policy
    on ores_iam_account_contact_informations_tbl;

create policy account_contact_informations_tenant_isolation_policy
on ores_iam_account_contact_informations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Roles
-- -----------------------------------------------------------------------------
alter table ores_iam_roles_tbl enable row level security;

drop policy if exists roles_tenant_isolation_policy
    on ores_iam_roles_tbl;

create policy roles_tenant_isolation_policy on ores_iam_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Permissions
-- -----------------------------------------------------------------------------
alter table ores_iam_permissions_tbl enable row level security;

drop policy if exists permissions_tenant_isolation_policy
    on ores_iam_permissions_tbl;

create policy permissions_tenant_isolation_policy on ores_iam_permissions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Roles (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_account_roles_tbl enable row level security;

drop policy if exists account_roles_tenant_isolation_policy
    on ores_iam_account_roles_tbl;

create policy account_roles_tenant_isolation_policy on ores_iam_account_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Role Permissions (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_role_permissions_tbl enable row level security;

drop policy if exists role_permissions_tenant_isolation_policy
    on ores_iam_role_permissions_tbl;

create policy role_permissions_tenant_isolation_policy on ores_iam_role_permissions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Sessions
-- -----------------------------------------------------------------------------
alter table ores_iam_sessions_tbl enable row level security;

drop policy if exists sessions_tenant_isolation_policy
    on ores_iam_sessions_tbl;

create policy sessions_tenant_isolation_policy on ores_iam_sessions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Login Info
-- -----------------------------------------------------------------------------
alter table ores_iam_login_info_tbl enable row level security;

drop policy if exists login_info_tenant_isolation_policy
    on ores_iam_login_info_tbl;

create policy login_info_tenant_isolation_policy on ores_iam_login_info_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenants Table
-- -----------------------------------------------------------------------------
-- All tenant records are owned by system tenant (tenant_id = system_tenant_id).
-- The system tenant can manage all tenants (create, update, delete).
-- Regular tenants can only read their own tenant record (by id).
alter table ores_iam_tenants_tbl enable row level security;

drop policy if exists tenants_read_policy
    on ores_iam_tenants_tbl;

create policy tenants_read_policy on ores_iam_tenants_tbl
for select using (
    id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists tenants_write_policy
    on ores_iam_tenants_tbl;

create policy tenants_write_policy on ores_iam_tenants_tbl
for all using (tenant_id = ores_iam_current_tenant_id_fn())
with check (tenant_id = ores_iam_current_tenant_id_fn());

-- -----------------------------------------------------------------------------
-- Role Grant Request Roles (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_role_grant_request_roles_tbl enable row level security;

drop policy if exists role_grant_request_roles_tenant_isolation_policy
    on ores_iam_role_grant_request_roles_tbl;

create policy role_grant_request_roles_tenant_isolation_policy on ores_iam_role_grant_request_roles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Account Parties (many-to-many)
-- -----------------------------------------------------------------------------
alter table ores_iam_account_parties_tbl enable row level security;

drop policy if exists account_parties_tenant_isolation_policy
    on ores_iam_account_parties_tbl;

create policy account_parties_tenant_isolation_policy on ores_iam_account_parties_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Note: no party isolation policy on this table. account_parties IS the party
-- membership table — it must be readable before a party context is established
-- (e.g. during login). Tenant isolation above is the correct boundary here.
-- Party isolation on other tables (business_units, etc.) is fine because those
-- tables are only accessed after login when a party context is already set.

-- -----------------------------------------------------------------------------
-- Authentication events and run token issues
-- -----------------------------------------------------------------------------
-- Audit rows name accounts, sessions, source addresses and the runs a grant
-- served, so a tenant reads and writes only its own. Both tables carry the
-- tenant as text, because an event can happen before a tenant is known; such a
-- row is the system tenant's, and the system tenant reads only its own like any
-- other. The log classes write each row through a session of the row's tenant.
alter table ores_iam_auth_events_tbl enable row level security;

drop policy if exists auth_events_tenant_isolation_policy
    on ores_iam_auth_events_tbl;

create policy auth_events_tenant_isolation_policy on ores_iam_auth_events_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()::text
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()::text
);

alter table ores_iam_run_token_issues_tbl enable row level security;

drop policy if exists run_token_issues_tenant_isolation_policy
    on ores_iam_run_token_issues_tbl;

create policy run_token_issues_tenant_isolation_policy on ores_iam_run_token_issues_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()::text
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()::text
);

-- -----------------------------------------------------------------------------
-- Seed profiles and their parts
-- -----------------------------------------------------------------------------
-- The platform's own configuration. The models state :system_scope:, so every
-- row belongs to the system tenant and every tenant may read it, because a
-- tenant is provisioned from a profile. Only the system tenant writes it, which
-- a write confined to the session's own tenant states, because the insert
-- trigger pins every row to the system tenant. This is the shared-reference
-- shape: one policy per command, because a single
-- `for all` policy would let a tenant change or remove a profile.

alter table ores_iam_seed_profiles_tbl enable row level security;

drop policy if exists seed_profile_select_policy on ores_iam_seed_profiles_tbl;
drop policy if exists seed_profile_insert_policy on ores_iam_seed_profiles_tbl;
drop policy if exists seed_profile_update_policy on ores_iam_seed_profiles_tbl;
drop policy if exists seed_profile_delete_policy on ores_iam_seed_profiles_tbl;

create policy seed_profile_select_policy on ores_iam_seed_profiles_tbl
for select using (
    tenant_id = ores_utility_system_tenant_id_fn()
);

create policy seed_profile_insert_policy on ores_iam_seed_profiles_tbl
for insert with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_update_policy on ores_iam_seed_profiles_tbl
for update using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_delete_policy on ores_iam_seed_profiles_tbl
for delete using (
    tenant_id = ores_iam_current_tenant_id_fn()
);

alter table ores_iam_seed_profile_steps_tbl enable row level security;

drop policy if exists seed_profile_step_select_policy on ores_iam_seed_profile_steps_tbl;
drop policy if exists seed_profile_step_insert_policy on ores_iam_seed_profile_steps_tbl;
drop policy if exists seed_profile_step_update_policy on ores_iam_seed_profile_steps_tbl;
drop policy if exists seed_profile_step_delete_policy on ores_iam_seed_profile_steps_tbl;

create policy seed_profile_step_select_policy on ores_iam_seed_profile_steps_tbl
for select using (
    tenant_id = ores_utility_system_tenant_id_fn()
);

create policy seed_profile_step_insert_policy on ores_iam_seed_profile_steps_tbl
for insert with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_step_update_policy on ores_iam_seed_profile_steps_tbl
for update using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_step_delete_policy on ores_iam_seed_profile_steps_tbl
for delete using (
    tenant_id = ores_iam_current_tenant_id_fn()
);

alter table ores_iam_seed_profile_parameters_tbl enable row level security;

drop policy if exists seed_profile_parameter_select_policy on ores_iam_seed_profile_parameters_tbl;
drop policy if exists seed_profile_parameter_insert_policy on ores_iam_seed_profile_parameters_tbl;
drop policy if exists seed_profile_parameter_update_policy on ores_iam_seed_profile_parameters_tbl;
drop policy if exists seed_profile_parameter_delete_policy on ores_iam_seed_profile_parameters_tbl;

create policy seed_profile_parameter_select_policy on ores_iam_seed_profile_parameters_tbl
for select using (
    tenant_id = ores_utility_system_tenant_id_fn()
);

create policy seed_profile_parameter_insert_policy on ores_iam_seed_profile_parameters_tbl
for insert with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_parameter_update_policy on ores_iam_seed_profile_parameters_tbl
for update using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy seed_profile_parameter_delete_policy on ores_iam_seed_profile_parameters_tbl
for delete using (
    tenant_id = ores_iam_current_tenant_id_fn()
);
