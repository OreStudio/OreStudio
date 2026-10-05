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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Portfolio Right Table
 *
 * A portfolio right says that one account holds one named right at one node
 * of the [[id:282C87C1-11E1-42F0-BEE6-D6983A5F836B][portfolio]] tree. A right held at a node applies to every node
 * below it, so a right granted on a desk covers its sub-desks and not its
 * sibling desks.
 *
 * Two rights exist, the ones [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][sandboxes]] need: read, to see what a
 * node holds, and open_sandbox, to open a sandbox anchored at the node.
 * ores_refdata_account_holds_portfolio_right_fn answers whether an account
 * holds a right at a node, directly or through an ancestor. It runs with the
 * caller's rights, so it answers only within the session's tenant, and a
 * closed account holds no right. A right granted to an account stays on
 * record after the account closes; the function stops honouring it.
 */

create table if not exists "ores_refdata_portfolio_rights_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "account_id" uuid not null,
    "portfolio_id" uuid not null,
    "right_code" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("right_code" in ('read', 'open_sandbox'))
);

-- Composite natural key: unique combination for active records
create unique index if not exists portfolio_rights_account_id_portfolio_id_right_code_uniq_idx
on "ores_refdata_portfolio_rights_tbl" (tenant_id, account_id, portfolio_id, right_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists portfolio_rights_version_uniq_idx
on "ores_refdata_portfolio_rights_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists portfolio_rights_id_uniq_idx
on "ores_refdata_portfolio_rights_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists portfolio_rights_tenant_idx
on "ores_refdata_portfolio_rights_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_portfolio_rights_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate account_id (soft FK to ores_iam_accounts_tbl)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid account_id: %. No account found with this id.', NEW.account_id
            using errcode = '23503';
    end if;

    -- Validate portfolio_id (soft FK to ores_refdata_portfolios_tbl)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid portfolio_id: %. No active portfolio found with this id.', NEW.portfolio_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_portfolio_rights_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_portfolio_rights_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_portfolio_rights_insert_trg
before insert on "ores_refdata_portfolio_rights_tbl"
for each row execute function ores_refdata_portfolio_rights_insert_fn();

create or replace rule ores_refdata_portfolio_rights_delete_rule as
on delete to "ores_refdata_portfolio_rights_tbl" do instead (
    update "ores_refdata_portfolio_rights_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
-- Whether an account is live. Security definer and narrow: the services that
-- check rights may not read the accounts table, and this answers one yes or
-- no about one account id.
create or replace function ores_refdata_account_is_live_fn(
    p_account_id uuid
) returns boolean as $$
begin
    return exists (
        select 1 from ores_iam_accounts_tbl
        where id = p_account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether an account holds a right at a portfolio node, directly or through
-- an ancestor. The function runs with the caller's rights, so row-level
-- security confines it to the session's tenant whatever tenant is passed.
-- A closed account holds nothing. Portfolio inserts refuse cycles; the depth
-- bound only keeps a corrupt tree from looping.
create or replace function ores_refdata_account_holds_portfolio_right_fn(
    p_tenant_id uuid,
    p_account_id uuid,
    p_portfolio_id uuid,
    p_right_code text
) returns boolean as $$
begin
    return exists (
    with recursive ancestry as (
        select id, parent_portfolio_id, 0 as depth
        from ores_refdata_portfolios_tbl
        where tenant_id = p_tenant_id
          and id = p_portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select p.id, p.parent_portfolio_id, a.depth + 1
        from ores_refdata_portfolios_tbl p
        join ancestry a on p.id = a.parent_portfolio_id
        where p.tenant_id = p_tenant_id
          and p.valid_to = ores_utility_infinity_timestamp_fn()
          and a.depth < 64
    )
        select 1
        from ores_refdata_portfolio_rights_tbl r
        join ancestry a on r.portfolio_id = a.id
        where r.tenant_id = p_tenant_id
          and r.account_id = p_account_id
          and r.right_code = p_right_code
          and r.valid_to = ores_utility_infinity_timestamp_fn()
    ) and ores_refdata_account_is_live_fn(p_account_id);
end;
$$ language plpgsql stable set search_path = public, pg_temp;

-- =============================================================================
-- Row-level security: tenant isolation for Portfolio Right
-- =============================================================================
alter table ores_refdata_portfolio_rights_tbl enable row level security;

drop policy if exists portfolio_rights_tbl_tenant_isolation_policy
    on ores_refdata_portfolio_rights_tbl;

create policy portfolio_rights_tbl_tenant_isolation_policy
on ores_refdata_portfolio_rights_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
