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
 * Cross-Currency Basis Convention Table
 *
 * Describes how ORE builds both legs of a cross-currency basis swap: the two
 * indices, their tenors, their payment lags and their fixing and cutoff rules, and
 * whether the spread is included in a leg's coupons. Corresponds to the
 * <CrossCurrencyBasis> element in ORE conventions.xml. The id field is the natural
 * key (ORE <Id> element).
 */

create table if not exists "ores_refdata_cross_currency_basis_conventions_tbl" (
    "id" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "settlement_days" integer not null,
    "settlement_calendar" text null,
    "roll_convention" text not null,
    "flat_index" text not null,
    "spread_index" text not null,
    "eom" boolean null,
    "is_resettable" boolean null,
    "flat_index_is_resettable" boolean null,
    "flat_tenor" text null,
    "spread_tenor" text null,
    "spread_payment_lag" integer null,
    "flat_payment_lag" integer null,
    "spread_include_spread" boolean null,
    "spread_lookback" text null,
    "spread_fixing_days" integer null,
    "spread_rate_cutoff" integer null,
    "spread_is_averaged" boolean null,
    "spread_observation_shift" boolean null,
    "flat_include_spread" boolean null,
    "flat_lookback" text null,
    "flat_fixing_days" integer null,
    "flat_rate_cutoff" integer null,
    "flat_is_averaged" boolean null,
    "flat_observation_shift" boolean null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, party_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        party_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists cross_currency_basis_conventions_version_uniq_idx
on "ores_refdata_cross_currency_basis_conventions_tbl" (tenant_id, id, party_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cross_currency_basis_conventions_id_uniq_idx
on "ores_refdata_cross_currency_basis_conventions_tbl" (tenant_id, id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cross_currency_basis_conventions_tenant_idx
on "ores_refdata_cross_currency_basis_conventions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_cross_currency_basis_conventions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_cross_currency_basis_conventions_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id and party_id = NEW.party_id
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
        update "ores_refdata_cross_currency_basis_conventions_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id and party_id = NEW.party_id
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

create or replace trigger ores_refdata_cross_currency_basis_conventions_insert_trg
before insert on "ores_refdata_cross_currency_basis_conventions_tbl"
for each row execute function ores_refdata_cross_currency_basis_conventions_insert_fn();

create or replace rule ores_refdata_cross_currency_basis_conventions_delete_rule as
on delete to "ores_refdata_cross_currency_basis_conventions_tbl" do instead (
    update "ores_refdata_cross_currency_basis_conventions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id and party_id = OLD.party_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Cross-Currency Basis Convention
-- =============================================================================
alter table ores_refdata_cross_currency_basis_conventions_tbl enable row level security;

drop policy if exists cross_currency_basis_conventions_tbl_tenant_isolation_policy
    on ores_refdata_cross_currency_basis_conventions_tbl;

create policy cross_currency_basis_conventions_tbl_tenant_isolation_policy
on ores_refdata_cross_currency_basis_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists cross_currency_basis_conventions_tbl_party_isolation_policy
    on ores_refdata_cross_currency_basis_conventions_tbl;

create policy cross_currency_basis_conventions_tbl_party_isolation_policy
on ores_refdata_cross_currency_basis_conventions_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
