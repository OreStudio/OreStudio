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
 * Bond Issue Table
 *
 * One row per bond issue (ISIN), the stable row every instrument of the
 * family references. The columns map one to one from bondData
 * (instruments.xsd lines 382-403): security_id, issuer, currency,
 * face_value, coupon_rate, coupon_frequency_code, day_count_code,
 * issue_date, maturity_date, settlement_days and the free description;
 * the coupon terms come from the coupon leg of the issue's LegData.
 */

create table if not exists "ores_trading_bond_issues_tbl" (
    "issue_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "security_id" text not null,
    "issuer" text not null,
    "currency" text not null,
    "face_value" numeric(28, 10) not null,
    "coupon_rate" numeric(28, 10) not null,
    "coupon_frequency_code" text not null,
    "day_count_code" text not null,
    "issue_date" date not null,
    "maturity_date" date not null,
    "settlement_days" integer not null,
    "description" text null,
    "calendar" text null,
    "credit_curve_id" text null,
    "reference_curve_id" text null,
    "income_curve_id" text null,
    "bond_notional" text null,
    "workspace_id" uuid not null default ores_utility_live_workspace_id_fn(), -- soft FK to ores_workspaces_tbl(id)
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, issue_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        issue_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("issue_id" <> ores_utility_nil_uuid_fn()),
    check ("maturity_date" > "issue_date"),
    check ("face_value" > 0),
    check ("coupon_rate" >= 0),
    check ("issuer" <> ''),
    check ("currency" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_issues_version_uniq_idx
on "ores_trading_bond_issues_tbl" (tenant_id, issue_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_issues_id_uniq_idx
on "ores_trading_bond_issues_tbl" (tenant_id, issue_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_issues_tenant_idx
on "ores_trading_bond_issues_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_issues_security_idx
on "ores_trading_bond_issues_tbl" (tenant_id, security_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_issues_workspace_idx
on "ores_trading_bond_issues_tbl" (workspace_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_issues_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate workspace_id
    NEW.workspace_id := ores_workspace_validate_fn(NEW.workspace_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_bond_issues_tbl"
    where tenant_id = NEW.tenant_id
      and issue_id = NEW.issue_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
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
        update "ores_trading_bond_issues_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and issue_id = NEW.issue_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_bond_issues_insert_trg
before insert on "ores_trading_bond_issues_tbl"
for each row execute function ores_trading_bond_issues_insert_fn();

create or replace rule ores_trading_bond_issues_delete_rule as
on delete to "ores_trading_bond_issues_tbl" do instead (
    update "ores_trading_bond_issues_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and issue_id = OLD.issue_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
