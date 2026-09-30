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
 * family references. The columns map from bondReferenceDatum
 * (referencedata.xsd lines 97-113): security_id, issuer, the three curve
 * identifiers, the six fields beside them, issue date, settlement days and
 * calendar. Only IssuerId is required by that schema; every other
 * element is optional, so every other column is nullable and an absent
 * element is stored as NULL rather than as an empty string or a zero.
 *
 * The bond's coupon terms are not here. ORE states them on legData
 * alone -- bondReferenceDatum carries no coupon rate, coupon frequency,
 * day counter or currency -- and the security's legs are
 * bond_issue_leg and its children, so the leg holds them once and this
 * row is not a second copy.
 *
 * face_value is the issue's per-unit value and repeats the first leg's
 * notional. It stays because a row set that holds an issue and no legs
 * still has to export a leg, and it is what that leg's notional is built
 * from.
 *
 * Two columns come from the trade's own copy of the datum and not from
 * bondReferenceDatum: payer and credit_risk are bondData's top-level
 * members (instruments.xsd lines 404 and 406), which the reference datum
 * does not state. They live here because bondData is the trade's copy of
 * the issue's terms and every other member of it is already mapped to this
 * row, so leaving the two out would drop them from the round trip.
 */

create table if not exists "ores_trading_bond_issues_tbl" (
    "issue_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "security_id" text not null,
    "issuer" text null,
    "face_value" numeric(28, 10) null,
    "issue_date" date null,
    "settlement_days" integer null,
    "calendar" text null,
    "credit_curve_id" text null,
    "reference_curve_id" text null,
    "income_curve_id" text null,
    "credit_group" text null,
    "volatility_curve_id" text null,
    "price_quote_method" text null,
    "price_quote_base_value" text null,
    "sub_type" text null,
    "price_type" text null,
    "payer" text null,
    "credit_risk" text null,
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
    check ("face_value" > 0),
    check ("price_type" is null or "price_type" in ('Clean', 'Dirty'))
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

-- The rows this row owns, closed when it is deleted. A function and not a
-- rule body, because the rule resolves the tables it names when it is
-- created and a child may be created after its parent; a plpgsql body
-- resolves them when it runs.
create or replace function ores_trading_bond_issues_cascade_delete_fn(
    p_row "ores_trading_bond_issues_tbl")
returns void as $$
begin
    -- Close every row this row owns, so one delete removes the family and
    -- not the header alone. The store enforces it, so every caller gets it.
    delete from "ores_trading_bond_issue_legs_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_leg_amounts_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_leg_rates_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_leg_amortizations_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_leg_schedules_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_leg_schedule_dates_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_call_dates_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
    delete from "ores_trading_bond_issue_conversion_targets_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace rule ores_trading_bond_issues_delete_rule as
on delete to "ores_trading_bond_issues_tbl" do instead (
    select ores_trading_bond_issues_cascade_delete_fn(OLD);
    update "ores_trading_bond_issues_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and issue_id = OLD.issue_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
