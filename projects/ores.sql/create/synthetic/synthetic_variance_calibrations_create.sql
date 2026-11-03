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
 * Variance Calibration Table
 *
 * One GJR-GARCH shape of a pair's daily variance and the evidence a reviewer judges
 * it by. A fitted calibration names the variance_calibration_input it was fitted
 * from and the version of the method that fitted it; an authored one names no input
 * and is reviewed like any other. A feed runs only a calibration that is approved,
 * and names it. See "A calibration is stored and reviewed before a feed runs it" in
 * [[id:351CB816-28F5-428A-A289-414B6F4B9F36][Volatility clustering and GARCH models]].
 *
 * A calibration is proposed when it is stored. A person approves or rejects it and
 * states why in state_reason; approving one makes the pair's earlier approval
 * superseded in the same change, so one approved calibration of a pair is current.
 * Each decision is a new version of the row, so its history shows who decided what
 * and when. A fit that could not be made, for too few returns or a shape at the
 * stationarity bound, is stored as rejected with the reason, so the attempt is kept;
 * such a row may have no shape, so the shape is required, and admissible, in the
 * other three states only. Each bound is written so that a missing value or NaN
 * fails it: a check passes on null, and PostgreSQL orders NaN above every number. A
 * value that is present is finite in every state, rejected included, so the audit
 * trail holds no NaN. The evidence of a fit agrees with its method: an authored shape
 * has no starts, returns or likelihood, and a fit that was not rejected has all three.
 *
 * The checks do not tie persistence to the three parameters, because p_minus is
 * not stored; the fit that writes a calibration owns that equality.
 *
 * persistence is alpha + gamma p_minus + beta, with p_minus the share of the
 * daily return's variance that falls on negative days, so it is stored rather than
 * recomputed from the three parameters alone.
 */

create table if not exists "ores_synthetic_variance_calibrations_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "input_id" uuid null,
    "method" text not null,
    "method_version" integer not null,
    "alpha" double precision null,
    "gamma" double precision null,
    "beta" double precision null,
    "persistence" double precision null,
    "long_run_variance" double precision null,
    "starting_variance_ratio" double precision null,
    "log_likelihood" double precision null,
    "returns_used" integer not null,
    "starts" integer not null,
    "starts_agreed" boolean not null,
    "state" text not null,
    "state_reason" text not null default '',
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
    check ("base_currency_code" <> "quote_currency_code"),
    check ("method" in ('gjr_garch_qmle', 'authored')),
    check (("method" = 'authored') = ("input_id" is null)),
    check ("method_version" > 0),
    check ("state" in ('proposed', 'approved', 'rejected', 'superseded')),
    check ("state" not in ('approved', 'rejected', 'superseded') or "state_reason" <> ''),
    check ("returns_used" >= 0 and "starts" >= 0 and (not "starts_agreed" or "starts" > 0)),
    check ("method" <> 'authored' or ("starts" = 0 and "returns_used" = 0 and "log_likelihood" is null)),
    check ("method" = 'authored' or "state" = 'rejected' or ("starts" > 0 and "returns_used" > 0 and "log_likelihood" is not null)),
    check (coalesce("alpha", 0) < 'Infinity'::double precision and coalesce("gamma", 0) < 'Infinity'::double precision and coalesce("beta", 0) < 'Infinity'::double precision and coalesce("persistence", 0) < 'Infinity'::double precision and coalesce("long_run_variance", 0) < 'Infinity'::double precision and coalesce("starting_variance_ratio", 0) < 'Infinity'::double precision and coalesce("log_likelihood", 0) < 'Infinity'::double precision),
    check ("state" = 'rejected' or coalesce("alpha" >= 0 and "alpha" < 1 and "beta" >= 0 and "beta" < 1 and "alpha" + "gamma" >= 0 and "alpha" + "gamma" < 'Infinity'::double precision, false)),
    check ("state" = 'rejected' or coalesce("persistence" >= 0 and "persistence" < 1, false)),
    check ("state" = 'rejected' or coalesce("long_run_variance" > 0 and "long_run_variance" < 'Infinity'::double precision, false)),
    check ("state" = 'rejected' or coalesce("starting_variance_ratio" > 0 and "starting_variance_ratio" < 'Infinity'::double precision, false))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists variance_calibrations_version_uniq_idx
on "ores_synthetic_variance_calibrations_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists variance_calibrations_id_uniq_idx
on "ores_synthetic_variance_calibrations_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists variance_calibrations_tenant_idx
on "ores_synthetic_variance_calibrations_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists variance_calibrations_approved_pair_idx
on "ores_synthetic_variance_calibrations_tbl" (tenant_id, party_id, base_currency_code, quote_currency_code)
where valid_to = ores_utility_infinity_timestamp_fn()
  and state = 'approved';

create or replace function ores_synthetic_variance_calibrations_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate input_id (optional soft FK to ores_synthetic_variance_calibration_inputs_tbl)
    if NEW.input_id is not null then
        if not exists (
            select 1 from ores_synthetic_variance_calibration_inputs_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.input_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid input_id: %. No active variance_calibration_input found with this id.', NEW.input_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate base_currency_code
    NEW.base_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.base_currency_code);

    -- Validate quote_currency_code
    NEW.quote_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.quote_currency_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_variance_calibrations_tbl"
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
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'variance_calibration',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'variance_calibration',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_synthetic_variance_calibrations_tbl"
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

create or replace trigger ores_synthetic_variance_calibrations_insert_trg
before insert on "ores_synthetic_variance_calibrations_tbl"
for each row execute function ores_synthetic_variance_calibrations_insert_fn();

create or replace rule ores_synthetic_variance_calibrations_delete_rule as
on delete to "ores_synthetic_variance_calibrations_tbl" do instead (
    update "ores_synthetic_variance_calibrations_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);
