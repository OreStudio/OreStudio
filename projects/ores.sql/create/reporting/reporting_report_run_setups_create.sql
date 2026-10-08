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
 * Report Run Setup Table
 *
 * The Setup block of the ORE run document, as typed columns: the dates the run
 * works to, the paths it reads and writes, the flags it runs under, and the
 * configuration files it names. The corpus uses forty of these parameters across
 * all four hundred and sixteen shipped run documents, and asofDate, inputPath,
 * outputPath and logFile appear in every one.
 *
 * Each row is owned by exactly one report_definition (1:1, enforced by the
 * unique index on report_definition_id). It replaces the part of
 * risk_report_config that held a handful of these parameters as columns mixed in
 * with the analytic switches; the switches become rows in report_analytic, and
 * what is left of the run's configuration is here.
 *
 * Values are held as ORE spells them rather than converted, because the run
 * document is written back to ORE and the flags are not all booleans: the
 * fixing and cashflow flags are Y or N, the model flags are true or
 * false, and accrualDate may be the literal ASOF. Converting them would
 * mean inventing a canonical form ORE does not have.
 */

create table if not exists "ores_reporting_report_run_setups_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "report_definition_id" uuid not null,
    "party_id" uuid not null,
    "asof_date" text null,
    "accrual_date" text null,
    "input_path" text null,
    "input_path_market" text null,
    "input_path_portfolio" text null,
    "output_path" text null,
    "log_file" text null,
    "log_mask" integer null,
    "n_threads" integer null,
    "observation_model" text null,
    "base_currency" text null,
    "date_calendar" text null,
    "date_convention" text null,
    "fixing_cutoff" text null,
    "continue_on_error" text null,
    "build_failed_trades" text null,
    "imply_todays_fixings" text null,
    "ignore_fixing_lag" integer null,
    "include_todays_cash_flows" text null,
    "include_reference_date_events" text null,
    "lazy_market_building" text null,
    "enrich_index_fixings" text null,
    "use_analytics" text null,
    "csv_comment_report_header" text null,
    "default_mapping_to_identity" text null,
    "portfolio_recurse_into_sub_directories" text null,
    "curve_config_file" text null,
    "conventions_file" text null,
    "market_config_file" text null,
    "pricing_engines_file" text null,
    "pricing_engines_file_scenario" text null,
    "portfolio_file" text null,
    "market_data_file" text null,
    "market_data_mapping_file" text null,
    "fixing_data_file" text null,
    "fixing_data_mapping_file" text null,
    "calendar_adjustment" text null,
    "currency_configuration" text null,
    "reference_data_file" text null,
    "counterparty_file" text null,
    "script_library" text null,
    "ibor_fallback_config" text null,
    "additional_results" text null,
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
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Unique report_definition_id for active records
create unique index if not exists report_run_setups_report_definition_id_uniq_idx
on "ores_reporting_report_run_setups_tbl" (tenant_id, report_definition_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists report_run_setups_version_uniq_idx
on "ores_reporting_report_run_setups_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists report_run_setups_id_uniq_idx
on "ores_reporting_report_run_setups_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists report_run_setups_tenant_idx
on "ores_reporting_report_run_setups_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_report_run_setups_insert_fn()
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
    from "ores_reporting_report_run_setups_tbl"
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
                    'report_run_setup',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'report_run_setup',
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
        update "ores_reporting_report_run_setups_tbl"
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

create or replace trigger ores_reporting_report_run_setups_insert_trg
before insert on "ores_reporting_report_run_setups_tbl"
for each row execute function ores_reporting_report_run_setups_insert_fn();

create or replace rule ores_reporting_report_run_setups_delete_rule as
on delete to "ores_reporting_report_run_setups_tbl" do instead (
    update "ores_reporting_report_run_setups_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Report Run Setup
-- =============================================================================
alter table ores_reporting_report_run_setups_tbl enable row level security;

drop policy if exists report_run_setups_tbl_tenant_isolation_policy
    on ores_reporting_report_run_setups_tbl;

create policy report_run_setups_tbl_tenant_isolation_policy
on ores_reporting_report_run_setups_tbl
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
drop policy if exists report_run_setups_tbl_party_isolation_policy
    on ores_reporting_report_run_setups_tbl;

create policy report_run_setups_tbl_party_isolation_policy
on ores_reporting_report_run_setups_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
