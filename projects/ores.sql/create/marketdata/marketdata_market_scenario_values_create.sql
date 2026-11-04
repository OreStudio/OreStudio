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
 * Market Scenario Value Table
 *
 * The market a run generated scenarios over, one value at a time. A stress scenario
 * file and a scenario statistics file state one row for each date and scenario, and
 * one column for each risk factor the run simulated: a discount factor at a tenor, an
 * index curve point, a volatility, a spot. Each of those values is a row here.
 *
 * This is market state and not a run result: it says what the world looked like
 * under each scenario, it is what the engine priced against, and it belongs beside
 * the series and the fixings the market data store already keeps.
 */

create table if not exists "ores_marketdata_market_scenario_values_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "report_instance_id" uuid not null,
    "result_id" uuid not null,
    "source" text not null,
    "line" integer not null,
    "as_of" timestamp with time zone not null,
    "factor" text not null,
    "report_definition_id" uuid not null,
    "family" text not null,
    "scenario" text not null,
    "value" double precision not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, as_of),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists market_scenario_values_party_id_report_instance_id_result_id_source_line_as_of_factor_uniq_idx
on "ores_marketdata_market_scenario_values_tbl" (tenant_id, party_id, report_instance_id, result_id, source, line, as_of, factor)
where valid_to = ores_utility_infinity_timestamp_fn();



create or replace function ores_marketdata_market_scenario_values_insert_fn()
returns trigger as $$
begin
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    update "ores_marketdata_market_scenario_values_tbl"
    set valid_to = current_timestamp
    where tenant_id = new.tenant_id
      and party_id = new.party_id
      and report_instance_id = new.report_instance_id
      and result_id = new.result_id
      and source = new.source
      and line = new.line
      and as_of = new.as_of
      and factor = new.factor
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from := current_timestamp;
    new.valid_to   := ores_utility_infinity_timestamp_fn();
    return new;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_scenario_values_insert_trg
before insert on "ores_marketdata_market_scenario_values_tbl"
for each row execute function ores_marketdata_market_scenario_values_insert_fn();

create or replace function ores_marketdata_market_scenario_values_delete_fn()
returns trigger as $$
begin
    update "ores_marketdata_market_scenario_values_tbl"
    set valid_to = current_timestamp
    where tenant_id = old.tenant_id
      and party_id = old.party_id
      and report_instance_id = old.report_instance_id
      and result_id = old.result_id
      and source = old.source
      and line = old.line
      and as_of = old.as_of
      and factor = old.factor
      and valid_to = ores_utility_infinity_timestamp_fn();
    return null;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_marketdata_market_scenario_values_delete_trg
before delete on "ores_marketdata_market_scenario_values_tbl"
for each row execute function ores_marketdata_market_scenario_values_delete_fn();
do $$
declare
    tsdb_installed boolean;
begin
    select exists (
        select 1 from pg_extension where extname = 'timescaledb'
    ) into tsdb_installed;

    if tsdb_installed then
        raise notice 'TimescaleDB detected - creating hypertable (30 days chunks)';

        perform public.create_hypertable(
            '"ores_marketdata_market_scenario_values_tbl"',
            'as_of',
            chunk_time_interval => interval '30 days',
            if_not_exists => true
        );
    else
        raise notice 'TimescaleDB not available - using regular table (manual cleanup required)';
    end if;
end $$;

-- =============================================================================
-- Row-level security: tenant isolation for Market Scenario Value
-- =============================================================================
alter table ores_marketdata_market_scenario_values_tbl enable row level security;

drop policy if exists market_scenario_values_tbl_tenant_isolation_policy
    on ores_marketdata_market_scenario_values_tbl;

create policy market_scenario_values_tbl_tenant_isolation_policy
on ores_marketdata_market_scenario_values_tbl
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
drop policy if exists market_scenario_values_tbl_party_isolation_policy
    on ores_marketdata_market_scenario_values_tbl;

create policy market_scenario_values_tbl_party_isolation_policy
on ores_marketdata_market_scenario_values_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
