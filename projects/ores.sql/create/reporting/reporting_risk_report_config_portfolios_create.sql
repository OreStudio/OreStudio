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
 * Risk Report Config Portfolio Scope Table
 *
 * Associates a risk report configuration with the portfolios it should include.
 * An empty set (no rows for a given risk_report_config_id) means all portfolios
 * visible to the tenant are included.
 *
 * This is a temporal junction table: entries are closed when removed, preserving
 * historical scope records alongside the config version history.
 */

create table if not exists "ores_reporting_risk_report_config_portfolios_tbl" (
    "tenant_id"             uuid not null,
    "risk_report_config_id" uuid not null,
    "portfolio_id"          uuid not null,
    "valid_from"            timestamp with time zone not null,
    "valid_to"              timestamp with time zone not null,
    primary key (risk_report_config_id, portfolio_id, valid_from),
    exclude using gist (
        risk_report_config_id WITH =,
        portfolio_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Unique active association
create unique index if not exists ores_reporting_risk_report_config_portfolios_uniq_idx
on "ores_reporting_risk_report_config_portfolios_tbl" (tenant_id, risk_report_config_id, portfolio_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Config index
create index if not exists ores_reporting_risk_report_config_portfolios_config_idx
on "ores_reporting_risk_report_config_portfolios_tbl" (risk_report_config_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_reporting_risk_report_config_portfolios_tenant_idx
on "ores_reporting_risk_report_config_portfolios_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_risk_report_config_portfolios_insert_fn()
returns trigger as $$
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Close any existing active association for this config + portfolio pair
    update "ores_reporting_risk_report_config_portfolios_tbl"
    set valid_to = current_timestamp
    where risk_report_config_id = new.risk_report_config_id
      and portfolio_id = new.portfolio_id
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_reporting_risk_report_config_portfolios_insert_trg
before insert on "ores_reporting_risk_report_config_portfolios_tbl"
for each row execute function ores_reporting_risk_report_config_portfolios_insert_fn();

create or replace rule ores_reporting_risk_report_config_portfolios_delete_rule as
on delete to "ores_reporting_risk_report_config_portfolios_tbl" do instead
    update "ores_reporting_risk_report_config_portfolios_tbl"
    set valid_to = current_timestamp
    where risk_report_config_id = old.risk_report_config_id
      and portfolio_id = old.portfolio_id
      and valid_to = ores_utility_infinity_timestamp_fn();
