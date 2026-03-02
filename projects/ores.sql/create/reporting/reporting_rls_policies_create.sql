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
-- Row-Level Security Policies for Reporting Tables
-- =============================================================================
-- Report definitions and instances are party-scoped. RLS enforces both tenant
-- and party isolation on all reporting tables.
-- Junction tables (portfolios, books) are scoped to their parent config's tenant.

-- -----------------------------------------------------------------------------
-- Report Definitions
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_definitions_tbl enable row level security;

create policy ores_reporting_report_definitions_tenant_isolation_policy
on ores_reporting_report_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_reporting_report_definitions_party_isolation_policy
on ores_reporting_report_definitions_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Risk Report Configs
-- -----------------------------------------------------------------------------
alter table ores_reporting_risk_report_configs_tbl enable row level security;

create policy ores_reporting_risk_report_configs_tenant_isolation_policy
on ores_reporting_risk_report_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Risk Report Config Portfolios
-- -----------------------------------------------------------------------------
alter table ores_reporting_risk_report_config_portfolios_tbl enable row level security;

create policy ores_reporting_risk_report_config_portfolios_tenant_isolation_policy
on ores_reporting_risk_report_config_portfolios_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Risk Report Config Books
-- -----------------------------------------------------------------------------
alter table ores_reporting_risk_report_config_books_tbl enable row level security;

create policy ores_reporting_risk_report_config_books_tenant_isolation_policy
on ores_reporting_risk_report_config_books_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Report Instances
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_instances_tbl enable row level security;

create policy ores_reporting_report_instances_tenant_isolation_policy
on ores_reporting_report_instances_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ores_reporting_report_instances_party_isolation_policy
on ores_reporting_report_instances_tbl
as restrictive
for select using (
    party_id = ANY(ores_iam_visible_party_ids_fn())
);
