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

-- Party isolation (RESTRICTIVE — ANDed with the permissive tenant policy).
-- When no party context is set (visible_party_ids is NULL), the policy
-- passes through, preserving backward compatibility with service contexts
-- that set only tenant context (e.g. report populators, test helpers).
create policy ores_reporting_report_definitions_party_isolation_policy
on ores_reporting_report_definitions_tbl
as restrictive
for select using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
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

create policy ores_reporting_risk_report_configs_party_isolation_policy
on ores_reporting_risk_report_configs_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_report_definitions_tbl rd
        where rd.tenant_id = ores_reporting_risk_report_configs_tbl.tenant_id
          and rd.id = ores_reporting_risk_report_configs_tbl.report_definition_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rd.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_report_definitions_tbl rd
        where rd.tenant_id = ores_reporting_risk_report_configs_tbl.tenant_id
          and rd.id = ores_reporting_risk_report_configs_tbl.report_definition_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rd.valid_to = ores_utility_infinity_timestamp_fn()
    )
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

create policy ores_reporting_risk_report_config_portfolios_party_isolation_policy
on ores_reporting_risk_report_config_portfolios_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        join ores_reporting_report_definitions_tbl rd
          on rd.tenant_id = rc.tenant_id
         and rd.id = rc.report_definition_id
         and rd.valid_to = ores_utility_infinity_timestamp_fn()
        where rc.tenant_id = ores_reporting_risk_report_config_portfolios_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_portfolios_tbl.risk_report_config_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        join ores_reporting_report_definitions_tbl rd
          on rd.tenant_id = rc.tenant_id
         and rd.id = rc.report_definition_id
         and rd.valid_to = ores_utility_infinity_timestamp_fn()
        where rc.tenant_id = ores_reporting_risk_report_config_portfolios_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_portfolios_tbl.risk_report_config_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
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

create policy ores_reporting_risk_report_config_books_party_isolation_policy
on ores_reporting_risk_report_config_books_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        join ores_reporting_report_definitions_tbl rd
          on rd.tenant_id = rc.tenant_id
         and rd.id = rc.report_definition_id
         and rd.valid_to = ores_utility_infinity_timestamp_fn()
        where rc.tenant_id = ores_reporting_risk_report_config_books_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_books_tbl.risk_report_config_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        join ores_reporting_report_definitions_tbl rd
          on rd.tenant_id = rc.tenant_id
         and rd.id = rc.report_definition_id
         and rd.valid_to = ores_utility_infinity_timestamp_fn()
        where rc.tenant_id = ores_reporting_risk_report_config_books_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_books_tbl.risk_report_config_id
          and rd.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
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

-- Party isolation (RESTRICTIVE — ANDed with the permissive tenant policy).
-- Same null-bypass semantics as report_definitions: when no party context
-- is set, the policy passes through for compatibility with service contexts.
create policy ores_reporting_report_instances_party_isolation_policy
on ores_reporting_report_instances_tbl
as restrictive
for select using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
