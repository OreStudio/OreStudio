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

drop policy if exists report_definitions_tenant_isolation_policy
    on ores_reporting_report_definitions_tbl;

create policy report_definitions_tenant_isolation_policy
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
drop policy if exists report_definitions_party_isolation_policy
    on ores_reporting_report_definitions_tbl;

create policy report_definitions_party_isolation_policy
on ores_reporting_report_definitions_tbl
as restrictive
for select using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Risk Report Config Portfolios
-- -----------------------------------------------------------------------------
alter table ores_reporting_risk_report_config_portfolios_tbl enable row level security;

drop policy if exists risk_report_config_portfolios_tenant_isolation_policy
    on ores_reporting_risk_report_config_portfolios_tbl;

create policy risk_report_config_portfolios_tenant_isolation_policy
on ores_reporting_risk_report_config_portfolios_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists risk_report_config_portfolios_party_isolation_policy
    on ores_reporting_risk_report_config_portfolios_tbl;

create policy risk_report_config_portfolios_party_isolation_policy
on ores_reporting_risk_report_config_portfolios_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        where rc.tenant_id = ores_reporting_risk_report_config_portfolios_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_portfolios_tbl.risk_report_config_id
          and rc.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        where rc.tenant_id = ores_reporting_risk_report_config_portfolios_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_portfolios_tbl.risk_report_config_id
          and rc.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
);

-- -----------------------------------------------------------------------------
-- Risk Report Config Books
-- -----------------------------------------------------------------------------
alter table ores_reporting_risk_report_config_books_tbl enable row level security;

drop policy if exists risk_report_config_books_tenant_isolation_policy
    on ores_reporting_risk_report_config_books_tbl;

create policy risk_report_config_books_tenant_isolation_policy
on ores_reporting_risk_report_config_books_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

drop policy if exists risk_report_config_books_party_isolation_policy
    on ores_reporting_risk_report_config_books_tbl;

create policy risk_report_config_books_party_isolation_policy
on ores_reporting_risk_report_config_books_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        where rc.tenant_id = ores_reporting_risk_report_config_books_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_books_tbl.risk_report_config_id
          and rc.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or exists (
        select 1
        from ores_reporting_risk_report_configs_tbl rc
        where rc.tenant_id = ores_reporting_risk_report_config_books_tbl.tenant_id
          and rc.id = ores_reporting_risk_report_config_books_tbl.risk_report_config_id
          and rc.party_id = any(ores_iam_visible_party_ids_fn())
          and rc.valid_to = ores_utility_infinity_timestamp_fn()
    )
);

-- -----------------------------------------------------------------------------
-- Report Instances
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_instances_tbl enable row level security;

drop policy if exists report_instances_tenant_isolation_policy
    on ores_reporting_report_instances_tbl;

create policy report_instances_tenant_isolation_policy
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
drop policy if exists report_instances_party_isolation_policy
    on ores_reporting_report_instances_tbl;

create policy report_instances_party_isolation_policy
on ores_reporting_report_instances_tbl
as restrictive
for select using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Report Input Bundles
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_input_bundles_tbl enable row level security;

drop policy if exists report_input_bundles_tenant_isolation_policy
    on ores_reporting_report_input_bundles_tbl;

create policy report_input_bundles_tenant_isolation_policy
on ores_reporting_report_input_bundles_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Report Errors
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_errors_tbl enable row level security;

drop policy if exists report_errors_tenant_isolation_policy
    on ores_reporting_report_errors_tbl;

create policy report_errors_tenant_isolation_policy
on ores_reporting_report_errors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE — ANDed with the permissive tenant policy).
-- Same null-bypass semantics as report_instances: when no party context is
-- set, the policy passes through for compatibility with service contexts.
-- The party is the run's, carried on the error so that a member of one party
-- cannot read the failures of another party's run.
drop policy if exists report_errors_party_isolation_policy
    on ores_reporting_report_errors_tbl;

create policy report_errors_party_isolation_policy
on ores_reporting_report_errors_tbl
as restrictive
for select using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Report Error Scopes
-- System-owned vocabulary: seeded by the system tenant and read by every
-- tenant that reads a report's errors. A tenant does not copy it at
-- provisioning, and does not extend it: the codes are the engine's own group
-- names, and an error's scope has to mean the same thing in every tenant.
-- -----------------------------------------------------------------------------
alter table ores_reporting_report_error_scopes_tbl enable row level security;

drop policy if exists report_error_scopes_read_policy
    on ores_reporting_report_error_scopes_tbl;

create policy report_error_scopes_read_policy
on ores_reporting_report_error_scopes_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);

-- One policy, not a read one beside a write one: a report's errors are checked
-- against the system tenant's rows whatever tenant owns the report, so a row a
-- tenant adds here is a row no error can ever name. Restricting the write would
-- refuse a row nothing reads, which is a cost with no rule behind it.
drop policy if exists report_error_scopes_modification_policy
    on ores_reporting_report_error_scopes_tbl;

create policy report_error_scopes_modification_policy
on ores_reporting_report_error_scopes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
