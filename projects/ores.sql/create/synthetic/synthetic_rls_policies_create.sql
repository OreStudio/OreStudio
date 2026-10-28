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
-- Row-Level Security Policies for Synthetic Tables
-- =============================================================================
-- The generators belong to the system tenant and the system party. Every tenant
-- reads them, because a simulated feed stands for one observation in the world
-- and every party must see the same value. A session writes only its own
-- tenant's rows, so only the system tenant writes the installation's generators.
-- See doc/knowledge/domain/generator_ownership.org.
--
-- The policy is the shared reference shape: a tenant reads its own rows and the
-- system tenant's, and writes only its own. No policy reads the party, because no
-- generator belongs to a party the reader could be outside of.

-- -----------------------------------------------------------------------------
-- market_data_generation_configs
-- -----------------------------------------------------------------------------
alter table ores_synthetic_market_data_generation_configs_tbl enable row level security;

drop policy if exists market_data_generation_configs_tenant_isolation_policy
    on ores_synthetic_market_data_generation_configs_tbl;

create policy market_data_generation_configs_tenant_isolation_policy
on ores_synthetic_market_data_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- fx_spot_generation_configs
-- -----------------------------------------------------------------------------
alter table ores_synthetic_fx_spot_generation_configs_tbl enable row level security;

drop policy if exists fx_spot_generation_configs_tenant_isolation_policy
    on ores_synthetic_fx_spot_generation_configs_tbl;

create policy fx_spot_generation_configs_tenant_isolation_policy
on ores_synthetic_fx_spot_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- gmm_components
-- -----------------------------------------------------------------------------
alter table ores_synthetic_gmm_components_tbl enable row level security;

drop policy if exists gmm_components_tenant_isolation_policy
    on ores_synthetic_gmm_components_tbl;

create policy gmm_components_tenant_isolation_policy
on ores_synthetic_gmm_components_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- fx_spot_fixing_items
-- -----------------------------------------------------------------------------
alter table ores_synthetic_fx_spot_fixing_items_tbl enable row level security;

drop policy if exists fx_spot_fixing_items_tenant_isolation_policy
    on ores_synthetic_fx_spot_fixing_items_tbl;

create policy fx_spot_fixing_items_tenant_isolation_policy
on ores_synthetic_fx_spot_fixing_items_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- ir_curve_generation_configs
-- -----------------------------------------------------------------------------
alter table ores_synthetic_ir_curve_generation_configs_tbl enable row level security;

drop policy if exists ir_curve_generation_configs_tenant_isolation_policy
    on ores_synthetic_ir_curve_generation_configs_tbl;

create policy ir_curve_generation_configs_tenant_isolation_policy
on ores_synthetic_ir_curve_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- ir_curve_template_entries
-- -----------------------------------------------------------------------------
alter table ores_synthetic_ir_curve_template_entries_tbl enable row level security;

drop policy if exists ir_curve_template_entries_tenant_isolation_policy
    on ores_synthetic_ir_curve_template_entries_tbl;

create policy ir_curve_template_entries_tenant_isolation_policy
on ores_synthetic_ir_curve_template_entries_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- folders
-- -----------------------------------------------------------------------------
alter table ores_synthetic_folders_tbl enable row level security;

drop policy if exists folders_tenant_isolation_policy
    on ores_synthetic_folders_tbl;

create policy folders_tenant_isolation_policy
on ores_synthetic_folders_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- ir_curve_generation_config_process_parameter_values
-- -----------------------------------------------------------------------------
alter table ores_synthetic_config_process_parameter_values_tbl enable row level security;

drop policy if exists ir_curve_generation_config_process_parameter_values_tenant_isolation_policy
    on ores_synthetic_config_process_parameter_values_tbl;

create policy ir_curve_generation_config_process_parameter_values_tenant_isolation_policy
on ores_synthetic_config_process_parameter_values_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

alter table ores_synthetic_yield_curve_process_types_tbl enable row level security;

drop policy if exists yield_curve_process_types_tenant_isolation_policy
    on ores_synthetic_yield_curve_process_types_tbl;

create policy yield_curve_process_types_tenant_isolation_policy
on ores_synthetic_yield_curve_process_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Market data providers (the installation's catalogue). The providers are
-- published into the system tenant alone, and every tenant reads them, so the
-- policy is the shared shape: a tenant reads its own rows and the system
-- tenant's, and writes only its own. The providers are published into the
-- system tenant alone, and a role that may write a provider is the system's.
-- -----------------------------------------------------------------------------
alter table ores_synthetic_market_data_providers_tbl enable row level security;

drop policy if exists market_data_providers_tenant_isolation_policy
    on ores_synthetic_market_data_providers_tbl;

create policy market_data_providers_tenant_isolation_policy
on ores_synthetic_market_data_providers_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Yield curve process parameter definitions (system-tenant reference data;
-- tenant isolation only, same shape as yield_curve_process_types)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_process_parameter_definitions_tbl enable row level security;

drop policy if exists yield_curve_process_parameter_definitions_tenant_isolation_policy
    on ores_synthetic_process_parameter_definitions_tbl;

create policy yield_curve_process_parameter_definitions_tenant_isolation_policy
on ores_synthetic_process_parameter_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
