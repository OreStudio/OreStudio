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
-- Row-Level Security Policies for Analytics Tables
-- =============================================================================
-- Pricing engine types are a system-owned global registry: seeded by the system
-- tenant and readable by all tenants. Pricing model configs, products, and
-- parameters are strictly tenant-scoped.

-- -----------------------------------------------------------------------------
-- Pricing Engine Types (global registry — system tenant records visible to all)
-- -----------------------------------------------------------------------------
alter table ores_analytics_pricing_engine_types_tbl enable row level security;

create policy ores_analytics_pricing_engine_types_tenant_isolation_policy
on ores_analytics_pricing_engine_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_iam_system_tenant_id_fn()  -- system engine types visible to all
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Pricing Model Configs (tenant-scoped)
-- -----------------------------------------------------------------------------
alter table ores_analytics_pricing_model_configs_tbl enable row level security;

create policy ores_analytics_pricing_model_configs_tenant_isolation_policy
on ores_analytics_pricing_model_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Pricing Model Products (tenant-scoped)
-- -----------------------------------------------------------------------------
alter table ores_analytics_pricing_model_products_tbl enable row level security;

create policy ores_analytics_pricing_model_products_tenant_isolation_policy
on ores_analytics_pricing_model_products_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Pricing Model Product Parameters (tenant-scoped)
-- -----------------------------------------------------------------------------
alter table ores_analytics_pricing_model_product_parameters_tbl enable row level security;

create policy ores_analytics_pricing_model_product_parameters_tenant_isolation_policy
on ores_analytics_pricing_model_product_parameters_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
