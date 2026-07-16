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
-- Each synthetic generation config (and its sub-configs) is scoped to a tenant
-- and an owning party. Tenant isolation is the standard permissive policy;
-- party isolation is a RESTRICTIVE policy ANDed on top. When no party context
-- is set (visible_party_ids is NULL) the party policy passes through, matching
-- the established refdata pattern.

-- -----------------------------------------------------------------------------
-- Market data generation configs (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_market_data_generation_configs_tbl enable row level security;

create policy market_data_generation_configs_tenant_isolation_policy
on ores_synthetic_market_data_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy market_data_generation_configs_party_isolation_policy
on ores_synthetic_market_data_generation_configs_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- FX spot generation configs (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_fx_spot_generation_configs_tbl enable row level security;

create policy fx_spot_generation_configs_tenant_isolation_policy
on ores_synthetic_fx_spot_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy fx_spot_generation_configs_party_isolation_policy
on ores_synthetic_fx_spot_generation_configs_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- GMM components (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_gmm_components_tbl enable row level security;

create policy gmm_components_tenant_isolation_policy
on ores_synthetic_gmm_components_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy gmm_components_party_isolation_policy
on ores_synthetic_gmm_components_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- IR curve generation configs (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_ir_curve_generation_configs_tbl enable row level security;

create policy ir_curve_generation_configs_tenant_isolation_policy
on ores_synthetic_ir_curve_generation_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ir_curve_generation_configs_party_isolation_policy
on ores_synthetic_ir_curve_generation_configs_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- IR curve template entries (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_ir_curve_template_entries_tbl enable row level security;

create policy ir_curve_template_entries_tenant_isolation_policy
on ores_synthetic_ir_curve_template_entries_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy ir_curve_template_entries_party_isolation_policy
on ores_synthetic_ir_curve_template_entries_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);

-- -----------------------------------------------------------------------------
-- Folders (dual RLS: tenant + party isolation)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_folders_tbl enable row level security;

create policy folders_tenant_isolation_policy
on ores_synthetic_folders_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

create policy folders_party_isolation_policy
on ores_synthetic_folders_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
