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

-- A NULL tenant_id/party_id is a wider-scope row (system/tenant scope --
-- see the scope column), not an unscoped or orphaned one: it must stay
-- visible to every session in that wider radius, not be filtered out by
-- an equality/ANY() check that a NULL can never satisfy. That widening
-- applies to USING (visibility/reads) only -- WITH CHECK (writes) stays
-- strict, or any session could elevate a row's scope beyond its own
-- isolation boundary just by omitting the narrower identifier. Only a
-- system-tenant-context caller may write an actual null-tenant row;
-- mirrors scheduler_rls_policies_create.sql's job_instances_write_policy.
create policy market_data_generation_configs_tenant_isolation_policy
on ores_synthetic_market_data_generation_configs_tbl
for all using (
    tenant_id is null
    or tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    or ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);

-- Same USING/WITH CHECK split as above: a party-restricted session can
-- READ a null-party_id (tenant-scope) row, but cannot WRITE one -- only
-- a session with no party restriction (ores_iam_visible_party_ids_fn()
-- is null, e.g. a tenant admin) can create/update a tenant-scope row.
create policy market_data_generation_configs_party_isolation_policy
on ores_synthetic_market_data_generation_configs_tbl
as restrictive
for all using (
    party_id is null
    or party_id = ores_iam_current_party_id_fn()
)
with check (
    party_id is null
    or party_id = ores_iam_current_party_id_fn()
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

-- A sub-config's own party_id is irrelevant once its parent container is
-- tenant/system-scoped -- the container's scope, not this row's party_id,
-- is what makes generated data shared across a tenant's parties (see the
-- "Synthetic data scope and binding" story). Read-side only, matching the
-- container's own USING/WITH CHECK split: writes still require actual
-- party membership (or an unrestricted session).
create policy fx_spot_generation_configs_party_isolation_policy
on ores_synthetic_fx_spot_generation_configs_tbl
as restrictive
for all using (
    party_id = ores_iam_current_party_id_fn()
    or exists (
        select 1 from ores_synthetic_market_data_generation_configs_tbl c
        where c.id = config_id
          and c.scope in ('tenant', 'system')
          and c.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    party_id = ores_iam_current_party_id_fn()
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

-- Same widening as fx_spot_generation_configs above, one hop further:
-- gmm_component -> fx_spot_generation_config -> market_data_generation_config.
create policy gmm_components_party_isolation_policy
on ores_synthetic_gmm_components_tbl
as restrictive
for all using (
    party_id = ores_iam_current_party_id_fn()
    or exists (
        select 1 from ores_synthetic_fx_spot_generation_configs_tbl f
        join ores_synthetic_market_data_generation_configs_tbl c on c.id = f.config_id
        where f.id = fx_spot_config_id
          and c.scope in ('tenant', 'system')
          and f.valid_to = ores_utility_infinity_timestamp_fn()
          and c.valid_to = ores_utility_infinity_timestamp_fn()
    )
)
with check (
    party_id = ores_iam_current_party_id_fn()
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
    party_id = ores_iam_current_party_id_fn()
)
with check (
    party_id = ores_iam_current_party_id_fn()
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
    party_id = ores_iam_current_party_id_fn()
)
with check (
    party_id = ores_iam_current_party_id_fn()
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
    party_id is null
    or party_id = ores_iam_current_party_id_fn()
)
with check (
    party_id is null
    or party_id = ores_iam_current_party_id_fn()
);

alter table ores_synthetic_yield_curve_process_types_tbl enable row level security;

create policy yield_curve_process_types_tenant_isolation_policy
on ores_synthetic_yield_curve_process_types_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Yield curve process parameter definitions (system-tenant reference data;
-- tenant isolation only, same shape as yield_curve_process_types)
-- -----------------------------------------------------------------------------
alter table ores_synthetic_process_parameter_definitions_tbl enable row level security;

create policy yield_curve_process_parameter_definitions_tenant_isolation_policy
on ores_synthetic_process_parameter_definitions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- IR curve generation config process parameter values (dual RLS: tenant +
-- party isolation). The row carries no party_id of its own -- its scope
-- comes from its parent config, exactly as the config's own party policy
-- works: reads and writes require the parent config to be visible to /
-- owned by the current party.
-- -----------------------------------------------------------------------------
alter table ores_synthetic_config_process_parameter_values_tbl enable row level security;

create policy ir_curve_generation_config_process_parameter_values_tenant_isolation_policy
on ores_synthetic_config_process_parameter_values_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Note: the parent config table has its own config_id column (the
-- market-data generation config that produced it), so the outer
-- reference must be table-qualified -- an unqualified config_id binds
-- to the inner table and turns the predicate into c.id = c.config_id
-- (always false), hiding every value row under party isolation.
create policy ir_curve_generation_config_process_parameter_values_party_isolation_policy
on ores_synthetic_config_process_parameter_values_tbl
as restrictive
for all using (
    exists (
        select 1 from ores_synthetic_ir_curve_generation_configs_tbl c
        where c.id = ores_synthetic_config_process_parameter_values_tbl.config_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
          and c.party_id = ores_iam_current_party_id_fn()
    )
)
with check (
    exists (
        select 1 from ores_synthetic_ir_curve_generation_configs_tbl c
        where c.id = ores_synthetic_config_process_parameter_values_tbl.config_id
          and c.valid_to = ores_utility_infinity_timestamp_fn()
          and c.party_id = ores_iam_current_party_id_fn()
    )
);
