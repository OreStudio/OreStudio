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
-- Drop Row-Level Security Policies for Synthetic Tables
-- =============================================================================
-- Must be dropped before the corresponding tables are dropped.

drop policy if exists folders_party_isolation_policy on "ores_synthetic_folders_tbl";
drop policy if exists folders_tenant_isolation_policy on "ores_synthetic_folders_tbl";

drop policy if exists ir_curve_template_entries_party_isolation_policy on "ores_synthetic_ir_curve_template_entries_tbl";
drop policy if exists ir_curve_template_entries_tenant_isolation_policy on "ores_synthetic_ir_curve_template_entries_tbl";

drop policy if exists ir_curve_generation_configs_party_isolation_policy on "ores_synthetic_ir_curve_generation_configs_tbl";
drop policy if exists ir_curve_generation_configs_tenant_isolation_policy on "ores_synthetic_ir_curve_generation_configs_tbl";

drop policy if exists gmm_components_party_isolation_policy on "ores_synthetic_gmm_components_tbl";
drop policy if exists gmm_components_tenant_isolation_policy on "ores_synthetic_gmm_components_tbl";

drop policy if exists fx_spot_generation_configs_party_isolation_policy on "ores_synthetic_fx_spot_generation_configs_tbl";
drop policy if exists fx_spot_generation_configs_tenant_isolation_policy on "ores_synthetic_fx_spot_generation_configs_tbl";

drop policy if exists market_data_generation_configs_party_isolation_policy on "ores_synthetic_market_data_generation_configs_tbl";
drop policy if exists market_data_generation_configs_tenant_isolation_policy on "ores_synthetic_market_data_generation_configs_tbl";

drop policy if exists yield_curve_process_types_tenant_isolation_policy on "ores_synthetic_yield_curve_process_types_tbl";
