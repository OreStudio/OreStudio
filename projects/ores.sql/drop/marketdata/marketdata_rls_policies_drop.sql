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

drop policy if exists fixings_tbl_tenant_isolation_policy on ores_marketdata_fixings_tbl;
drop policy if exists observations_tbl_tenant_isolation_policy on ores_marketdata_observations_tbl;
drop policy if exists series_tbl_tenant_isolation_policy on ores_marketdata_series_tbl;
drop policy if exists crm_topology_configs_tbl_tenant_isolation_policy on ores_marketdata_crm_topology_configs_tbl;
drop policy if exists crm_driver_pairs_tbl_tenant_isolation_policy on ores_marketdata_crm_driver_pairs_tbl;
drop policy if exists crm_enabled_derived_pairs_tbl_tenant_isolation_policy on ores_marketdata_crm_enabled_derived_pairs_tbl;
drop policy if exists tenor_anchors_tbl_tenant_isolation_policy on ores_marketdata_tenor_anchors_tbl;
drop policy if exists tenors_tbl_tenant_isolation_policy on ores_marketdata_tenors_tbl;
drop policy if exists tenor_conventions_tbl_tenant_isolation_policy on ores_marketdata_tenor_conventions_tbl;
drop policy if exists tenor_convention_resolutions_tbl_tenant_isolation_policy on ores_marketdata_tenor_convention_resolutions_tbl;
