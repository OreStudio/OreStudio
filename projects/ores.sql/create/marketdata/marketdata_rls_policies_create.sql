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
-- Row-Level Security Policies for Market Data Tables
-- =============================================================================
-- These policies enforce strict tenant isolation. Each tenant can only see
-- and modify their own series, observations, and fixings.

-- -----------------------------------------------------------------------------
-- Series
-- -----------------------------------------------------------------------------
alter table ores_marketdata_series_tbl enable row level security;

create policy series_tbl_tenant_isolation_policy
on ores_marketdata_series_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Observations
-- -----------------------------------------------------------------------------
alter table ores_marketdata_observations_tbl enable row level security;

create policy observations_tbl_tenant_isolation_policy
on ores_marketdata_observations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Fixings
-- -----------------------------------------------------------------------------
alter table ores_marketdata_fixings_tbl enable row level security;

create policy fixings_tbl_tenant_isolation_policy
on ores_marketdata_fixings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Market Series (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_market_series_tbl enable row level security;

create policy market_series_tbl_tenant_isolation_policy
on ores_marketdata_market_series_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Market Observations (codegen-generated hypertable)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_market_observations_tbl enable row level security;

create policy market_observations_tbl_tenant_isolation_policy
on ores_marketdata_market_observations_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Market Fixings (codegen-generated hypertable)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_market_fixings_tbl enable row level security;

create policy market_fixings_tbl_tenant_isolation_policy
on ores_marketdata_market_fixings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Feed bindings
-- -----------------------------------------------------------------------------
alter table ores_marketdata_feed_bindings_tbl enable row level security;

create policy feed_bindings_tbl_tenant_isolation_policy
on ores_marketdata_feed_bindings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
    OR ores_iam_current_tenant_id_fn() = ores_utility_system_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CRM topology configs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_crm_topology_configs_tbl enable row level security;

create policy crm_topology_configs_tbl_tenant_isolation_policy
on ores_marketdata_crm_topology_configs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CRM driver pairs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_crm_driver_pairs_tbl enable row level security;

create policy crm_driver_pairs_tbl_tenant_isolation_policy
on ores_marketdata_crm_driver_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- CRM enabled derived pairs (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_crm_enabled_derived_pairs_tbl enable row level security;

create policy crm_enabled_derived_pairs_tbl_tenant_isolation_policy
on ores_marketdata_crm_enabled_derived_pairs_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Anchors (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_tenor_anchors_tbl enable row level security;

create policy tenor_anchors_tbl_tenant_isolation_policy
on ores_marketdata_tenor_anchors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenors (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_tenors_tbl enable row level security;

create policy tenors_tbl_tenant_isolation_policy
on ores_marketdata_tenors_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Conventions (codegen-generated table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_tenor_conventions_tbl enable row level security;

create policy tenor_conventions_tbl_tenant_isolation_policy
on ores_marketdata_tenor_conventions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- -----------------------------------------------------------------------------
-- Tenor Convention Resolutions (codegen-generated junction table)
-- -----------------------------------------------------------------------------
alter table ores_marketdata_tenor_convention_resolutions_tbl enable row level security;

create policy tenor_convention_resolutions_tbl_tenant_isolation_policy
on ores_marketdata_tenor_convention_resolutions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
