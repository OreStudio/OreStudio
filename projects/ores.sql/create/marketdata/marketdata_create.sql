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

-- Series must be created before observations and fixings (trigger references it).
\ir ./marketdata_series_create.sql
\ir ./marketdata_observations_create.sql
\ir ./marketdata_fixings_create.sql

-- Codegen market data entities (market_series must precede observations/fixings).
\ir ./marketdata_market_series_create.sql
\ir ./marketdata_market_series_notify_trigger_create.sql
\ir ./marketdata_market_observations_create.sql
\ir ./marketdata_market_observations_notify_trigger_create.sql
\ir ./marketdata_market_fixings_create.sql
\ir ./marketdata_market_fixings_notify_trigger_create.sql
\ir ./marketdata_feed_bindings_create.sql
\ir ./marketdata_feed_bindings_notify_trigger_create.sql

-- CRM config (Cross-rates matrix story): topology_configs must precede
-- driver_pairs/enabled_derived_pairs (foreign key references it).
\ir ./marketdata_crm_topology_configs_create.sql
\ir ./marketdata_crm_topology_configs_notify_trigger_create.sql
\ir ./marketdata_crm_driver_pairs_create.sql
\ir ./marketdata_crm_driver_pairs_notify_trigger_create.sql
\ir ./marketdata_crm_enabled_derived_pairs_create.sql
\ir ./marketdata_crm_enabled_derived_pairs_notify_trigger_create.sql

\ir ./marketdata_publish_from_dq_create.sql

-- Tenor reference data: anchors and the tenor catalog before conventions;
-- conventions before the resolution junction that joins them.
\ir ./marketdata_tenor_anchors_create.sql
\ir ./marketdata_tenor_anchors_notify_trigger_create.sql
\ir ./marketdata_tenors_create.sql
\ir ./marketdata_tenors_notify_trigger_create.sql
\ir ./marketdata_tenor_conventions_create.sql
\ir ./marketdata_tenor_conventions_notify_trigger_create.sql
\ir ./marketdata_tenor_convention_resolution_create.sql
