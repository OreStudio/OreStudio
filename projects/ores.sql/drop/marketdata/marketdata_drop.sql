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

-- Drop observations and fixings before series (they reference it).
-- CRM config: driver_pairs/enabled_derived_pairs before topology_configs
-- (foreign key references it).
\ir ./marketdata_crm_enabled_derived_pairs_notify_trigger_drop.sql
\ir ./marketdata_crm_enabled_derived_pairs_drop.sql
\ir ./marketdata_crm_driver_pairs_notify_trigger_drop.sql
\ir ./marketdata_crm_driver_pairs_drop.sql
\ir ./marketdata_crm_topology_configs_notify_trigger_drop.sql
\ir ./marketdata_crm_topology_configs_drop.sql
\ir ./marketdata_feed_bindings_notify_trigger_drop.sql
\ir ./marketdata_feed_bindings_drop.sql
\ir ./marketdata_market_fixings_notify_trigger_drop.sql
\ir ./marketdata_market_fixings_drop.sql
\ir ./marketdata_market_observations_notify_trigger_drop.sql
\ir ./marketdata_market_observations_drop.sql
\ir ./marketdata_market_series_notify_trigger_drop.sql
\ir ./marketdata_market_series_drop.sql
\ir ./marketdata_fixings_drop.sql
\ir ./marketdata_observations_drop.sql
\ir ./marketdata_series_drop.sql
