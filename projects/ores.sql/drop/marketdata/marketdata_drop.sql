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

-- Drop observations, fixings and the asset-class junction before series
-- (they reference it).
\ir ./marketdata_subscription_intervals_notify_trigger_drop.sql
\ir ./marketdata_subscription_intervals_drop.sql
\ir ./marketdata_demand_intervals_notify_trigger_drop.sql
\ir ./marketdata_demand_intervals_drop.sql
\ir ./marketdata_delivery_counts_notify_trigger_drop.sql
\ir ./marketdata_delivery_counts_drop.sql
\ir ./marketdata_rate_card_components_notify_trigger_drop.sql
\ir ./marketdata_rate_card_components_drop.sql
\ir ./marketdata_routes_notify_trigger_drop.sql
\ir ./marketdata_routes_drop.sql
\ir ./marketdata_subscriptions_notify_trigger_drop.sql
\ir ./marketdata_subscriptions_drop.sql
\ir ./marketdata_market_data_mappings_notify_trigger_drop.sql
\ir ./marketdata_market_data_mappings_drop.sql
\ir ./marketdata_market_data_vendor_instruments_notify_trigger_drop.sql
\ir ./marketdata_market_data_vendor_instruments_drop.sql
\ir ./marketdata_market_data_sources_notify_trigger_drop.sql
\ir ./marketdata_market_data_sources_drop.sql
\ir ./marketdata_providers_notify_trigger_drop.sql
\ir ./marketdata_providers_drop.sql
\ir ./marketdata_feed_bindings_notify_trigger_drop.sql
\ir ./marketdata_feed_bindings_drop.sql
\ir ./marketdata_market_fixings_notify_trigger_drop.sql
\ir ./marketdata_market_fixings_drop.sql
\ir ./marketdata_observation_lineages_notify_trigger_drop.sql
\ir ./marketdata_observation_lineages_drop.sql
\ir ./marketdata_feed_binding_activity_drop.sql
\ir ./marketdata_tick_coords_drop.sql
\ir ./marketdata_ticks_drop.sql
\ir ./marketdata_market_observations_drop.sql
\ir ./marketdata_series_identity_views_drop.sql
\ir ./marketdata_market_series_identity_drop.sql
\ir ./marketdata_series_axis_values_drop.sql
\ir ./marketdata_series_axes_drop.sql
\ir ./marketdata_market_series_asset_class_drop.sql
\ir ./marketdata_market_series_notify_trigger_drop.sql
\ir ./marketdata_market_series_drop.sql
\ir ./marketdata_validate_curve_series_fn_drop.sql
\ir ./marketdata_curve_series_drop.sql
\ir ./marketdata_series_classification_rules_notify_trigger_drop.sql
\ir ./marketdata_series_classification_rules_drop.sql
