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

-- Codegen market data entities (market_series must precede observations/fixings,
-- and the asset-class junction references it).
\ir ./marketdata_market_series_create.sql
\ir ./marketdata_market_series_notify_trigger_create.sql
\ir ./marketdata_market_series_asset_class_create.sql
\ir ./marketdata_market_series_identity_create.sql
-- The curve identity a trade's reference curve names; it reads the series
-- table, which precedes it here.
\ir ./marketdata_validate_curve_series_fn_create.sql
-- The series a published curve configuration states, which is what the
-- validator above reads.
\ir ./marketdata_curve_series_create.sql
\ir ./marketdata_series_axes_create.sql
\ir ./marketdata_series_axis_values_create.sql
\ir ./marketdata_series_identity_views_create.sql
\ir ./marketdata_market_observations_create.sql
\ir ./marketdata_ticks_create.sql
\ir ./marketdata_tick_coords_create.sql
\ir ./marketdata_observation_lineages_create.sql
\ir ./marketdata_observation_lineages_notify_trigger_create.sql
-- The subject of an approved curve publish: a request's detail row, keyed by
-- its request id (the inbox table is created after marketdata, so the check
-- runs at write time).
\ir ./marketdata_curve_publishes_create.sql
\ir ./marketdata_curve_publishes_notify_trigger_create.sql
\ir ./marketdata_market_fixings_create.sql
\ir ./marketdata_market_fixings_notify_trigger_create.sql
\ir ./marketdata_series_classification_rules_create.sql
\ir ./marketdata_series_classification_rules_notify_trigger_create.sql
\ir ./marketdata_adapters_create.sql
\ir ./marketdata_adapters_notify_trigger_create.sql
\ir ./marketdata_suppliers_create.sql
\ir ./marketdata_suppliers_notify_trigger_create.sql
\ir ./marketdata_providers_create.sql
\ir ./marketdata_providers_notify_trigger_create.sql
\ir ./marketdata_feeds_create.sql
\ir ./marketdata_feeds_notify_trigger_create.sql
\ir ./marketdata_sessions_create.sql
\ir ./marketdata_sessions_notify_trigger_create.sql
\ir ./marketdata_sessions_settings_validate_trigger_create.sql
\ir ./marketdata_market_data_vendor_instruments_create.sql
\ir ./marketdata_market_data_vendor_instruments_notify_trigger_create.sql
\ir ./marketdata_market_data_mappings_create.sql
\ir ./marketdata_market_data_mappings_notify_trigger_create.sql
\ir ./marketdata_subscriptions_create.sql
\ir ./marketdata_subscriptions_notify_trigger_create.sql
\ir ./marketdata_routes_create.sql
\ir ./marketdata_routes_notify_trigger_create.sql
\ir ./marketdata_composite_inputs_create.sql
\ir ./marketdata_composite_inputs_notify_trigger_create.sql
\ir ./marketdata_composites_create.sql
\ir ./marketdata_composites_notify_trigger_create.sql
\ir ./marketdata_rate_card_components_create.sql
\ir ./marketdata_rate_card_components_notify_trigger_create.sql
\ir ./marketdata_delivery_counts_create.sql
\ir ./marketdata_delivery_counts_notify_trigger_create.sql
\ir ./marketdata_demand_intervals_create.sql
\ir ./marketdata_demand_intervals_notify_trigger_create.sql
\ir ./marketdata_subscription_intervals_create.sql
\ir ./marketdata_subscription_intervals_notify_trigger_create.sql

\ir ./marketdata_publish_from_dq_create.sql
