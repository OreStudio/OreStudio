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
-- Analytics Component
-- =============================================================================
-- Creates analytics tables for pricing configuration, model selection, and
-- valuation settings. Depends on:
-- - ores.iam (for tenant validation)
-- - ores.dq (for change reason validation)
-- - ores.trading (for trade_types reference in pricing_engine_types)

-- Reference data (pricing engine type taxonomy)
\ir ./analytics_pricing_engine_types_functions_create.sql
\ir ./analytics_pricing_engine_types_create.sql
\ir ./analytics_pricing_engine_types_notify_trigger_create.sql

-- Pricing model configuration (header)
\ir ./analytics_pricing_model_configs_create.sql
\ir ./analytics_pricing_model_configs_notify_trigger_create.sql

-- Pricing model products (detail rows)
\ir ./analytics_pricing_model_products_create.sql
\ir ./analytics_pricing_model_products_notify_trigger_create.sql

-- Pricing model product parameters (normalised key-value pairs)
\ir ./analytics_pricing_model_product_parameters_create.sql
\ir ./analytics_pricing_model_product_parameters_notify_trigger_create.sql

-- Credit simulation configuration (ORE credit simulation document root)
\ir ./analytics_credit_simulation_configs_create.sql
\ir ./analytics_credit_simulation_configs_notify_trigger_create.sql
\ir ./analytics_credit_simulation_netting_set_configs_create.sql
\ir ./analytics_credit_simulation_netting_set_configs_notify_trigger_create.sql

-- Credit simulation transition matrices (named, reusable)
\ir ./analytics_credit_simulation_matrix_configs_create.sql
\ir ./analytics_credit_simulation_matrix_configs_notify_trigger_create.sql
\ir ./analytics_credit_simulation_matrix_row_configs_create.sql
\ir ./analytics_credit_simulation_matrix_row_configs_notify_trigger_create.sql

-- Credit simulation entities (one row per migrating entity)
\ir ./analytics_credit_simulation_entity_configs_create.sql
\ir ./analytics_credit_simulation_entity_configs_notify_trigger_create.sql

-- Credit simulation transition matrix cells (one row per grid cell)

-- Stress testing (the ORE stress library and its scenarios)
\ir ./analytics_stress_test_libraries_create.sql
\ir ./analytics_stress_test_libraries_notify_trigger_create.sql
\ir ./analytics_stress_test_scenarios_create.sql
\ir ./analytics_stress_test_scenarios_notify_trigger_create.sql
\ir ./analytics_stress_test_shifts_create.sql
\ir ./analytics_stress_test_shifts_notify_trigger_create.sql

-- Today's market (an ORE TodaysMarket document, its collections and entries,
-- and the configurations that bind one entry from each). Ordered by dependency:
-- the collections and entries both reference the document, and a binding
-- references a configuration.
\ir ./analytics_todays_market_configs_create.sql
\ir ./analytics_todays_market_configs_notify_trigger_create.sql
\ir ./analytics_todays_market_collections_create.sql
\ir ./analytics_todays_market_collections_notify_trigger_create.sql
\ir ./analytics_todays_market_entries_create.sql
\ir ./analytics_todays_market_entries_notify_trigger_create.sql
\ir ./analytics_todays_market_configurations_create.sql
\ir ./analytics_todays_market_configurations_notify_trigger_create.sql
\ir ./analytics_todays_market_configuration_bindings_create.sql
\ir ./analytics_todays_market_configuration_bindings_notify_trigger_create.sql
