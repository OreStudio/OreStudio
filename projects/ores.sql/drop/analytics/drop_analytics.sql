\ir ./analytics_stress_test_shifts_notify_trigger_drop.sql
\ir ./analytics_stress_test_shifts_drop.sql
\ir ./analytics_stress_test_scenarios_notify_trigger_drop.sql
\ir ./analytics_stress_test_scenarios_drop.sql
\ir ./analytics_stress_test_libraries_notify_trigger_drop.sql
\ir ./analytics_stress_test_libraries_drop.sql

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

-- Drop in reverse dependency order (children before parents)
\ir ./analytics_pricing_model_product_parameters_notify_trigger_drop.sql
\ir ./analytics_pricing_model_product_parameters_drop.sql

\ir ./analytics_pricing_model_products_notify_trigger_drop.sql
\ir ./analytics_pricing_model_products_drop.sql

\ir ./analytics_pricing_model_configs_notify_trigger_drop.sql
\ir ./analytics_pricing_model_configs_drop.sql

\ir ./analytics_pricing_engine_types_notify_trigger_drop.sql
\ir ./analytics_pricing_engine_types_drop.sql


\ir ./analytics_credit_simulation_entity_configs_notify_trigger_drop.sql
\ir ./analytics_credit_simulation_entity_configs_drop.sql


\ir ./analytics_credit_simulation_matrix_row_configs_notify_trigger_drop.sql
\ir ./analytics_credit_simulation_matrix_row_configs_drop.sql

\ir ./analytics_credit_simulation_matrix_configs_notify_trigger_drop.sql
\ir ./analytics_credit_simulation_matrix_configs_drop.sql

\ir ./analytics_credit_simulation_configs_notify_trigger_drop.sql
\ir ./analytics_credit_simulation_netting_set_configs_notify_trigger_drop.sql
\ir ./analytics_credit_simulation_netting_set_configs_drop.sql

\ir ./analytics_credit_simulation_configs_drop.sql

-- Today's market, children first so nothing is dropped while a reference holds.
\ir ./analytics_todays_market_configuration_bindings_notify_trigger_drop.sql
\ir ./analytics_todays_market_configuration_bindings_drop.sql
\ir ./analytics_todays_market_configurations_notify_trigger_drop.sql
\ir ./analytics_todays_market_configurations_drop.sql
\ir ./analytics_todays_market_entries_notify_trigger_drop.sql
\ir ./analytics_todays_market_entries_drop.sql
\ir ./analytics_todays_market_collections_notify_trigger_drop.sql
\ir ./analytics_todays_market_collections_drop.sql
\ir ./analytics_todays_market_configs_notify_trigger_drop.sql
\ir ./analytics_todays_market_configs_drop.sql
