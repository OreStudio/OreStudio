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
-- Reporting Tables
-- =============================================================================
-- Report definitions (what to run and when) and report instances (each run).
-- Risk report configuration is stored in a dedicated table with analytics flags
-- derived from the ORE ore.xml structure.

-- Helper functions (depend on DQ FSM tables, which are created before reporting)
\ir ./reporting_functions_create.sql

-- Enum tables (must precede report_definitions which validates against them)
\ir ./reporting_report_types_create.sql
\ir ./reporting_report_types_notify_trigger_create.sql
\ir ./reporting_configuration_types_create.sql
\ir ./reporting_configuration_types_notify_trigger_create.sql
\ir ./reporting_report_type_configuration_type_create.sql
\ir ./reporting_analytic_types_create.sql
\ir ./reporting_analytic_types_notify_trigger_create.sql
\ir ./reporting_file_types_create.sql
\ir ./reporting_file_types_notify_trigger_create.sql
\ir ./reporting_configuration_analytics_create.sql
\ir ./reporting_configuration_analytics_notify_trigger_create.sql
\ir ./reporting_configuration_analytic_parameters_create.sql
\ir ./reporting_configuration_analytic_parameters_notify_trigger_create.sql
\ir ./reporting_configuration_market_bindings_create.sql
\ir ./reporting_configuration_market_bindings_notify_trigger_create.sql
\ir ./reporting_ore_parameter_value_domains_create.sql
\ir ./reporting_ore_parameter_value_domains_notify_trigger_create.sql
\ir ./reporting_ore_parameters_create.sql
\ir ./reporting_ore_parameters_notify_trigger_create.sql
\ir ./reporting_ore_parameter_value_set_catalogues_create.sql
\ir ./reporting_ore_parameter_value_set_catalogues_notify_trigger_create.sql
\ir ./reporting_ore_element_map_columns_create.sql
\ir ./reporting_ore_element_map_columns_notify_trigger_create.sql
\ir ./reporting_ore_parameter_vocabularies_create.sql
\ir ./reporting_ore_parameter_vocabularies_notify_trigger_create.sql
\ir ./reporting_ore_parameter_value_validate_fn_create.sql
\ir ./reporting_ore_parameter_value_sets_create.sql
\ir ./reporting_ore_parameter_value_sets_notify_trigger_create.sql
\ir ./reporting_ore_parameter_value_set_values_create.sql
\ir ./reporting_ore_parameter_value_set_values_notify_trigger_create.sql
\ir ./reporting_ore_parameter_setting_value_sets_create.sql
\ir ./reporting_ore_parameter_setting_value_sets_notify_trigger_create.sql
\ir ./reporting_engine_settings_groups_create.sql
\ir ./reporting_engine_settings_groups_notify_trigger_create.sql
\ir ./reporting_engine_settings_group_settings_create.sql
\ir ./reporting_engine_settings_group_settings_notify_trigger_create.sql
\ir ./reporting_configuration_parameters_create.sql
\ir ./reporting_configuration_parameters_notify_trigger_create.sql
\ir ./reporting_configurations_create.sql
\ir ./reporting_configurations_notify_trigger_create.sql
\ir ./reporting_report_configurations_create.sql
\ir ./reporting_report_configurations_notify_trigger_create.sql
\ir ./reporting_report_types_validate_fn_create.sql
\ir ./reporting_concurrency_policies_create.sql
\ir ./reporting_concurrency_policies_notify_trigger_create.sql
\ir ./reporting_concurrency_policies_validate_fn_create.sql
\ir ./reporting_report_instance_kinds_create.sql
\ir ./reporting_report_instance_kinds_notify_trigger_create.sql
\ir ./reporting_report_error_scopes_create.sql
\ir ./reporting_report_error_scopes_notify_trigger_create.sql
\ir ./reporting_report_trigger_types_create.sql
\ir ./reporting_report_trigger_types_notify_trigger_create.sql

\ir ./reporting_report_definitions_create.sql
\ir ./reporting_report_definitions_notify_trigger_create.sql
\ir ./reporting_risk_report_configs_create.sql
\ir ./reporting_risk_report_configs_notify_trigger_create.sql
\ir ./reporting_risk_report_config_portfolios_create.sql
\ir ./reporting_risk_report_config_books_create.sql
\ir ./reporting_engine_settings_create.sql
\ir ./reporting_engine_settings_notify_trigger_create.sql
\ir ./reporting_engine_settings_fixings_create.sql
\ir ./reporting_engine_settings_fixings_notify_trigger_create.sql
\ir ./reporting_engine_settings_building_create.sql
\ir ./reporting_engine_settings_building_notify_trigger_create.sql
\ir ./reporting_engine_settings_execution_create.sql
\ir ./reporting_engine_settings_execution_notify_trigger_create.sql
\ir ./reporting_engine_settings_part_value_guard_create.sql

-- The lines a proposal holds while its request waits
\ir ./reporting_engine_settings_changes_create.sql
\ir ./reporting_engine_settings_changes_notify_trigger_create.sql
\ir ./reporting_report_trigger_events_create.sql
\ir ./reporting_report_instances_create.sql
\ir ./reporting_report_instances_fsm_guard_create.sql
\ir ./reporting_report_instances_notify_trigger_create.sql
\ir ./reporting_report_result_rows_create.sql
\ir ./reporting_report_result_rows_notify_trigger_create.sql
\ir ./reporting_report_error_events_create.sql
\ir ./reporting_report_error_events_notify_trigger_create.sql
\ir ./reporting_report_input_bundles_create.sql
\ir ./reporting_in_flight_instance_fn_create.sql
-- The rows a report run produced, and their trigger. The table names the run it
-- belongs to, so it loads after the run tables above it.
\ir ./reporting_report_result_rows_create.sql
\ir ./reporting_report_result_rows_notify_trigger_create.sql

\ir ./reporting_resolve_book_ids_fn_create.sql
\ir ./reporting_configuration_analytics_fn_create.sql
\ir ./reporting_publish_from_dq_create.sql

-- The parameter settings view reads every value set table, so it comes last.
\ir ./reporting_ore_parameter_settings_view_create.sql
