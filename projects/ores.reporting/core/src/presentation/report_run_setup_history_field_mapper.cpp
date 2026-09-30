/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.core/presentation/report_run_setup_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::presentation {

std::vector<ores::diff::domain::field_value>
render_report_run_setup_fields(const domain::report_run_setup& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Report Definition ID", .value = boost::uuids::to_string(v.report_definition_id)});
    fields.push_back({.name = "Asof Date", .value = v.asof_date.value_or(std::string{})});
    fields.push_back({.name = "Accrual Date", .value = v.accrual_date.value_or(std::string{})});
    fields.push_back({.name = "Input Path", .value = v.input_path.value_or(std::string{})});
    fields.push_back(
        {.name = "Input Path Market", .value = v.input_path_market.value_or(std::string{})});
    fields.push_back(
        {.name = "Input Path Portfolio", .value = v.input_path_portfolio.value_or(std::string{})});
    fields.push_back({.name = "Output Path", .value = v.output_path.value_or(std::string{})});
    fields.push_back({.name = "Log File", .value = v.log_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Log Mask", .value = v.log_mask ? std::to_string(*v.log_mask) : std::string{}});
    fields.push_back(
        {.name = "N Threads", .value = v.n_threads ? std::to_string(*v.n_threads) : std::string{}});
    fields.push_back(
        {.name = "Observation Model", .value = v.observation_model.value_or(std::string{})});
    fields.push_back({.name = "Base Currency", .value = v.base_currency.value_or(std::string{})});
    fields.push_back({.name = "Date Calendar", .value = v.date_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Date Convention", .value = v.date_convention.value_or(std::string{})});
    fields.push_back({.name = "Fixing Cutoff", .value = v.fixing_cutoff.value_or(std::string{})});
    fields.push_back(
        {.name = "Continue On Error", .value = v.continue_on_error.value_or(std::string{})});
    fields.push_back(
        {.name = "Build Failed Trades", .value = v.build_failed_trades.value_or(std::string{})});
    fields.push_back(
        {.name = "Imply Todays Fixings", .value = v.imply_todays_fixings.value_or(std::string{})});
    fields.push_back(
        {.name = "Ignore Fixing Lag",
         .value = v.ignore_fixing_lag ? std::to_string(*v.ignore_fixing_lag) : std::string{}});
    fields.push_back({.name = "Include Todays Cash Flows",
                      .value = v.include_todays_cash_flows.value_or(std::string{})});
    fields.push_back({.name = "Include Reference Date Events",
                      .value = v.include_reference_date_events.value_or(std::string{})});
    fields.push_back(
        {.name = "Lazy Market Building", .value = v.lazy_market_building.value_or(std::string{})});
    fields.push_back(
        {.name = "Enrich Index Fixings", .value = v.enrich_index_fixings.value_or(std::string{})});
    fields.push_back({.name = "Use Analytics", .value = v.use_analytics.value_or(std::string{})});
    fields.push_back({.name = "Csv Comment Report Header",
                      .value = v.csv_comment_report_header.value_or(std::string{})});
    fields.push_back({.name = "Default Mapping To Identity",
                      .value = v.default_mapping_to_identity.value_or(std::string{})});
    fields.push_back({.name = "Portfolio Recurse Into Sub Directories",
                      .value = v.portfolio_recurse_into_sub_directories.value_or(std::string{})});
    fields.push_back(
        {.name = "Curve Config File", .value = v.curve_config_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Conventions File", .value = v.conventions_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Market Config File", .value = v.market_config_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Pricing Engines File", .value = v.pricing_engines_file.value_or(std::string{})});
    fields.push_back({.name = "Pricing Engines File Scenario",
                      .value = v.pricing_engines_file_scenario.value_or(std::string{})});
    fields.push_back({.name = "Portfolio File", .value = v.portfolio_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Market Data File", .value = v.market_data_file.value_or(std::string{})});
    fields.push_back({.name = "Market Data Mapping File",
                      .value = v.market_data_mapping_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Fixing Data File", .value = v.fixing_data_file.value_or(std::string{})});
    fields.push_back({.name = "Fixing Data Mapping File",
                      .value = v.fixing_data_mapping_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Calendar Adjustment", .value = v.calendar_adjustment.value_or(std::string{})});
    fields.push_back({.name = "Currency Configuration",
                      .value = v.currency_configuration.value_or(std::string{})});
    fields.push_back(
        {.name = "Reference Data File", .value = v.reference_data_file.value_or(std::string{})});
    fields.push_back(
        {.name = "Counterparty File", .value = v.counterparty_file.value_or(std::string{})});
    fields.push_back({.name = "Script Library", .value = v.script_library.value_or(std::string{})});
    fields.push_back(
        {.name = "Ibor Fallback Config", .value = v.ibor_fallback_config.value_or(std::string{})});
    fields.push_back(
        {.name = "Additional Results", .value = v.additional_results.value_or(std::string{})});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
