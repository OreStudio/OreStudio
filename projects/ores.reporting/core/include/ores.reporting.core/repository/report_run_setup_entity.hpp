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
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_CORE_REPOSITORY_REPORT_RUN_SETUP_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_REPORT_RUN_SETUP_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a report run setup in the database.
 */
struct report_run_setup_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_report_run_setups_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string report_definition_id;

    std::optional<std::string> asof_date;
    std::optional<std::string> accrual_date;
    std::optional<std::string> input_path;
    std::optional<std::string> input_path_market;
    std::optional<std::string> input_path_portfolio;
    std::optional<std::string> output_path;
    std::optional<std::string> log_file;
    std::optional<int> log_mask;
    std::optional<int> n_threads;
    std::optional<std::string> observation_model;
    std::optional<std::string> base_currency;
    std::optional<std::string> date_calendar;
    std::optional<std::string> date_convention;
    std::optional<std::string> fixing_cutoff;
    std::optional<std::string> continue_on_error;
    std::optional<std::string> build_failed_trades;
    std::optional<std::string> imply_todays_fixings;
    std::optional<int> ignore_fixing_lag;
    std::optional<std::string> include_todays_cash_flows;
    std::optional<std::string> include_reference_date_events;
    std::optional<std::string> lazy_market_building;
    std::optional<std::string> enrich_index_fixings;
    std::optional<std::string> use_analytics;
    std::optional<std::string> csv_comment_report_header;
    std::optional<std::string> default_mapping_to_identity;
    std::optional<std::string> portfolio_recurse_into_sub_directories;
    std::optional<std::string> curve_config_file;
    std::optional<std::string> conventions_file;
    std::optional<std::string> market_config_file;
    std::optional<std::string> pricing_engines_file;
    std::optional<std::string> pricing_engines_file_scenario;
    std::optional<std::string> portfolio_file;
    std::optional<std::string> market_data_file;
    std::optional<std::string> market_data_mapping_file;
    std::optional<std::string> fixing_data_file;
    std::optional<std::string> fixing_data_mapping_file;
    std::optional<std::string> calendar_adjustment;
    std::optional<std::string> currency_configuration;
    std::optional<std::string> reference_data_file;
    std::optional<std::string> counterparty_file;
    std::optional<std::string> script_library;
    std::optional<std::string> ibor_fallback_config;
    std::optional<std::string> additional_results;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const report_run_setup_entity& v);

}

#endif
