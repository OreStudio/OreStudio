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
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include <array>
#include <optional>
#include <string_view>

namespace ores::ore::domain {

namespace {

using setup_t = reporting::domain::report_run_setup;

/**
 * @brief A setup column that holds text, and the ORE parameter name it answers to.
 *
 * The mapper is a rename table rather than forty branches: the forward map looks
 * a parameter's name up here and writes its value into the member, and the
 * reverse map walks the table in order and writes a parameter for every member
 * that has a value.
 */
struct text_binding {
    std::string_view parameter;
    std::optional<std::string> setup_t::* member;
};

struct int_binding {
    std::string_view parameter;
    std::optional<int> setup_t::* member;
};

const std::array<text_binding, 40> text_bindings = {{
    {"asofDate", &setup_t::asof_date},
    {"accrualDate", &setup_t::accrual_date},
    {"inputPath", &setup_t::input_path},
    {"inputPathMarket", &setup_t::input_path_market},
    {"inputPathPortfolio", &setup_t::input_path_portfolio},
    {"outputPath", &setup_t::output_path},
    {"logFile", &setup_t::log_file},
    {"observationModel", &setup_t::observation_model},
    {"baseCurrency", &setup_t::base_currency},
    {"dateCalendar", &setup_t::date_calendar},
    {"dateConvention", &setup_t::date_convention},
    {"fixingCutoff", &setup_t::fixing_cutoff},
    {"continueOnError", &setup_t::continue_on_error},
    {"buildFailedTrades", &setup_t::build_failed_trades},
    {"implyTodaysFixings", &setup_t::imply_todays_fixings},
    {"includeTodaysCashFlows", &setup_t::include_todays_cash_flows},
    {"includeReferenceDateEvents", &setup_t::include_reference_date_events},
    {"lazyMarketBuilding", &setup_t::lazy_market_building},
    {"enrichIndexFixings", &setup_t::enrich_index_fixings},
    {"useAnalytics", &setup_t::use_analytics},
    {"csvCommentReportHeader", &setup_t::csv_comment_report_header},
    {"defaultMappingToIdentity", &setup_t::default_mapping_to_identity},
    {"portfolioRecurseIntoSubDirectories", &setup_t::portfolio_recurse_into_sub_directories},
    {"curveConfigFile", &setup_t::curve_config_file},
    {"conventionsFile", &setup_t::conventions_file},
    {"marketConfigFile", &setup_t::market_config_file},
    {"pricingEnginesFile", &setup_t::pricing_engines_file},
    {"pricingEnginesFileScenario", &setup_t::pricing_engines_file_scenario},
    {"portfolioFile", &setup_t::portfolio_file},
    {"marketDataFile", &setup_t::market_data_file},
    {"marketDataMappingFile", &setup_t::market_data_mapping_file},
    {"fixingDataFile", &setup_t::fixing_data_file},
    {"fixingDataMappingFile", &setup_t::fixing_data_mapping_file},
    {"calendarAdjustment", &setup_t::calendar_adjustment},
    {"currencyConfiguration", &setup_t::currency_configuration},
    {"referenceDataFile", &setup_t::reference_data_file},
    {"counterpartyFile", &setup_t::counterparty_file},
    {"scriptLibrary", &setup_t::script_library},
    {"iborFallbackConfig", &setup_t::ibor_fallback_config},
    {"additionalResults", &setup_t::additional_results},
}};

const std::array<int_binding, 3> int_bindings = {{
    {"logMask", &setup_t::log_mask},
    {"nThreads", &setup_t::n_threads},
    {"ignoreFixingLag", &setup_t::ignore_fixing_lag},
}};

}

reporting::domain::report_run_setup run_document_mapper::map_setup(const ore& v) {
    reporting::domain::report_run_setup r;

    for (const auto& parameter : v.Setup.Parameter) {
        const std::string name(parameter.name);
        const std::string value(parameter);

        bool matched = false;
        for (const auto& binding : text_bindings) {
            if (binding.parameter == name) {
                r.*(binding.member) = value;
                matched = true;
                break;
            }
        }
        if (matched)
            continue;

        for (const auto& binding : int_bindings) {
            if (binding.parameter == name) {
                r.*(binding.member) = std::stoi(value);
                break;
            }
        }
    }

    return r;
}

parameterListType run_document_mapper::reverse_setup(const reporting::domain::report_run_setup& v) {
    parameterListType r;

    const auto append = [&r](std::string_view name, const std::string& value) {
        parameterListType_Parameter_t parameter;
        parameter.name = std::string(name);
        static_cast<std::string&>(parameter) = value;
        r.Parameter.push_back(parameter);
    };

    for (const auto& binding : text_bindings) {
        if (const auto& value = v.*(binding.member))
            append(binding.parameter, *value);
    }
    for (const auto& binding : int_bindings) {
        if (const auto& value = v.*(binding.member))
            append(binding.parameter, std::to_string(*value));
    }

    return r;
}

}
