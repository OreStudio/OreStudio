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
#include <algorithm>
#include <array>
#include <charconv>
#include <optional>
#include <set>
#include <stdexcept>
#include <string_view>

namespace ores::ore::domain {

namespace {

using settings_t = reporting::domain::risk_report_config;

/**
 * @brief A choice that holds text, and the ORE parameter name it answers to.
 *
 * The mapper is a rename table rather than forty branches: the forward map looks
 * a parameter's name up here and writes its value into the member, and the
 * reverse map walks the table in order and writes a parameter for every member
 * that has a value.
 */
struct text_binding {
    std::string_view parameter;
    std::optional<std::string> settings_t::*member;
};

/**
 * @brief A choice the configuration always states: the folders and the log file.
 *
 * Every shipped run document states all three, so a stored default never adds a
 * parameter the document did not have.
 */
struct required_binding {
    std::string_view parameter;
    std::string settings_t::*member;
};

struct int_binding {
    std::string_view parameter;
    std::optional<int> settings_t::*member;
};

const std::array<text_binding, 20> text_bindings = {{
    {"accrualDate", &settings_t::accrual_date},
    {"inputPathMarket", &settings_t::input_path_market},
    {"inputPathPortfolio", &settings_t::input_path_portfolio},
    {"observationModel", &settings_t::observation_model},
    {"baseCurrency", &settings_t::base_currency},
    {"dateCalendar", &settings_t::date_calendar},
    {"dateConvention", &settings_t::date_convention},
    {"fixingCutoff", &settings_t::fixing_cutoff},
    {"continueOnError", &settings_t::continue_on_error},
    {"buildFailedTrades", &settings_t::build_failed_trades},
    {"implyTodaysFixings", &settings_t::imply_todays_fixings},
    {"includeTodaysCashFlows", &settings_t::include_todays_cash_flows},
    {"includeReferenceDateEvents", &settings_t::include_reference_date_events},
    {"lazyMarketBuilding", &settings_t::lazy_market_building},
    {"enrichIndexFixings", &settings_t::enrich_index_fixings},
    {"useAnalytics", &settings_t::use_analytics},
    {"csvCommentReportHeader", &settings_t::csv_comment_report_header},
    {"defaultMappingToIdentity", &settings_t::default_mapping_to_identity},
    {"portfolioRecurseIntoSubDirectories", &settings_t::portfolio_recurse_into_sub_directories},
    {"additionalResults", &settings_t::additional_results},
}};

const std::array<required_binding, 3> required_bindings = {{
    {"inputPath", &settings_t::input_path},
    {"outputPath", &settings_t::output_path},
    {"logFile", &settings_t::log_file},
}};

const std::array<int_binding, 3> int_bindings = {{
    {"logMask", &settings_t::log_mask},
    {"nThreads", &settings_t::n_threads},
    {"ignoreFixingLag", &settings_t::ignore_fixing_lag},
}};

/**
 * @brief The Setup parameters that name a file.
 *
 * They are data, not choices, so they are carried beside the configuration and
 * never stored on it.
 */
const std::array<std::string_view, 16> file_parameters = {{
    "curveConfigFile",
    "conventionsFile",
    "marketConfigFile",
    "pricingEnginesFile",
    "pricingEnginesFileScenario",
    "portfolioFile",
    "marketDataFile",
    "marketDataMappingFile",
    "fixingDataFile",
    "fixingDataMappingFile",
    "calendarAdjustment",
    "currencyConfiguration",
    "referenceDataFile",
    "counterpartyFile",
    "scriptLibrary",
    "iborFallbackConfig",
}};

/**
 * @brief Reads a parameter the entity holds as an integer.
 *
 * ORE writes these as text, so a value that is not a whole number is a
 * document the mapper cannot represent. It is refused by name rather than
 * converted with std::stoi, which throws an exception that says nothing about
 * which parameter was at fault.
 */
int parse_int(const std::string& name, const std::string& value) {
    int out = 0;
    const auto* first = value.data();
    const auto* last = value.data() + value.size();
    const auto result = std::from_chars(first, last, out);
    if (result.ec != std::errc() || result.ptr != last)
        throw std::runtime_error("run_document_mapper: Setup parameter '" + name +
                                 "' is not an integer: '" + value + "'");
    return out;
}

}

mapped_setup run_document_mapper::map_setup(const ore& v) {
    mapped_setup r;
    auto& settings = r.settings;

    // A name that appears more than once is read from its last occurrence.
    // Three shipped documents repeat continueOnError, and in all three the
    // repeated values agree, so the rule is stated rather than enforced.
    for (const auto& parameter : v.Setup.Parameter) {
        const std::string name(parameter.name);
        const std::string value(parameter);

        if (name == "asofDate") {
            settings.market_data_type = "date";
            settings.market_data_date = value;
            continue;
        }

        bool matched = false;
        for (const auto& binding : text_bindings) {
            if (binding.parameter == name) {
                settings.*(binding.member) = value;
                matched = true;
                break;
            }
        }
        if (matched)
            continue;

        for (const auto& binding : required_bindings) {
            if (binding.parameter == name) {
                settings.*(binding.member) = value;
                matched = true;
                break;
            }
        }
        if (matched)
            continue;

        for (const auto& binding : int_bindings) {
            if (binding.parameter == name) {
                settings.*(binding.member) = parse_int(name, value);
                matched = true;
                break;
            }
        }
        if (matched)
            continue;

        const auto file = std::find(file_parameters.begin(), file_parameters.end(), name);
        if (file == file_parameters.end())
            throw std::runtime_error("run_document_mapper: no column holds Setup parameter '" +
                                     name + "'");

        const auto existing = std::find_if(
            r.files.begin(), r.files.end(), [&name](const auto& f) { return f.parameter == name; });
        if (existing != r.files.end())
            existing->name = value;
        else
            r.files.push_back({name, value});
    }

    return r;
}

std::vector<ores::reporting::messaging::run_analytic>
run_document_mapper::map_analytics(const ore& v) {
    std::vector<ores::reporting::messaging::run_analytic> r;
    r.reserve(v.Analytics.Analytic.size());

    int order = 0;
    for (const auto& element : v.Analytics.Analytic) {
        ++order;
        ores::reporting::messaging::run_analytic mapped;
        mapped.analytic.display_order = order;
        if (element.type)
            mapped.analytic.analytic_type_code = std::string(*element.type);

        int position = 0;
        for (const auto& parameter : element.Parameter) {
            ++position;
            const std::string name(parameter.name);
            const std::string value(parameter);
            if (name == "active") {
                mapped.analytic.active = value;
                continue;
            }
            mapped.parameters.push_back({name, value, position});
        }

        r.push_back(std::move(mapped));
    }

    return r;
}

analyticsType run_document_mapper::reverse_analytics(
    const std::vector<ores::reporting::messaging::run_analytic>& v) {
    analyticsType r;

    for (const auto& mapped : v) {
        analyticsType_Analytic_t element;
        element.type = mapped.analytic.analytic_type_code;

        // An element the document wrote without an active flag must not gain
        // one on the way back out.
        if (!mapped.analytic.active.empty()) {
            parameterListType_Parameter_t active;
            active.name = "active";
            static_cast<std::string&>(active) = mapped.analytic.active;
            element.Parameter.push_back(active);
        }

        for (const auto& parameter : mapped.parameters) {
            parameterListType_Parameter_t x;
            x.name = parameter.name;
            static_cast<std::string&>(x) = parameter.value;
            element.Parameter.push_back(x);
        }

        r.Analytic.push_back(element);
    }

    return r;
}

std::vector<reporting::domain::configuration_market_binding>
run_document_mapper::map_market_bindings(const ore& v) {
    std::vector<reporting::domain::configuration_market_binding> r;
    if (!v.Markets)
        return r;

    int position = 0;
    for (const auto& parameter : v.Markets->Parameter) {
        ++position;
        reporting::domain::configuration_market_binding binding;
        binding.role = std::string(parameter.name);
        binding.market_configuration_name = static_cast<const std::string&>(parameter);
        binding.position = position;
        r.push_back(std::move(binding));
    }

    return r;
}

parameterListType run_document_mapper::reverse_market_bindings(
    const std::vector<reporting::domain::configuration_market_binding>& v) {
    parameterListType r;

    for (const auto& binding : v) {
        parameterListType_Parameter_t parameter;
        parameter.name = binding.role;
        static_cast<std::string&>(parameter) = binding.market_configuration_name;
        r.Parameter.push_back(parameter);
    }

    return r;
}

parameterListType run_document_mapper::reverse_setup(const mapped_setup& v) {
    parameterListType r;
    const auto& settings = v.settings;

    const auto append = [&r](std::string_view name, const std::string& value) {
        parameterListType_Parameter_t parameter;
        parameter.name = std::string(name);
        static_cast<std::string&>(parameter) = value;
        r.Parameter.push_back(parameter);
    };

    if (settings.market_data_type == "date")
        append("asofDate", settings.market_data_date);

    for (const auto& binding : required_bindings)
        append(binding.parameter, settings.*(binding.member));
    for (const auto& binding : text_bindings) {
        if (const auto& value = settings.*(binding.member))
            append(binding.parameter, *value);
    }
    for (const auto& binding : int_bindings) {
        if (const auto& value = settings.*(binding.member))
            append(binding.parameter, std::to_string(*value));
    }
    for (const auto& file : v.files)
        append(file.parameter, file.name);

    return r;
}

}
