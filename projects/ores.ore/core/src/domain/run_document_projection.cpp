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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.ore.core/domain/run_document_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.ore.core/xml/emitter.hpp"
#include "ores.ore.core/xml/record.hpp"
#include <algorithm>
#include <array>
#include <charconv>
#include <format>
#include <stdexcept>

namespace ores::ore::domain {

namespace {

namespace xg = xml::generated::ore;
using xml::record_reader;
using xml::record_writer;

using settings_t = reporting::domain::risk_report_config;

// A choice that holds text, and the ORE parameter name it answers to. The
// projection is a rename table rather than forty branches.
struct text_binding {
    std::string_view parameter;
    std::optional<std::string> settings_t::* member;
};

struct required_binding {
    std::string_view parameter;
    std::string settings_t::* member;
};

struct int_binding {
    std::string_view parameter;
    std::optional<int> settings_t::* member;
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

// The Setup parameters that name a file: data, not choices, carried beside the
// configuration and never stored on it.
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

std::runtime_error refusal(const std::string& what) {
    return std::runtime_error("run_document_projection: " + what);
}

// ORE writes these as text, so a value that is not a whole number is a document
// the projection cannot represent. It is refused by name rather than converted
// with std::stoi, whose exception says nothing about which parameter was at
// fault.
int parse_int(const std::string& name, const std::string& value) {
    int out = 0;
    const auto* first = value.data();
    const auto* last = value.data() + value.size();
    const auto result = std::from_chars(first, last, out);
    if (result.ec != std::errc() || result.ptr != last)
        throw refusal("Setup parameter '" + name + "' is not an integer: '" + value + "'");
    return out;
}

void append_parameter(record_writer out, std::string_view name, const std::string& value) {
    auto parameter = out.append("Parameter");
    parameter.set_text("name", name);
    parameter.set_own_text(value);
}

}

ores::reporting::messaging::run_document run_document_projection::map(const document& v) {
    ores::reporting::messaging::run_document r;
    auto setup = map_setup(v);
    r.settings = std::move(setup.settings);
    r.files = std::move(setup.files);
    r.analytics = map_analytics(v);
    r.market_bindings = map_market_bindings(v);
    return r;
}

run_document_projection::document
run_document_projection::reverse(const ores::reporting::messaging::run_document& v) {
    document out{};
    auto written = record_writer(*xg::ore_document().root, &out);
    reverse_setup(written.child("Setup"), {v.settings, v.files});
    reverse_analytics(written.child("Analytics"), v.analytics);
    if (!v.market_bindings.empty())
        reverse_market_bindings(written.child("Markets"), v.market_bindings);
    return out;
}

mapped_setup run_document_projection::map_setup(const document& v) {
    mapped_setup r;
    auto& settings = r.settings;
    const auto root = record_reader(*xg::ore_document().root, &v);
    const auto setup = root.child("Setup");
    if (!setup)
        throw refusal("the document states no Setup block");

    // A name that appears more than once is read from its last occurrence.
    // Three shipped documents repeat continueOnError, and in all three the
    // repeated values agree, so the rule is stated rather than enforced.
    for (const auto& parameter : setup->children("Parameter")) {
        const auto name = parameter.text("name").value_or("");
        const auto value = parameter.own_text().value_or("");

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

        if (std::find(file_parameters.begin(), file_parameters.end(), name) ==
            file_parameters.end())
            throw refusal("no column holds Setup parameter '" + name + "'");
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
run_document_projection::map_analytics(const document& v) {
    std::vector<ores::reporting::messaging::run_analytic> r;
    const auto root = record_reader(*xg::ore_document().root, &v);
    const auto analytics = root.child("Analytics");
    if (!analytics)
        throw refusal("the document states no Analytics block");

    int order = 0;
    for (const auto& element : analytics->children("Analytic")) {
        ++order;
        ores::reporting::messaging::run_analytic mapped;
        mapped.analytic.display_order = order;
        mapped.analytic.analytic_type_code = element.text("type").value_or("");

        int position = 0;
        for (const auto& parameter : element.children("Parameter")) {
            ++position;
            const auto name = parameter.text("name").value_or("");
            const auto value = parameter.own_text().value_or("");
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

std::vector<reporting::domain::configuration_market_binding>
run_document_projection::map_market_bindings(const document& v) {
    std::vector<reporting::domain::configuration_market_binding> r;
    const auto root = record_reader(*xg::ore_document().root, &v);
    const auto markets = root.child("Markets");
    if (!markets)
        return r;

    int position = 0;
    for (const auto& parameter : markets->children("Parameter")) {
        ++position;
        reporting::domain::configuration_market_binding binding;
        binding.role = parameter.text("name").value_or("");
        binding.market_configuration_name = parameter.own_text().value_or("");
        binding.position = position;
        r.push_back(std::move(binding));
    }
    return r;
}

void run_document_projection::reverse_setup(record_writer out, const mapped_setup& v) {
    const auto& settings = v.settings;
    if (settings.market_data_type == "date")
        append_parameter(out, "asofDate", settings.market_data_date);
    for (const auto& binding : required_bindings)
        append_parameter(out, binding.parameter, settings.*(binding.member));
    for (const auto& binding : text_bindings) {
        if (const auto& value = settings.*(binding.member))
            append_parameter(out, binding.parameter, *value);
    }
    for (const auto& binding : int_bindings) {
        if (const auto& value = settings.*(binding.member))
            append_parameter(out, binding.parameter, std::to_string(*value));
    }
    for (const auto& file : v.files)
        append_parameter(out, file.parameter, file.name);
}

void run_document_projection::reverse_analytics(
    record_writer out, const std::vector<ores::reporting::messaging::run_analytic>& v) {
    for (const auto& mapped : v) {
        auto element = out.append("Analytic");
        element.set_optional_text("type", mapped.analytic.analytic_type_code);
        // An element the document wrote without an active flag must not gain one
        // on the way back out.
        if (!mapped.analytic.active.empty())
            append_parameter(element, "active", mapped.analytic.active);
        for (const auto& parameter : mapped.parameters) {
            auto x = element.append("Parameter");
            x.set_text("name", parameter.name);
            x.set_own_text(parameter.value);
        }
    }
}

void run_document_projection::reverse_market_bindings(
    record_writer out, const std::vector<reporting::domain::configuration_market_binding>& v) {
    for (const auto& binding : v)
        append_parameter(out, binding.role, binding.market_configuration_name);
}

run_document_projection::document run_document_projection::read(std::string_view text,
                                                                std::string_view origin) {
    document record;
    const auto bound = xml::binder::bind(xg::ore_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("run document") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string run_document_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(xg::ore_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

}
