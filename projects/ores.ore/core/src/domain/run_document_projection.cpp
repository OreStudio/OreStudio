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
#include <functional>
#include <optional>
#include <stdexcept>

namespace ores::ore::domain {

namespace {

namespace run_document_projection_xg = xml::generated::ore;
using xml::record_reader;
using xml::record_writer;

using settings_t = reporting::domain::risk_report_config;

// A choice that holds text, and the ORE parameter name it answers to. The
// projection is a rename table rather than forty branches.
struct text_binding {
    std::string_view parameter;
    std::optional<std::string> settings_t::*member;
};

struct required_binding {
    std::string_view parameter;
    std::string settings_t::*member;
};

const std::array<text_binding, 4> text_bindings = {{
    {"inputPathMarket", &settings_t::input_path_market},
    {"inputPathPortfolio", &settings_t::input_path_portfolio},
    {"baseCurrency", &settings_t::base_currency},
    {"portfolioRecurseIntoSubDirectories", &settings_t::portfolio_recurse_into_sub_directories},
}};

const std::array<required_binding, 3> required_bindings = {{
    {"inputPath", &settings_t::input_path},
    {"outputPath", &settings_t::output_path},
    {"logFile", &settings_t::log_file},
}};

using engine_t = reporting::messaging::run_engine_settings;

// An engine setting: the ORE parameter, and how to read it into a part and write
// it back. The parts are created when their first setting is read, so a document
// that sets none of a group leaves the group absent and the engine default holds.
struct engine_binding {
    std::string_view parameter;
    std::function<void(engine_t&, const std::string&)> read;
    std::function<std::optional<std::string>(const engine_t&)> write;
};

// ORE's parseBool spellings.
bool parse_flag(const std::string& name, const std::string& value) {
    static constexpr std::array<std::string_view, 6> yes = {
        "Y", "YES", "TRUE", "True", "true", "1"};
    static constexpr std::array<std::string_view, 6> no = {
        "N", "NO", "FALSE", "False", "false", "0"};
    if (std::ranges::find(yes, value) != yes.end())
        return true;
    if (std::ranges::find(no, value) != no.end())
        return false;
    throw std::runtime_error("run_document_projection: Setup parameter '" + name +
                             "' is not a flag: '" + value + "'");
}

int parse_int(const std::string& name, const std::string& value);

template <typename Part>
engine_binding flag_binding(std::string_view parameter,
                            std::optional<Part> engine_t::*part,
                            std::optional<bool> Part::*member) {
    return {parameter,
            [=](engine_t& e, const std::string& v) {
                auto& p = e.*part;
                if (!p)
                    p.emplace();
                (*p).*member = parse_flag(std::string(parameter), v);
            },
            [=](const engine_t& e) -> std::optional<std::string> {
                if (!(e.*part) || !((*(e.*part)).*member))
                    return std::nullopt;
                return *((*(e.*part)).*member) ? "true" : "false";
            }};
}

template <typename Part>
engine_binding int_binding(std::string_view parameter,
                           std::optional<Part> engine_t::*part,
                           std::optional<int> Part::*member) {
    return {parameter,
            [=](engine_t& e, const std::string& v) {
                auto& p = e.*part;
                if (!p)
                    p.emplace();
                (*p).*member = parse_int(std::string(parameter), v);
            },
            [=](const engine_t& e) -> std::optional<std::string> {
                if (!(e.*part) || !((*(e.*part)).*member))
                    return std::nullopt;
                return std::to_string(*((*(e.*part)).*member));
            }};
}

using fixings_t = reporting::domain::engine_settings_fixings;
using building_t = reporting::domain::engine_settings_building;
using execution_t = reporting::domain::engine_settings_execution;

const std::vector<engine_binding>& engine_bindings() {
    static const std::vector<engine_binding> bindings = {
        flag_binding("implyTodaysFixings", &engine_t::fixings, &fixings_t::imply_todays_fixings),
        flag_binding(
            "includeTodaysCashFlows", &engine_t::fixings, &fixings_t::include_todays_cash_flows),
        flag_binding("includeReferenceDateEvents",
                     &engine_t::fixings,
                     &fixings_t::include_reference_date_events),
        flag_binding("enrichIndexFixings", &engine_t::fixings, &fixings_t::enrich_index_fixings),
        int_binding("ignoreFixingLag", &engine_t::fixings, &fixings_t::ignore_fixing_lag),
        int_binding("ignoreFixingLead", &engine_t::fixings, &fixings_t::ignore_fixing_lead),
        {"observationModel",
         [](engine_t& e, const std::string& v) {
             /*
              * The document states a word here and the store refuses one its set
              * does not accept, so no list of the words lives in this codec.
              */
             if (!e.building)
                 e.building.emplace();
             e.building->observation_model = v;
         },
         [](const engine_t& e) -> std::optional<std::string> {
             if (!e.building)
                 return std::nullopt;
             return e.building->observation_model;
         }},
        flag_binding("lazyMarketBuilding", &engine_t::building, &building_t::lazy_market_building),
        flag_binding("buildFailedTrades", &engine_t::building, &building_t::build_failed_trades),
        flag_binding("continueOnError", &engine_t::building, &building_t::continue_on_error),
        flag_binding(
            "useAtParCouponsCurves", &engine_t::building, &building_t::use_at_par_coupons_curves),
        flag_binding(
            "useAtParCouponsTrades", &engine_t::building, &building_t::use_at_par_coupons_trades),
        int_binding("nThreads", &engine_t::execution, &execution_t::n_threads),
        int_binding("logMask", &engine_t::execution, &execution_t::log_mask),
        int_binding(
            "gzipCompressionLevel", &engine_t::execution, &execution_t::gzip_compression_level),
    };
    return bindings;
}

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

std::runtime_error run_document_projection_refusal(const std::string& what) {
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
        throw run_document_projection_refusal("Setup parameter '" + name +
                                              "' is not an integer: '" + value + "'");
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
    r.engine_settings = std::move(setup.engine_settings);
    r.files = std::move(setup.files);
    r.analytics = map_analytics(v);
    r.market_bindings = map_market_bindings(v);
    return r;
}

run_document_projection::document
run_document_projection::reverse(const ores::reporting::messaging::run_document& v) {
    document out{};
    auto written = record_writer(*run_document_projection_xg::ore_document().root, &out);
    reverse_setup(written.child("Setup"), {v.settings, v.engine_settings, v.files, {}});
    reverse_analytics(written.child("Analytics"), v.analytics);
    if (!v.market_bindings.empty())
        reverse_market_bindings(written.child("Markets"), v.market_bindings);
    return out;
}

mapped_setup run_document_projection::map_setup(const document& v) {
    mapped_setup r;
    auto& settings = r.settings;
    const auto root = record_reader(*run_document_projection_xg::ore_document().root, &v);
    const auto setup = root.child("Setup");
    if (!setup)
        throw run_document_projection_refusal("the document states no Setup block");

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
        for (const auto& binding : engine_bindings()) {
            if (binding.parameter == name) {
                if (!r.engine_settings)
                    r.engine_settings.emplace();
                binding.read(*r.engine_settings, value);
                matched = true;
                break;
            }
        }
        if (matched)
            continue;

        // A parameter the model does not hold is dropped, and the import reports it.
        if (std::find(file_parameters.begin(), file_parameters.end(), name) ==
            file_parameters.end()) {
            if (std::ranges::find(r.dropped, name) == r.dropped.end())
                r.dropped.push_back(name);
            continue;
        }
        const auto existing = std::find_if(
            r.files.begin(), r.files.end(), [&name](const auto& f) { return f.parameter == name; });
        if (existing != r.files.end())
            existing->name = value;
        else
            r.files.push_back({name, value});
    }

    return r;
}

std::vector<std::string> run_document_projection::dropped_parameter_warnings(const document& v) {
    std::vector<std::string> warnings;
    for (const auto& name : map_setup(v).dropped) {
        auto text =
            std::format("Setup parameter '{}' is not held by the model and was dropped.", name);
        if (name == "fixingCutoff")
            text += " ORE reads it and the run will not, so its numbers may differ from the "
                    "original run.";
        else if (name == "csvCommentReportHeader")
            text += " ORE reads it and the run will not.";
        warnings.push_back(std::move(text));
    }
    return warnings;
}

std::vector<ores::reporting::messaging::run_analytic>
run_document_projection::map_analytics(const document& v) {
    std::vector<ores::reporting::messaging::run_analytic> r;
    const auto root = record_reader(*run_document_projection_xg::ore_document().root, &v);
    const auto analytics = root.child("Analytics");
    if (!analytics)
        throw run_document_projection_refusal("the document states no Analytics block");

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
    const auto root = record_reader(*run_document_projection_xg::ore_document().root, &v);
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
    if (v.engine_settings) {
        for (const auto& binding : engine_bindings()) {
            if (const auto value = binding.write(*v.engine_settings))
                append_parameter(out, binding.parameter, *value);
        }
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
    const auto bound = xml::binder::bind(run_document_projection_xg::ore_document(), text, &record);
    if (!bound) {
        const auto where = origin.empty() ? std::string("run document") : std::string(origin);
        throw std::invalid_argument(
            std::format("{}: line {}: {}", where, bound.error().line, bound.error().message));
    }
    return record;
}

std::string run_document_projection::write(const document& v) {
    std::string out;
    const auto emitted = xml::emitter::emit(run_document_projection_xg::ore_document(), &v, out);
    if (!emitted)
        throw std::runtime_error(emitted.error().message);
    return out;
}

std::optional<std::string> run_document_projection::as_of_date(std::string_view text) {
    const auto mapped = map(read(text));
    if (mapped.settings.market_data_type != "date" || mapped.settings.market_data_date.empty())
        return std::nullopt;
    return mapped.settings.market_data_date;
}

std::string run_document_projection::with_as_of_date(std::string_view text, std::string_view date) {
    auto mapped = map(read(text));
    mapped.settings.market_data_type = "date";
    mapped.settings.market_data_date = std::string(date);
    return write(reverse(mapped));
}

}
