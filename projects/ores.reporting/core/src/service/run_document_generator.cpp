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
#include "ores.reporting.core/service/run_document_generator.hpp"
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <format>
#include <optional>
#include <stdexcept>

namespace ores::reporting::service {

namespace {

using domain::file_type;

constexpr const char* output_kind = "output_file";
constexpr const char* intermediate_kind = "intermediate_file";

const file_type* find_by_code(const std::vector<file_type>& types, const std::string& code) {
    const auto it = std::ranges::find(types, code, &file_type::code);
    return it == types.end() ? nullptr : &*it;
}

// The Setup file type of a configuration type: the parameter that names the document.
const file_type* setup_file_type(const std::vector<file_type>& types, const std::string& type_code) {
    const auto it = std::ranges::find_if(types, [&](const file_type& t) {
        return !t.analytic_type_code && t.configuration_type_code == type_code;
    });
    return it == types.end() ? nullptr : &*it;
}

const messaging::run_parameter* find_parameter(const messaging::run_analytic& a,
                                               const std::string& name) {
    const auto it = std::ranges::find(a.parameters, name, &messaging::run_parameter::name);
    return it == a.parameters.end() ? nullptr : &*it;
}

bool is_on(const messaging::run_analytic& a) {
    return a.analytic.active == "Y";
}

// The name a file has in this run: the value the configuration states, or the well
// known name. A reader follows its writer, so the two cannot differ.
std::optional<std::string> name_of(const std::vector<file_type>& types,
                                   const std::vector<messaging::run_analytic>& analytics,
                                   const file_type& t) {
    if (t.produced_by_file_type_code) {
        const auto* writer = find_by_code(types, *t.produced_by_file_type_code);
        if (writer == nullptr || !writer->analytic_type_code)
            return std::nullopt;
        const auto it = std::ranges::find_if(analytics, [&](const auto& a) {
            return is_on(a) && a.analytic.analytic_type_code == *writer->analytic_type_code;
        });
        if (it == analytics.end())
            return std::nullopt;
        if (const auto* stated = find_parameter(*it, writer->ore_parameter))
            return stated->value;
        return writer->name_pattern;
    }
    return t.name_pattern;
}

// Names the files an analytic reads or writes by default.
void name_default_files(const std::vector<file_type>& types,
                        const std::vector<messaging::run_analytic>& analytics,
                        messaging::run_analytic& a) {
    int position = 0;
    for (const auto& p : a.parameters)
        position = std::max(position, p.position);
    for (const auto& t : types) {
        if (!t.is_default || t.analytic_type_code != a.analytic.analytic_type_code)
            continue;
        if (t.kind != output_kind && t.kind != intermediate_kind)
            continue;
        if (find_parameter(a, t.ore_parameter) != nullptr)
            continue;
        const auto name = name_of(types, analytics, t);
        if (!name)
            continue;
        a.parameters.push_back({t.ore_parameter, *name, ++position});
    }
}

// The weekday before a date. Holiday calendars are not consulted.
std::string previous_weekday(const std::string& iso_date) {
    int y = 0;
    unsigned m = 0, d = 0;
    char tail = 0;
    if (std::sscanf(iso_date.c_str(), "%d-%u-%u%c", &y, &m, &d, &tail) != 3)
        throw std::invalid_argument(
            std::format("The run date '{}' is not an ISO 8601 date.", iso_date));
    const std::chrono::year_month_day ymd{std::chrono::year{y}, std::chrono::month{m},
                                          std::chrono::day{d}};
    if (!ymd.ok())
        throw std::invalid_argument(
            std::format("The run date '{}' is not an ISO 8601 date.", iso_date));
    std::chrono::sys_days day{ymd};
    do {
        day -= std::chrono::days{1};
    } while (std::chrono::weekday{day} == std::chrono::Saturday ||
             std::chrono::weekday{day} == std::chrono::Sunday);
    return std::format("{:%F}", day);
}

// The date a run values the portfolio at.
std::string as_of(const domain::risk_report_config& risk, const std::string& run_date) {
    if (risk.market_data_type == "date")
        return risk.market_data_date;
    return risk.market_data_type == "eod" ? previous_weekday(run_date) : run_date;
}

}

messaging::run_document generate_run_document(const run_document_inputs& in) {
    if (std::ranges::none_of(in.analytics, [](const auto& a) { return a.analytic.active == "Y"; }))
        throw std::invalid_argument(
            "The risk report configuration switches no analytic on. An ORE run needs at "
            "least one.");

    messaging::run_document r;
    r.settings = in.risk;
    r.settings.market_data_date = as_of(in.risk, in.run_date);
    r.settings.market_data_type = "date";
    for (const auto& type : in.bound_types) {
        // The risk configuration fills no file: the run document is built from it.
        if (type == "risk_report_config")
            continue;
        const auto* t = setup_file_type(in.file_types, type);
        if (t == nullptr)
            throw std::invalid_argument(
                std::format("No file type names the document of the {} configuration the "
                            "definition binds.",
                            type));
        r.files.push_back({t->ore_parameter, t->name_pattern});
    }
    r.analytics = in.analytics;
    for (auto& a : r.analytics)
        if (is_on(a))
            name_default_files(in.file_types, in.analytics, a);
    r.market_bindings = in.market_bindings;
    return r;
}

}
