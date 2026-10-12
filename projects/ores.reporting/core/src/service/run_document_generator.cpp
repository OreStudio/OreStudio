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

struct input_file {
    const char* type_code;
    const char* parameter;
    const char* file;
};

constexpr input_file input_files[] = {
    {"conventions", "conventionsFile", "conventions.xml"},
    {"curve_configuration", "curveConfigFile", "curveconfig.xml"},
    {"todays_market", "marketConfigFile", "todaysmarket.xml"},
    {"pricing_engines", "pricingEnginesFile", "pricingengine.xml"},
};

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
    for (const auto& f : input_files) {
        const auto bound = std::ranges::find(in.bound_types, f.type_code) != in.bound_types.end();
        if (bound)
            r.files.push_back({f.parameter, f.file});
    }
    r.analytics = in.analytics;
    r.market_bindings = in.market_bindings;
    return r;
}

}
