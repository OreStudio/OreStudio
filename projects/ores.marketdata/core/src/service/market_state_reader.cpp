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
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.ore.core/results/calibration_reader.hpp"
#include "ores.ore.core/results/metric_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <format>
#include <optional>
#include <set>
#include <tuple>

namespace ores::marketdata::service {
namespace {

/**
 * @brief The date a line states, or the run's own date when it states none.
 */
std::optional<std::chrono::system_clock::time_point>
instant_of(const std::string& stated, const std::chrono::system_clock::time_point& run_date) {
    if (stated.empty())
        return run_date;
    try {
        return std::chrono::sys_days(ores::platform::time::datetime::from_iso8601_date(stated));
    } catch (const std::exception&) {
    }
    try {
        return ores::platform::time::datetime::from_db_string(stated);
    } catch (const std::exception&) {
        return std::nullopt;
    }
}

void read_scenarios(const market_state_file& file,
                    const ores::ore::results::output_family& family,
                    const market_state_run& run,
                    market_state_outcome& outcome) {
    auto read = ores::ore::results::read_metrics(file.body, family);
    for (const auto& refusal : read.refusals)
        outcome.refusals.push_back(file.name + ": " + refusal);
    if (!read.refusals.empty())
        return;

    for (auto& one : read.metrics) {
        const auto as_of = instant_of(one.as_of, run.as_of);
        if (!as_of) {
            outcome.refusals.push_back(
                std::format("{}: line {} states the date '{}', which is not a date.",
                            file.name,
                            one.line,
                            one.as_of));
            return;
        }
        outcome.scenario_values.push_back(
            domain::market_scenario_value{.id = boost::uuids::random_generator()(),
                                          .party_id = run.party_id,
                                          .report_instance_id = run.report_instance_id,
                                          .result_id = file.result_id,
                                          .source = file.name,
                                          .line = one.line,
                                          .as_of = *as_of,
                                          .factor = std::move(one.measure),
                                          .report_definition_id = run.report_definition_id,
                                          .family = family.code,
                                          .scenario = std::move(one.scenario),
                                          .value = one.value});
    }
}

void read_calibrations(const market_state_file& file,
                       const ores::ore::results::output_family& family,
                       const market_state_run& run,
                       market_state_outcome& outcome) {
    auto read = ores::ore::results::read_calibration(file.body, family);
    for (const auto& refusal : read.refusals)
        outcome.refusals.push_back(file.name + ": " + refusal);
    if (!read.refusals.empty())
        return;

    for (auto& row : read.rows) {
        outcome.calibration_values.push_back(
            domain::market_calibration_value{.id = boost::uuids::random_generator()(),
                                             .party_id = run.party_id,
                                             .report_instance_id = run.report_instance_id,
                                             .result_id = file.result_id,
                                             .source = file.name,
                                             .line = row.line,
                                             .as_of = run.as_of,
                                             .report_definition_id = run.report_definition_id,
                                             .market_object_type = std::move(row.market_object_type),
                                             .market_object_id = std::move(row.market_object_id),
                                             .result_name = std::move(row.result_name),
                                             .result_key_1 = std::move(row.result_key_1),
                                             .result_key_2 = std::move(row.result_key_2),
                                             .result_key_3 = std::move(row.result_key_3),
                                             .result_type = std::move(row.result_type),
                                             .value = std::move(row.value),
                                             .figure = row.figure});
    }
}

}

market_state_outcome read_market_state(const std::vector<market_state_file>& files,
                                       const market_state_run& run) {
    market_state_outcome outcome;
    std::set<std::tuple<boost::uuids::uuid, std::string>> stated;

    for (const auto& file : files) {
        const auto* family = ores::ore::results::family_of_file(file.name);
        if (family == nullptr || !ores::ore::results::is_market_state(*family)) {
            outcome.left_alone.push_back(file.name);
            continue;
        }

        // A value is keyed by its result, its file and its line, so a second file
        // of the same name would close the first in the same statement. It is
        // refused here, where the reason can be named.
        if (!stated.emplace(file.result_id, file.name).second) {
            outcome.refusals.push_back(file.name + ": the result states this file twice.");
            continue;
        }

        if (ores::ore::results::is_calibration_family(*family))
            read_calibrations(file, *family, run, outcome);
        else
            read_scenarios(file, *family, run, outcome);
        outcome.families.push_back(family->code);
    }

    return outcome;
}

}
