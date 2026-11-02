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
#include "ores.reporting.core/results/run_output_reader.hpp"
#include "ores.ore.core/results/cashflow_reader.hpp"
#include "ores.ore.core/results/metric_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/text_value_reader.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/json.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <chrono>
#include <format>
#include <iterator>
#include <limits>
#include <optional>

namespace ores::reporting::results {
namespace {

/**
 * @brief The instant a file states, or the run's own date when it states none.
 *
 * A family states its observation as a date, and a file that states none is one
 * row for the whole run: the run's priced date is then the observation.
 */
bool instant_of(const std::string& stated,
                const std::chrono::system_clock::time_point& priced_at,
                std::chrono::system_clock::time_point& out) {
    if (stated.empty()) {
        out = priced_at;
        return true;
    }
    try {
        const auto date = ores::platform::time::datetime::from_iso8601_date(stated);
        out = std::chrono::sys_days(date);
        return true;
    } catch (const std::exception&) {
    }
    try {
        out = ores::platform::time::datetime::from_db_string(stated);
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

/**
 * @brief The qualifiers as one JSON object, or empty when the row states none.
 *
 * The names are the engine's headings and the values are the engine's text, so
 * the object is stored as written and interpreted by whoever reads the row.
 */
std::string qualifiers_json(const std::vector<std::pair<std::string, std::string>>& qualifiers) {
    if (qualifiers.empty())
        return {};
    boost::json::object object;
    for (const auto& [name, value] : qualifiers)
        object[name] = value;
    return boost::json::serialize(object);
}

}

void read_run_output(const output_file& file,
                     const run_stamp& run,
                     std::size_t block_size,
                     const run_output_sinks& sinks,
                     run_output_outcome& outcome) {
    const auto* family = ores::ore::results::family_of_file(file.name);
    if (family == nullptr) {
        outcome.files_not_modelled.push_back(file.name);
        return;
    }
    if (!run.families.stores(family->code)) {
        outcome.files_skipped.push_back(file.name);
        return;
    }

    if (ores::ore::results::is_cashflow_family(*family)) {
        std::vector<domain::report_cashflow> block;
        const auto refusals = ores::ore::results::read_cashflows_in_blocks(
            file.body, *family, block_size, [&](std::vector<ores::ore::results::cashflow_row>& rows) {
                block.clear();
                block.reserve(rows.size());
                for (auto& row : rows) {
                    std::chrono::system_clock::time_point as_of;
                    if (!instant_of(row.as_of, run.priced_at, as_of)) {
                        outcome.refusals.push_back(std::format(
                            "{}: the cashflow {} of trade {} states the observation '{}', "
                            "which is not a date.",
                            file.name,
                            row.cashflow_no,
                            row.subject,
                            row.as_of));
                        continue;
                    }
                    std::chrono::system_clock::time_point pay_date;
                    std::optional<std::chrono::system_clock::time_point> pay;
                    if (instant_of(row.pay_date, run.priced_at, pay_date) && !row.pay_date.empty())
                        pay = pay_date;

                    block.push_back(
                        domain::report_cashflow{.id = boost::uuids::random_generator()(),
                                                .party_id = run.party_id,
                                                .report_instance_id = run.report_instance_id,
                                                .family = family->code,
                                                .subject_type = std::move(row.subject_type),
                                                .subject_id = std::move(row.subject),
                                                .scenario = std::move(row.scenario),
                                                .cashflow_no = std::move(row.cashflow_no),
                                                .leg_no = std::move(row.leg_no),
                                                .as_of = as_of,
                                                .measure = std::move(row.measure),
                                                .part = std::move(row.part),
                                                .report_definition_id = run.report_definition_id,
                                                .pay_date = pay,
                                                .flow_type = std::move(row.flow_type),
                                                .currency = std::move(row.currency),
                                                .qualifiers = qualifiers_json(row.qualifiers),
                                                .value = row.value,
                                                .source = family->code});
                }
                if (!block.empty())
                    sinks.cashflows(block);
            });
        for (const auto& refusal : refusals)
            outcome.refusals.push_back(file.name + ": " + refusal);
        outcome.files_read.push_back(file.name);
        return;
    }

    if (ores::ore::results::is_text_value_family(*family)) {
        auto read = ores::ore::results::read_text_values(file.body, *family);
        for (const auto& refusal : read.refusals)
            outcome.refusals.push_back(file.name + ": " + refusal);
        std::vector<domain::report_text_value> block;
        for (auto& row : read.rows) {
            block.push_back(domain::report_text_value{.id = boost::uuids::random_generator()(),
                                                      .party_id = run.party_id,
                                                      .report_instance_id = run.report_instance_id,
                                                      .result_id = run.result_id,
                                                      .source = file.name,
                                                      .line = row.line,
                                                      .as_of = run.priced_at,
                                                      .report_definition_id =
                                                          run.report_definition_id,
                                                      .family = family->code,
                                                      .subject_type = std::move(row.subject_type),
                                                      .subject_id = std::move(row.subject),
                                                      .name = std::move(row.name),
                                                      .value_type = std::move(row.value_type),
                                                      .value = std::move(row.value),
                                                      .figure = row.figure});
            if (block.size() >= block_size) {
                sinks.text_values(block);
                block.clear();
            }
        }
        if (!block.empty())
            sinks.text_values(block);
        outcome.files_read.push_back(file.name);
        return;
    }

    std::vector<domain::report_metric> block;
    const auto refusals = ores::ore::results::read_metrics_in_blocks(
        file.body, *family, block_size, [&](std::vector<ores::ore::results::metric>& metrics) {
            block.clear();
            block.reserve(metrics.size());
            for (auto& one : metrics) {
                std::chrono::system_clock::time_point as_of;
                if (!instant_of(one.as_of, run.priced_at, as_of)) {
                    outcome.refusals.push_back(
                        std::format("{}: the metric {} of {} states the observation '{}', which "
                                    "is not a date.",
                                    file.name,
                                    one.measure,
                                    one.subject,
                                    one.as_of));
                    continue;
                }
                block.push_back(
                    domain::report_metric{.id = boost::uuids::random_generator()(),
                                          .party_id = run.party_id,
                                          .report_instance_id = run.report_instance_id,
                                          .result_id = run.result_id,
                                          .as_of = as_of,
                                          .measure = std::move(one.measure),
                                          .source = file.name,
                                          .line = one.line,
                                          .report_definition_id = run.report_definition_id,
                                          .family = family->code,
                                          .subject_type = std::move(one.subject_type),
                                          .subject_id = std::move(one.subject),
                                          .currency = std::move(one.currency),
                                          .scenario = std::move(one.scenario),
                                          .factor = std::move(one.factor),
                                          .qualifiers = qualifiers_json(one.qualifiers),
                                          .value = one.value});
            }
            if (!block.empty())
                sinks.metrics(block);
        });
    for (const auto& refusal : refusals)
        outcome.refusals.push_back(file.name + ": " + refusal);
    outcome.files_read.push_back(file.name);
}

run_output_outcome read_run_outputs(const std::vector<output_file>& files, const run_stamp& run) {
    run_output_outcome outcome;
    const auto append = [](auto& into) {
        return [&into](auto& block) {
            into.insert(into.end(),
                        std::make_move_iterator(block.begin()),
                        std::make_move_iterator(block.end()));
        };
    };
    const run_output_sinks sinks{.metrics = append(outcome.metrics),
                                 .cashflows = append(outcome.cashflows),
                                 .text_values = append(outcome.text_values)};
    for (const auto& file : files)
        read_run_output(file, run, std::numeric_limits<std::size_t>::max(), sinks, outcome);
    return outcome;
}

}
