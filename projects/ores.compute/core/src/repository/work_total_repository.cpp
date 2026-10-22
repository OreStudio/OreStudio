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
#include "ores.compute.core/repository/work_total_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <string>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

std::int64_t micros_of(std::chrono::system_clock::time_point t) {
    return std::chrono::duration_cast<std::chrono::microseconds>(t.time_since_epoch()).count();
}

}

void work_totals::add(const domain::grid_sample& sample) {
    results_inactive += sample.results_inactive;
    results_unsent += sample.results_unsent;
    results_in_progress += sample.results_in_progress;
    results_done += sample.results_done;
    total_workunits += sample.total_workunits;
    total_batches += sample.total_batches;
    active_batches += sample.active_batches;
    outcomes_success += sample.outcomes_success;
    outcomes_client_error += sample.outcomes_client_error;
    outcomes_no_reply += sample.outcomes_no_reply;
}

void work_total_repository::write(context ctx, const work_totals& totals) {
    static const std::string sql = R"(
        INSERT INTO ores_compute_work_totals_tbl (
            id, sampled_at, tenants_counted,
            results_inactive, results_unsent, results_in_progress, results_done,
            total_workunits, total_batches, active_batches,
            outcomes_success, outcomes_client_error, outcomes_no_reply)
        VALUES ($1::uuid, to_timestamp($2::bigint / 1000000.0), $3::int,
            $4::int, $5::int, $6::int, $7::int,
            $8::int, $9::int, $10::int,
            $11::int, $12::int, $13::int)
    )";
    execute_parameterized_command(ctx,
                                  sql,
                                  {boost::uuids::to_string(boost::uuids::random_generator()()),
                                   std::to_string(micros_of(totals.sampled_at)),
                                   std::to_string(totals.tenants_counted),
                                   std::to_string(totals.results_inactive),
                                   std::to_string(totals.results_unsent),
                                   std::to_string(totals.results_in_progress),
                                   std::to_string(totals.results_done),
                                   std::to_string(totals.total_workunits),
                                   std::to_string(totals.total_batches),
                                   std::to_string(totals.active_batches),
                                   std::to_string(totals.outcomes_success),
                                   std::to_string(totals.outcomes_client_error),
                                   std::to_string(totals.outcomes_no_reply)},
                                  lg(),
                                  "writing the work totals of the grid");
}

std::optional<work_totals> work_total_repository::read_newest(context ctx) {
    static const std::string sql = R"(
        SELECT (extract(epoch FROM sampled_at) * 1000000)::bigint, tenants_counted,
            results_inactive, results_unsent, results_in_progress, results_done,
            total_workunits, total_batches, active_batches,
            outcomes_success, outcomes_client_error, outcomes_no_reply
        FROM ores_compute_work_totals_tbl
        ORDER BY sampled_at DESC
        LIMIT 1
    )";
    const auto rows =
        execute_parameterized_multi_column_query(ctx, sql, {}, lg(), "reading the work totals");
    if (rows.empty() || rows.front().size() < 12)
        return std::nullopt;

    const auto& row = rows.front();
    const auto to_int = [](const std::optional<std::string>& v) {
        return v ? std::stoi(*v) : 0;
    };
    work_totals totals;
    totals.sampled_at = std::chrono::system_clock::time_point{
        std::chrono::microseconds{row[0] ? std::stoll(*row[0]) : 0}};
    totals.tenants_counted = to_int(row[1]);
    totals.results_inactive = to_int(row[2]);
    totals.results_unsent = to_int(row[3]);
    totals.results_in_progress = to_int(row[4]);
    totals.results_done = to_int(row[5]);
    totals.total_workunits = to_int(row[6]);
    totals.total_batches = to_int(row[7]);
    totals.active_batches = to_int(row[8]);
    totals.outcomes_success = to_int(row[9]);
    totals.outcomes_client_error = to_int(row[10]);
    totals.outcomes_no_reply = to_int(row[11]);
    return totals;
}

}
