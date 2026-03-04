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
#include "ores.mq/repository/queue_stats_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::mq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(
        "ores.mq.repository.queue_stats_repository");
    return instance;
}

/**
 * @brief Parses a PostgreSQL timestamptz string into a system_clock::time_point.
 */
std::chrono::system_clock::time_point parse_pg_timestamp(const std::string& s) {
    if (s.empty()) return {};

    std::string base = s;
    const auto dot = base.find('.');
    if (dot != std::string::npos) {
        base = base.substr(0, dot);
    } else {
        const auto tz = base.find_first_of("+-", 11);
        if (tz != std::string::npos) {
            base = base.substr(0, tz);
        }
    }

    try {
        return ores::platform::time::datetime::parse_time_point_utc(base);
    } catch (const std::exception&) {
        return {};
    }
}

/**
 * @brief Parses a row from a queue stats query into a queue_stats.
 *
 * Expected column order:
 *   recorded_at(0), queue_id(1), tenant_id(2), party_id(3),
 *   pending_count(4), processing_count(5), total_archived(6)
 */
domain::queue_stats parse_stats_row(
    const std::vector<std::optional<std::string>>& row) {

    domain::queue_stats stats;
    if (row.size() < 7) return stats;

    stats.recorded_at = parse_pg_timestamp(row[0].value_or(""));

    if (row[1]) {
        try {
            stats.queue_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
        } catch (...) {}
    }

    if (row[2]) {
        try {
            stats.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*row[2]);
        } catch (...) {}
    }

    if (row[3]) {
        try {
            stats.party_id = boost::lexical_cast<boost::uuids::uuid>(*row[3]);
        } catch (...) {}
    }

    try { stats.pending_count    = std::stoll(row[4].value_or("0")); } catch (...) {}
    try { stats.processing_count = std::stoll(row[5].value_or("0")); } catch (...) {}
    try { stats.total_archived   = std::stoll(row[6].value_or("0")); } catch (...) {}

    return stats;
}

} // anonymous namespace

// ---------------------------------------------------------------------------
// queue_stats_repository
// ---------------------------------------------------------------------------

std::vector<domain::queue_stats> queue_stats_repository::read_latest(
    context ctx) {

    BOOST_LOG_SEV(lg(), debug) << "Reading latest queue stats.";

    const std::string sql =
        "SELECT recorded_at::text, queue_id::text, "
        "tenant_id::text, party_id::text, "
        "pending_count::text, processing_count::text, total_archived::text "
        "FROM ores_mq_queue_stats_latest_fn()";

    auto rows = execute_raw_multi_column_query(ctx, sql,
        lg(), "Reading latest queue stats");

    std::vector<domain::queue_stats> result;
    result.reserve(rows.size());
    for (const auto& row : rows)
        result.push_back(parse_stats_row(row));
    return result;
}

std::vector<domain::queue_stats> queue_stats_repository::read_samples(
    context ctx, const boost::uuids::uuid& queue_id,
    std::optional<std::chrono::system_clock::time_point> from,
    std::optional<std::chrono::system_clock::time_point> to) {

    BOOST_LOG_SEV(lg(), debug) << "Reading queue stats samples for: "
                               << queue_id;

    const std::string from_str = from
        ? ores::platform::time::datetime::format_time_point_utc(*from) : "";
    const std::string to_str = to
        ? ores::platform::time::datetime::format_time_point_utc(*to) : "";

    const std::string sql =
        "SELECT recorded_at::text, queue_id::text, "
        "tenant_id::text, party_id::text, "
        "pending_count::text, processing_count::text, total_archived::text "
        "FROM ores_mq_queue_stats_samples_fn("
        "$1::uuid, "
        "NULLIF($2, '')::timestamptz, "
        "NULLIF($3, '')::timestamptz)";

    auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {boost::uuids::to_string(queue_id), from_str, to_str},
        lg(), "Reading queue stats samples");

    std::vector<domain::queue_stats> result;
    result.reserve(rows.size());
    for (const auto& row : rows)
        result.push_back(parse_stats_row(row));
    return result;
}

}
