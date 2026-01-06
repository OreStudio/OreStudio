/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/repository/telemetry_repository.hpp"

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.telemetry/repository/telemetry_mapper.hpp"
#include "ores.telemetry/repository/telemetry_entity.hpp"

namespace ores::telemetry::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

telemetry_repository::telemetry_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::string telemetry_repository::sql() {
    return generate_create_table_sql<telemetry_entity>(lg());
}

void telemetry_repository::create(const domain::telemetry_log_entry& entry) {
    BOOST_LOG_SEV(lg(), trace) << "Creating telemetry log entry: "
                               << boost::uuids::to_string(entry.id);

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(telemetry_mapper::to_entity(entry)))
        .and_then(commit);
    ensure_success(r, lg());
}

std::size_t telemetry_repository::create_batch(
    const domain::telemetry_batch& batch) {

    if (batch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Empty batch, nothing to insert";
        return 0;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating batch of " << batch.size()
                               << " telemetry log entries from "
                               << batch.source_name;

    std::vector<telemetry_entity> entities;
    entities.reserve(batch.size());
    for (const auto& entry : batch.entries) {
        auto entity = telemetry_mapper::to_entity(entry);
        // Override source info from batch
        entity.source = std::string(domain::to_string(batch.source));
        entity.source_name = batch.source_name;
        entities.push_back(std::move(entity));
    }

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(entities))
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Batch inserted successfully";
    return entities.size();
}

std::vector<domain::telemetry_log_entry>
telemetry_repository::query(const domain::telemetry_query& q) {
    BOOST_LOG_SEV(lg(), debug) << "Querying telemetry logs";

    const auto start_ts = timepoint_to_timestamp(q.start_time, lg());
    const auto end_ts = timepoint_to_timestamp(q.end_time, lg());

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // For now, we only filter by time range (required for hypertable efficiency).
    // Optional filters (source, level, etc.) would require raw SQL queries.
    // Use a large default limit when not specified (sqlgen type changes with limit).
    constexpr std::size_t default_limit = 10000;
    const auto effective_limit = q.limit > 0
        ? static_cast<std::size_t>(q.limit)
        : default_limit;

    const auto query = sqlgen::read<std::vector<telemetry_entity>> |
        where("timestamp"_c >= start_ts && "timestamp"_c < end_ts) |
        order_by("timestamp"_c.desc()) |
        limit(effective_limit);

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    // Map entities to domain objects and filter in memory if needed
    std::vector<domain::telemetry_log_entry> result;
    result.reserve(r->size());
    for (const auto& entity : *r) {
        // Apply in-memory filtering for optional criteria
        if (q.source.has_value()) {
            const auto source_str = std::string(domain::to_string(*q.source));
            if (entity.source != source_str) continue;
        }
        if (q.source_name.has_value() && entity.source_name != *q.source_name) {
            continue;
        }
        if (q.session_id.has_value()) {
            const auto session_str = boost::lexical_cast<std::string>(*q.session_id);
            if (entity.session_id != session_str) continue;
        }
        if (q.account_id.has_value()) {
            const auto account_str = boost::lexical_cast<std::string>(*q.account_id);
            if (entity.account_id != account_str) continue;
        }
        if (q.level.has_value() && entity.level != *q.level) {
            continue;
        }
        if (q.component.has_value() && entity.component != *q.component) {
            continue;
        }
        if (q.tag.has_value() && entity.tag != *q.tag) {
            continue;
        }

        result.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Query returned " << result.size() << " entries";
    return result;
}

std::uint64_t telemetry_repository::count(const domain::telemetry_query& q) {
    BOOST_LOG_SEV(lg(), debug) << "Counting telemetry logs";

    const auto start_ts = timepoint_to_timestamp(q.start_time, lg());
    const auto end_ts = timepoint_to_timestamp(q.end_time, lg());

    struct count_result {
        long long count;
    };

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // Count uses same time range filter as query(); optional filters applied in memory.
    auto count_query = sqlgen::select_from<telemetry_entity>(
        sqlgen::count().as<"count">()) |
        where("timestamp"_c >= start_ts && "timestamp"_c < end_ts) |
        sqlgen::to<count_result>;

    const auto r = ctx_.single_connection().and_then(count_query);
    ensure_success(r, lg());

    // For accurate count with optional filters, we'd need to query() and count results.
    // For now, return total count in time range (filters applied in query() method).
    BOOST_LOG_SEV(lg(), debug) << "Count: " << r->count;
    return static_cast<std::uint64_t>(r->count);
}

std::vector<domain::telemetry_log_entry>
telemetry_repository::read_by_session(const boost::uuids::uuid& session_id,
    std::uint32_t limit_count) {

    BOOST_LOG_SEV(lg(), debug) << "Reading telemetry for session: "
                               << boost::uuids::to_string(session_id);

    const auto session_str = boost::lexical_cast<std::string>(session_id);
    const auto query = sqlgen::read<std::vector<telemetry_entity>> |
        where("session_id"_c == session_str) |
        order_by("timestamp"_c.desc()) |
        sqlgen::limit(static_cast<std::size_t>(limit_count));

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    std::vector<domain::telemetry_log_entry> result;
    result.reserve(r->size());
    for (const auto& entity : *r) {
        result.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " log entries";
    return result;
}

std::vector<domain::telemetry_log_entry>
telemetry_repository::read_by_account(const boost::uuids::uuid& account_id,
    const std::chrono::system_clock::time_point& start,
    const std::chrono::system_clock::time_point& end,
    std::uint32_t limit_count) {

    BOOST_LOG_SEV(lg(), debug) << "Reading telemetry for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_str = boost::lexical_cast<std::string>(account_id);
    const auto start_ts = timepoint_to_timestamp(start, lg());
    const auto end_ts = timepoint_to_timestamp(end, lg());

    const auto query = sqlgen::read<std::vector<telemetry_entity>> |
        where("account_id"_c == account_str &&
              "timestamp"_c >= start_ts && "timestamp"_c < end_ts) |
        order_by("timestamp"_c.desc()) |
        sqlgen::limit(static_cast<std::size_t>(limit_count));

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    std::vector<domain::telemetry_log_entry> result;
    result.reserve(r->size());
    for (const auto& entity : *r) {
        result.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " log entries";
    return result;
}

std::vector<domain::telemetry_stats>
telemetry_repository::read_hourly_stats(const domain::telemetry_stats_query& q) {
    BOOST_LOG_SEV(lg(), debug) << "Reading hourly telemetry stats";

    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // Query by time range only, apply optional filters in memory.
    const auto query = sqlgen::read<std::vector<telemetry_stats_hourly_entity>> |
        where("hour"_c >= start_str && "hour"_c < end_str) |
        order_by("hour"_c.desc());

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    // Apply in-memory filtering for optional criteria
    std::vector<domain::telemetry_stats> result;
    result.reserve(r->size());
    for (const auto& entity : *r) {
        if (q.source.has_value()) {
            const auto source_str = std::string(domain::to_string(*q.source));
            if (entity.source != source_str) continue;
        }
        if (q.source_name.has_value() && entity.source_name != *q.source_name) {
            continue;
        }
        if (q.level.has_value() && entity.level != *q.level) {
            continue;
        }

        result.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " hourly stats";
    return result;
}

std::vector<domain::telemetry_stats>
telemetry_repository::read_daily_stats(const domain::telemetry_stats_query& q) {
    BOOST_LOG_SEV(lg(), debug) << "Reading daily telemetry stats";

    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // Query by time range only, apply optional filters in memory.
    const auto query = sqlgen::read<std::vector<telemetry_stats_daily_entity>> |
        where("day"_c >= start_str && "day"_c < end_str) |
        order_by("day"_c.desc());

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    // Apply in-memory filtering for optional criteria
    std::vector<domain::telemetry_stats> result;
    result.reserve(r->size());
    for (const auto& entity : *r) {
        if (q.source.has_value()) {
            const auto source_str = std::string(domain::to_string(*q.source));
            if (entity.source != source_str) continue;
        }
        if (q.source_name.has_value() && entity.source_name != *q.source_name) {
            continue;
        }
        if (q.component.has_value() && entity.component != *q.component) {
            continue;
        }
        if (q.level.has_value() && entity.level != *q.level) {
            continue;
        }

        result.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " daily stats";
    return result;
}

domain::telemetry_summary
telemetry_repository::get_summary(std::uint32_t hours) {
    BOOST_LOG_SEV(lg(), debug) << "Getting telemetry summary for last "
                               << hours << " hours";

    domain::telemetry_summary summary;
    summary.end_time = std::chrono::system_clock::now();
    summary.start_time = summary.end_time - std::chrono::hours(hours);

    const auto start_ts = timepoint_to_timestamp(summary.start_time, lg());
    const auto end_ts = timepoint_to_timestamp(summary.end_time, lg());

    // Count total logs in time range
    struct count_result {
        long long count;
    };

    auto total_query = sqlgen::select_from<telemetry_entity>(
        sqlgen::count().as<"count">()) |
        where("timestamp"_c >= start_ts && "timestamp"_c < end_ts) |
        sqlgen::to<count_result>;

    auto total_r = ctx_.single_connection().and_then(total_query);
    if (total_r) {
        summary.total_logs = static_cast<std::uint64_t>(total_r->count);
    }

    // Count by level
    const std::vector<std::pair<std::string, std::uint64_t*>> level_targets = {
        {"error", &summary.error_count},
        {"warn", &summary.warn_count},
        {"info", &summary.info_count},
        {"debug", &summary.debug_count},
        {"trace", &summary.trace_count}
    };

    for (const auto& [level, target] : level_targets) {
        auto level_query = sqlgen::select_from<telemetry_entity>(
            sqlgen::count().as<"count">()) |
            where("timestamp"_c >= start_ts && "timestamp"_c < end_ts &&
                  "level"_c == level) |
            sqlgen::to<count_result>;

        auto level_r = ctx_.single_connection().and_then(level_query);
        if (level_r) {
            *target = static_cast<std::uint64_t>(level_r->count);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Summary: " << summary.total_logs
                               << " total, " << summary.error_count << " errors";
    return summary;
}

std::uint64_t telemetry_repository::count_errors(const std::string& source_name,
    std::uint32_t hours) {

    BOOST_LOG_SEV(lg(), debug) << "Counting errors for " << source_name
                               << " in last " << hours << " hours";

    const auto end_time = std::chrono::system_clock::now();
    const auto start_time = end_time - std::chrono::hours(hours);
    const auto start_ts = timepoint_to_timestamp(start_time, lg());
    const auto end_ts = timepoint_to_timestamp(end_time, lg());

    struct count_result {
        long long count;
    };

    auto query = sqlgen::select_from<telemetry_entity>(
        sqlgen::count().as<"count">()) |
        where("timestamp"_c >= start_ts && "timestamp"_c < end_ts &&
              "source_name"_c == source_name && "level"_c == "error") |
        sqlgen::to<count_result>;

    const auto r = ctx_.single_connection().and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Error count: " << r->count;
    return static_cast<std::uint64_t>(r->count);
}

std::uint64_t telemetry_repository::delete_old_logs(
    const std::chrono::system_clock::time_point& older_than) {

    BOOST_LOG_SEV(lg(), info) << "Deleting telemetry logs older than cutoff";

    const auto older_ts = timepoint_to_timestamp(older_than, lg());
    const auto query = sqlgen::delete_from<telemetry_entity> |
        where("timestamp"_c < older_ts);

    execute_delete_query(ctx_, query, lg(), "deleting old telemetry logs");

    // Note: We don't easily get the count from sqlgen delete.
    // With TimescaleDB, this should be handled by retention policies anyway.
    return 0;
}

}
