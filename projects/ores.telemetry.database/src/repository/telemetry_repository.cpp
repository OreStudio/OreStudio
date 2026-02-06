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
#include "ores.telemetry.database/repository/telemetry_repository.hpp"

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.telemetry/log/skip_telemetry_guard.hpp"
#include "ores.telemetry.database/repository/telemetry_mapper.hpp"
#include "ores.telemetry.database/repository/telemetry_entity.hpp"

namespace ores::telemetry::database::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string telemetry_repository::sql() {
    return generate_create_table_sql<telemetry_entity>(lg());
}

void telemetry_repository::create(context ctx,
    const domain::telemetry_log_entry& entry) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), trace) << "Creating telemetry log entry: "
                               << boost::uuids::to_string(entry.id);

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(telemetry_mapper::to_entity(entry, ctx.tenant_id().to_string())))
        .and_then(commit);
    ensure_success(r, lg());
}

std::size_t telemetry_repository::create_batch(context ctx,
    const domain::telemetry_batch& batch) {
    ores::telemetry::log::skip_telemetry_guard guard;

    if (batch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Empty batch, nothing to insert";
        return 0;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating batch of " << batch.size()
                               << " telemetry log entries from "
                               << batch.source_name;

    const auto tenant_id_str = ctx.tenant_id().to_string();
    std::vector<telemetry_entity> entities;
    entities.reserve(batch.size());
    for (const auto& entry : batch.entries) {
        auto entity = telemetry_mapper::to_entity(entry, tenant_id_str);
        // Override source info from batch
        entity.source = std::string(domain::to_string(batch.source));
        entity.source_name = batch.source_name;
        entities.push_back(std::move(entity));
    }

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(entities))
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Batch inserted successfully";
    return entities.size();
}

namespace {

/**
 * @brief Helper to escape single quotes in SQL strings.
 */
std::string escape_sql_string(const std::string& s) {
    std::string result;
    result.reserve(s.size());
    for (char c : s) {
        if (c == '\'') {
            result += "''";
        } else {
            result += c;
        }
    }
    return result;
}

/**
 * @brief Build dynamic WHERE clause for telemetry queries.
 *
 * All filters are applied at the database level for correct results
 * with LIMIT/OFFSET pagination.
 */
std::string build_where_clause(const domain::telemetry_query& q,
    const std::string& start_ts, const std::string& end_ts) {

    std::string where_clause = std::format(
        "timestamp >= '{}' AND timestamp < '{}'", start_ts, end_ts);

    if (q.source.has_value()) {
        where_clause += std::format(" AND source = '{}'",
            escape_sql_string(std::string(domain::to_string(*q.source))));
    }
    if (q.source_name.has_value()) {
        where_clause += std::format(" AND source_name = '{}'",
            escape_sql_string(*q.source_name));
    }
    if (q.session_id.has_value()) {
        where_clause += std::format(" AND session_id = '{}'",
            boost::lexical_cast<std::string>(*q.session_id));
    }
    if (q.account_id.has_value()) {
        where_clause += std::format(" AND account_id = '{}'",
            boost::lexical_cast<std::string>(*q.account_id));
    }
    if (q.level.has_value()) {
        where_clause += std::format(" AND level = '{}'",
            escape_sql_string(*q.level));
    }
    if (q.min_level.has_value()) {
        // Severity ordering: error > warn > info > debug > trace
        // Use CASE expression for proper ordering
        where_clause += std::format(
            " AND CASE level "
            "WHEN 'error' THEN 5 "
            "WHEN 'warn' THEN 4 "
            "WHEN 'info' THEN 3 "
            "WHEN 'debug' THEN 2 "
            "WHEN 'trace' THEN 1 "
            "ELSE 0 END >= "
            "CASE '{}' "
            "WHEN 'error' THEN 5 "
            "WHEN 'warn' THEN 4 "
            "WHEN 'info' THEN 3 "
            "WHEN 'debug' THEN 2 "
            "WHEN 'trace' THEN 1 "
            "ELSE 0 END",
            escape_sql_string(*q.min_level));
    }
    if (q.component.has_value()) {
        // Support prefix matching for component
        where_clause += std::format(" AND component LIKE '{}%'",
            escape_sql_string(*q.component));
    }
    if (q.tag.has_value()) {
        where_clause += std::format(" AND tag = '{}'",
            escape_sql_string(*q.tag));
    }
    if (q.message_contains.has_value()) {
        // Case-insensitive substring search
        where_clause += std::format(" AND message ILIKE '%{}%'",
            escape_sql_string(*q.message_contains));
    }

    return where_clause;
}

}

std::vector<domain::telemetry_log_entry>
telemetry_repository::query(context ctx, const domain::telemetry_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Querying telemetry logs";

    const auto start_ts = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_ts = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);
    const auto where_clause = build_where_clause(q, start_ts, end_ts);

    constexpr std::uint32_t default_limit = 1000;
    const auto effective_limit = q.limit > 0 ? q.limit : default_limit;

    // Build complete SQL query with all filters applied at database level
    const auto sql = std::format(
        "SELECT id, timestamp, source, source_name, session_id, account_id, "
        "level, component, message, tag, recorded_at "
        "FROM ores_telemetry_logs_tbl "
        "WHERE {} "
        "ORDER BY timestamp DESC "
        "LIMIT {} OFFSET {}",
        where_clause, effective_limit, q.offset);

    BOOST_LOG_SEV(lg(), trace) << "Executing query: " << sql;

    // Use raw SQL execution via bitemporal_operations helper
    auto rows = execute_raw_multi_column_query(ctx, sql, lg(), "Querying telemetry logs");

    std::vector<domain::telemetry_log_entry> entries;
    entries.reserve(rows.size());

    // Column indices: 0=id, 1=timestamp, 2=source, 3=source_name, 4=session_id,
    //                 5=account_id, 6=level, 7=component, 8=message, 9=tag, 10=recorded_at
    for (const auto& row : rows) {
        if (row.size() < 11) continue;

        telemetry_entity entity;
        entity.id = row[0].value_or("");
        entity.timestamp = row[1].value_or("");
        entity.source = row[2].value_or("");
        entity.source_name = row[3].value_or("");
        entity.session_id = row[4].has_value() && !row[4]->empty()
            ? std::optional<std::string>(*row[4])
            : std::nullopt;
        entity.account_id = row[5].has_value() && !row[5]->empty()
            ? std::optional<std::string>(*row[5])
            : std::nullopt;
        entity.level = row[6].value_or("");
        entity.component = row[7].value_or("");
        entity.message = row[8].value_or("");
        entity.tag = row[9].value_or("");
        entity.recorded_at = row[10].value_or("");

        entries.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Query returned " << entries.size() << " entries";
    return entries;
}

std::uint64_t telemetry_repository::count(context ctx,
    const domain::telemetry_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Counting telemetry logs";

    const auto start_ts = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_ts = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);
    const auto where_clause = build_where_clause(q, start_ts, end_ts);

    // Build count query with same filters as query() for accurate pagination
    const auto sql = std::format(
        "SELECT COUNT(*) FROM ores_telemetry_logs_tbl WHERE {}",
        where_clause);

    BOOST_LOG_SEV(lg(), trace) << "Executing count: " << sql;

    // Use raw SQL execution via bitemporal_operations helper
    auto results = execute_raw_string_query(ctx, sql, lg(), "Counting telemetry logs");

    if (results.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Count query returned no results";
        return 0;
    }

    const auto count = std::stoull(results[0]);
    BOOST_LOG_SEV(lg(), debug) << "Count: " << count;
    return count;
}

std::vector<domain::telemetry_log_entry>
telemetry_repository::read_by_session(context ctx,
    const boost::uuids::uuid& session_id,
    std::uint32_t limit_count) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), debug) << "Reading telemetry for session: "
                               << boost::uuids::to_string(session_id);

    const auto session_str = boost::lexical_cast<std::string>(session_id);
    const auto query = sqlgen::read<std::vector<telemetry_entity>> |
        where("session_id"_c == session_str) |
        order_by("timestamp"_c.desc()) |
        sqlgen::limit(static_cast<std::size_t>(limit_count));

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
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
telemetry_repository::read_by_account(context ctx,
    const boost::uuids::uuid& account_id,
    const std::chrono::system_clock::time_point& start,
    const std::chrono::system_clock::time_point& end,
    std::uint32_t limit_count) {
    ores::telemetry::log::skip_telemetry_guard guard;

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

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
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
telemetry_repository::read_hourly_stats(context ctx,
    const domain::telemetry_stats_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Reading hourly telemetry stats";

    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // Query by time range only, apply optional filters in memory.
    const auto query = sqlgen::read<std::vector<telemetry_stats_hourly_entity>> |
        where("hour"_c >= start_str && "hour"_c < end_str) |
        order_by("hour"_c.desc());

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
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
telemetry_repository::read_daily_stats(context ctx,
    const domain::telemetry_stats_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Reading daily telemetry stats";

    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.start_time);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", q.end_time);

    // NOTE: sqlgen doesn't support dynamically adding where() clauses.
    // Query by time range only, apply optional filters in memory.
    const auto query = sqlgen::read<std::vector<telemetry_stats_daily_entity>> |
        where("day"_c >= start_str && "day"_c < end_str) |
        order_by("day"_c.desc());

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
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
telemetry_repository::get_summary(context ctx, std::uint32_t hours) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Getting telemetry summary for last "
                               << hours << " hours";

    domain::telemetry_summary summary;
    summary.end_time = std::chrono::system_clock::now();
    summary.start_time = summary.end_time - std::chrono::hours(hours);

    const auto start_ts = timepoint_to_timestamp(summary.start_time, lg());
    const auto end_ts = timepoint_to_timestamp(summary.end_time, lg());

    struct count_result {
        long long count;
    };

    auto total_query = sqlgen::select_from<telemetry_entity>(
        sqlgen::count().as<"count">()) |
        where("timestamp"_c >= start_ts && "timestamp"_c <= end_ts) |
        sqlgen::to<count_result>;

    auto total_r = sqlgen::session(ctx.connection_pool()).and_then(total_query);
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
            where("timestamp"_c >= start_ts && "timestamp"_c <= end_ts &&
                  "level"_c == level) |
            sqlgen::to<count_result>;

        auto level_r = sqlgen::session(ctx.connection_pool()).and_then(level_query);
        if (level_r) {
            *target = static_cast<std::uint64_t>(level_r->count);
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Summary: " << summary.total_logs
                               << " total, " << summary.error_count << " errors";
    return summary;
}

std::uint64_t telemetry_repository::count_errors(context ctx,
    const std::string& source_name, std::uint32_t hours) {
    ores::telemetry::log::skip_telemetry_guard guard;

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

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Error count: " << r->count;
    return static_cast<std::uint64_t>(r->count);
}

std::uint64_t telemetry_repository::delete_old_logs(context ctx,
    const std::chrono::system_clock::time_point& older_than) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), info) << "Deleting telemetry logs older than cutoff";

    const auto older_ts = timepoint_to_timestamp(older_than, lg());
    const auto query = sqlgen::delete_from<telemetry_entity> |
        where("timestamp"_c < older_ts);

    execute_delete_query(ctx, query, lg(), "deleting old telemetry logs");

    // Note: We don't easily get the count from sqlgen delete.
    // With TimescaleDB, this should be handled by retention policies anyway.
    return 0;
}

}
