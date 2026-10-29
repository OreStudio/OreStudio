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
#include "ores.database/pq/text.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.telemetry.core/log/skip_telemetry_guard.hpp"
#include "ores.telemetry.database/repository/telemetry_entity.hpp"
#include "ores.telemetry.database/repository/telemetry_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <map>

namespace ores::telemetry::database::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;
using ores::platform::time::datetime;

std::string telemetry_repository::sql() {
    return query_builder::insert_statement_sql<telemetry_entity>(lg());
}

void telemetry_repository::create(context ctx, const messaging::telemetry_log_entry& entry) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), trace) << "Creating telemetry log entry: "
                               << boost::uuids::to_string(entry.id);

    query_builder::execute_write_query(
        ctx,
        telemetry_mapper::to_entity(entry, ctx.tenant_id().to_string()),
        lg(),
        "Creating telemetry log entry");
}

std::size_t telemetry_repository::create_batch(context ctx, const domain::telemetry_batch& batch) {
    ores::telemetry::log::skip_telemetry_guard guard;

    if (batch.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Empty batch, nothing to insert";
        return 0;
    }

    BOOST_LOG_SEV(lg(), debug) << "Creating batch of " << batch.size()
                               << " telemetry log entries from " << batch.source_name;

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

    query_builder::execute_write_query(ctx, entities, lg(), "Creating telemetry log batch");

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
std::string build_where_clause(const messaging::telemetry_query& q,
                               const std::string& start_ts,
                               const std::string& end_ts) {

    std::string where_clause =
        std::format("timestamp >= '{}' AND timestamp < '{}'", start_ts, end_ts);

    if (q.source.has_value()) {
        where_clause += std::format(" AND source = '{}'",
                                    escape_sql_string(std::string(domain::to_string(*q.source))));
    }
    if (q.source_name.has_value()) {
        where_clause += std::format(" AND source_name = '{}'", escape_sql_string(*q.source_name));
    }
    if (q.session_id.has_value()) {
        where_clause +=
            std::format(" AND session_id = '{}'", boost::lexical_cast<std::string>(*q.session_id));
    }
    if (q.account_id.has_value()) {
        where_clause +=
            std::format(" AND account_id = '{}'", boost::lexical_cast<std::string>(*q.account_id));
    }
    if (q.level.has_value()) {
        where_clause += std::format(" AND level = '{}'", escape_sql_string(*q.level));
    }
    if (q.min_level.has_value()) {
        // Severity ordering: error > warn > info > debug > trace
        // Use CASE expression for proper ordering
        where_clause += std::format(" AND CASE level "
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
        where_clause += std::format(" AND component LIKE '{}%'", escape_sql_string(*q.component));
    }
    if (q.tag.has_value()) {
        where_clause += std::format(" AND tag = '{}'", escape_sql_string(*q.tag));
    }
    if (q.message_contains.has_value()) {
        // Case-insensitive substring search
        where_clause +=
            std::format(" AND message ILIKE '%{}%'", escape_sql_string(*q.message_contains));
    }

    return where_clause;
}

}

std::vector<messaging::telemetry_log_entry>
telemetry_repository::query(context ctx, const messaging::telemetry_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Querying telemetry logs";

    const auto start_ts = ores::platform::time::datetime::to_db_string(q.start_time);
    const auto end_ts = ores::platform::time::datetime::to_db_string(q.end_time);
    const auto where_clause = build_where_clause(q, start_ts, end_ts);

    constexpr std::uint32_t default_limit = 1000;
    const auto effective_limit = q.limit > 0 ? q.limit : default_limit;

    // Build complete SQL query with all filters applied at database level
    const auto sql =
        std::format("SELECT id, timestamp, source, source_name, session_id, account_id, "
                    "level, component, message, tag, recorded_at "
                    "FROM ores_telemetry_logs_tbl "
                    "WHERE {} "
                    "ORDER BY timestamp DESC "
                    "LIMIT {} OFFSET {}",
                    where_clause,
                    effective_limit,
                    q.offset);

    BOOST_LOG_SEV(lg(), trace) << "Executing query: " << sql;

    // Use raw SQL execution via bitemporal_operations helper
    auto rows =
        query_builder::execute_raw_multi_column_query(ctx, sql, lg(), "Querying telemetry logs");

    std::vector<messaging::telemetry_log_entry> entries;
    entries.reserve(rows.size());

    // Column indices: 0=id, 1=timestamp, 2=source, 3=source_name, 4=session_id,
    //                 5=account_id, 6=level, 7=component, 8=message, 9=tag, 10=recorded_at
    for (const auto& row : rows) {
        if (row.size() < 11)
            continue;

        telemetry_entity entity;
        entity.id = row[0].value_or("");
        entity.timestamp = ores::database::pq::parse_timestamp_text(row[1].value_or(""));
        entity.source = row[2].value_or("");
        entity.source_name = row[3].value_or("");
        entity.session_id = row[4].has_value() && !row[4]->empty() ?
                                std::optional<std::string>(*row[4]) :
                                std::nullopt;
        entity.account_id = row[5].has_value() && !row[5]->empty() ?
                                std::optional<std::string>(*row[5]) :
                                std::nullopt;
        entity.level = row[6].value_or("");
        entity.component = row[7].value_or("");
        entity.message = row[8].value_or("");
        entity.tag = row[9].value_or("");
        entity.recorded_at = ores::database::pq::parse_timestamp_text(row[10].value_or(""));

        entries.push_back(telemetry_mapper::to_domain(entity));
    }

    BOOST_LOG_SEV(lg(), debug) << "Query returned " << entries.size() << " entries";
    return entries;
}

std::uint64_t telemetry_repository::count(context ctx, const messaging::telemetry_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Counting telemetry logs";

    const auto start_ts = ores::platform::time::datetime::to_db_string(q.start_time);
    const auto end_ts = ores::platform::time::datetime::to_db_string(q.end_time);
    const auto where_clause = build_where_clause(q, start_ts, end_ts);

    // Build count query with same filters as query() for accurate pagination
    const auto sql =
        std::format("SELECT COUNT(*) FROM ores_telemetry_logs_tbl WHERE {}", where_clause);

    BOOST_LOG_SEV(lg(), trace) << "Executing count: " << sql;

    // Use raw SQL execution via bitemporal_operations helper
    auto results = query_builder::execute_parameterized_string_query(
        ctx, sql, {}, lg(), "Counting telemetry logs");

    if (results.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Count query returned no results";
        return 0;
    }

    const auto count = std::stoull(results[0]);
    BOOST_LOG_SEV(lg(), debug) << "Count: " << count;
    return count;
}

std::vector<messaging::telemetry_log_entry> telemetry_repository::read_by_session(
    context ctx, const boost::uuids::uuid& session_id, std::uint32_t limit_count) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), debug) << "Reading telemetry for session: "
                               << boost::uuids::to_string(session_id);

    const auto session_str = boost::lexical_cast<std::string>(session_id);
    const auto query = query_builder::read<std::vector<telemetry_entity>> |
                       query_builder::where("session_id"_c == session_str) |
                       query_builder::order_by("timestamp"_c.desc()) |
                       query_builder::limit(limit_count);

    const auto result =
        query_builder::execute_read_query<telemetry_entity, messaging::telemetry_log_entry>(
            ctx,
            query,
            [](const std::vector<telemetry_entity>& entities) {
                std::vector<messaging::telemetry_log_entry> r;
                r.reserve(entities.size());
                for (const auto& entity : entities)
                    r.push_back(telemetry_mapper::to_domain(entity));
                return r;
            },
            lg(),
            "Reading telemetry by session");

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " log entries";
    return result;
}

std::vector<messaging::telemetry_log_entry>
telemetry_repository::read_by_account(context ctx,
                                      const boost::uuids::uuid& account_id,
                                      const std::chrono::system_clock::time_point& start,
                                      const std::chrono::system_clock::time_point& end,
                                      std::uint32_t limit_count) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), debug) << "Reading telemetry for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_str = boost::lexical_cast<std::string>(account_id);
    const auto start_ts = start;
    const auto end_ts = end;

    const auto query = query_builder::read<std::vector<telemetry_entity>> |
                       query_builder::where("account_id"_c == account_str &&
                                            "timestamp"_c >= start_ts && "timestamp"_c < end_ts) |
                       query_builder::order_by("timestamp"_c.desc()) |
                       query_builder::limit(limit_count);

    const auto result =
        query_builder::execute_read_query<telemetry_entity, messaging::telemetry_log_entry>(
            ctx,
            query,
            [](const std::vector<telemetry_entity>& entities) {
                std::vector<messaging::telemetry_log_entry> r;
                r.reserve(entities.size());
                for (const auto& entity : entities)
                    r.push_back(telemetry_mapper::to_domain(entity));
                return r;
            },
            lg(),
            "Reading telemetry by account");

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " log entries";
    return result;
}

std::vector<domain::telemetry_stats>
telemetry_repository::read_hourly_stats(context ctx, const domain::telemetry_stats_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Reading hourly telemetry stats";

    const auto start_str = ores::platform::time::datetime::to_db_string(q.start_time);
    const auto end_str = ores::platform::time::datetime::to_db_string(q.end_time);

    // Query by time range only, apply optional filters in memory.
    const auto query = query_builder::read<std::vector<telemetry_stats_hourly_entity>> |
                       query_builder::where("hour"_c >= start_str && "hour"_c < end_str) |
                       query_builder::order_by("hour"_c.desc());

    const auto rows = query_builder::execute_read_query<telemetry_stats_hourly_entity,
                                                        telemetry_stats_hourly_entity>(
        ctx,
        query,
        [](const auto& entities) { return entities; },
        lg(),
        "Reading hourly telemetry stats");

    // Apply in-memory filtering for optional criteria
    std::vector<domain::telemetry_stats> result;
    result.reserve(rows.size());
    for (const auto& entity : rows) {
        if (q.source.has_value()) {
            const auto source_str = std::string(domain::to_string(*q.source));
            if (entity.source != source_str)
                continue;
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
telemetry_repository::read_daily_stats(context ctx, const domain::telemetry_stats_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Reading daily telemetry stats";

    const auto start_str = ores::platform::time::datetime::to_db_string(q.start_time);
    const auto end_str = ores::platform::time::datetime::to_db_string(q.end_time);

    // Query by time range only, apply optional filters in memory.
    const auto query = query_builder::read<std::vector<telemetry_stats_daily_entity>> |
                       query_builder::where("day"_c >= start_str && "day"_c < end_str) |
                       query_builder::order_by("day"_c.desc());

    const auto rows = query_builder::execute_read_query<telemetry_stats_daily_entity,
                                                        telemetry_stats_daily_entity>(
        ctx,
        query,
        [](const auto& entities) { return entities; },
        lg(),
        "Reading daily telemetry stats");

    // Apply in-memory filtering for optional criteria
    std::vector<domain::telemetry_stats> result;
    result.reserve(rows.size());
    for (const auto& entity : rows) {
        if (q.source.has_value()) {
            const auto source_str = std::string(domain::to_string(*q.source));
            if (entity.source != source_str)
                continue;
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

domain::telemetry_summary telemetry_repository::get_summary(context ctx, std::uint32_t hours) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Getting telemetry summary for last " << hours << " hours";

    domain::telemetry_summary summary;
    summary.end_time = std::chrono::system_clock::now();
    summary.start_time = summary.end_time - std::chrono::hours(hours);

    const auto start_ts = datetime::to_db_string(summary.start_time);
    const auto end_ts = datetime::to_db_string(summary.end_time);

    // A count that fails leaves its figure at zero, so one bad read does not
    // lose the rest of the summary.
    const auto count_of =
        [&](const std::string& sql, const std::vector<std::string>& params, std::uint64_t& target) {
            try {
                const auto rows = query_builder::execute_parameterized_string_query(
                    ctx, sql, params, lg(), "Counting telemetry logs for the summary");
                if (!rows.empty())
                    target = std::stoull(rows.front());
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), warn) << "Summary count failed: " << e.what();
            }
        };

    count_of("SELECT COUNT(*) FROM ores_telemetry_logs_tbl"
             " WHERE timestamp >= $1::timestamptz AND timestamp <= $2::timestamptz",
             {start_ts, end_ts},
             summary.total_logs);

    // Count by level
    const std::vector<std::pair<std::string, std::uint64_t*>> level_targets = {
        {"error", &summary.error_count},
        {"warn", &summary.warn_count},
        {"info", &summary.info_count},
        {"debug", &summary.debug_count},
        {"trace", &summary.trace_count}};

    for (const auto& [level, target] : level_targets) {
        count_of("SELECT COUNT(*) FROM ores_telemetry_logs_tbl"
                 " WHERE timestamp >= $1::timestamptz AND timestamp <= $2::timestamptz"
                 " AND level = $3",
                 {start_ts, end_ts, level},
                 *target);
    }

    BOOST_LOG_SEV(lg(), debug) << "Summary: " << summary.total_logs << " total, "
                               << summary.error_count << " errors";
    return summary;
}

std::uint64_t telemetry_repository::count_errors(context ctx,
                                                 const std::string& source_name,
                                                 std::uint32_t hours) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), debug) << "Counting errors for " << source_name << " in last " << hours
                               << " hours";

    const auto end_time = std::chrono::system_clock::now();
    const auto start_time = end_time - std::chrono::hours(hours);
    const auto rows = query_builder::execute_parameterized_string_query(
        ctx,
        "SELECT COUNT(*) FROM ores_telemetry_logs_tbl"
        " WHERE timestamp >= $1::timestamptz AND timestamp < $2::timestamptz"
        " AND source_name = $3 AND level = 'error'",
        {datetime::to_db_string(start_time), datetime::to_db_string(end_time), source_name},
        lg(),
        "Counting telemetry errors");
    const std::uint64_t count = rows.empty() ? 0 : std::stoull(rows.front());

    BOOST_LOG_SEV(lg(), debug) << "Error count: " << count;
    return count;
}

std::uint64_t
telemetry_repository::delete_old_logs(context ctx,
                                      const std::chrono::system_clock::time_point& older_than) {
    ores::telemetry::log::skip_telemetry_guard guard;

    BOOST_LOG_SEV(lg(), info) << "Deleting telemetry logs older than cutoff";

    const auto older_ts = older_than;
    const auto query = query_builder::delete_from<telemetry_entity> |
                       query_builder::where("timestamp"_c < older_ts);

    query_builder::execute_delete_query(ctx, query, lg(), "deleting old telemetry logs");

    // The delete reports no count. With TimescaleDB, retention policies should
    // remove old logs anyway.
    return 0;
}

void telemetry_repository::insert_server_sample(context ctx,
                                                const messaging::nats_server_sample& sample) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), trace) << "Inserting NATS server sample";

    query_builder::execute_write_query(
        ctx,
        telemetry_mapper::to_entity(sample, ctx.tenant_id().to_string()),
        lg(),
        "Inserting NATS server sample");
}

void telemetry_repository::insert_stream_samples(
    context ctx, const std::vector<messaging::nats_stream_sample>& samples) {
    ores::telemetry::log::skip_telemetry_guard guard;

    if (samples.empty()) {
        return;
    }

    BOOST_LOG_SEV(lg(), trace) << "Inserting " << samples.size() << " NATS stream sample(s)";

    const auto tenant_id_str = ctx.tenant_id().to_string();
    std::vector<nats_stream_sample_entity> entities;
    entities.reserve(samples.size());
    for (const auto& s : samples) {
        entities.push_back(telemetry_mapper::to_entity(s, tenant_id_str));
    }

    query_builder::execute_write_query(ctx, entities, lg(), "Inserting NATS stream samples");
}

std::vector<messaging::nats_server_sample>
telemetry_repository::query_server_samples(context ctx,
                                           const messaging::nats_server_samples_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Querying NATS server samples";

    const auto start_ts = q.start_time;
    const auto end_ts = q.end_time;

    const auto qry = query_builder::read<std::vector<nats_server_sample_entity>> |
                     query_builder::where("sampled_at"_c >= start_ts && "sampled_at"_c < end_ts) |
                     query_builder::order_by("sampled_at"_c.desc()) | query_builder::limit(q.limit);

    const auto result =
        query_builder::execute_read_query<nats_server_sample_entity, messaging::nats_server_sample>(
            ctx,
            qry,
            [](const std::vector<nats_server_sample_entity>& entities) {
                std::vector<messaging::nats_server_sample> r;
                r.reserve(entities.size());
                for (const auto& entity : entities)
                    r.push_back(telemetry_mapper::to_domain(entity));
                return r;
            },
            lg(),
            "Querying NATS server samples");

    BOOST_LOG_SEV(lg(), debug) << "Query returned " << result.size() << " server sample(s)";
    return result;
}

std::vector<messaging::nats_stream_sample>
telemetry_repository::query_stream_samples(context ctx,
                                           const messaging::nats_stream_samples_query& q) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Querying NATS stream samples for: " << q.stream_name;

    const auto start_ts = q.start_time;
    const auto end_ts = q.end_time;

    const auto qry = query_builder::read<std::vector<nats_stream_sample_entity>> |
                     query_builder::where("sampled_at"_c >= start_ts && "sampled_at"_c < end_ts &&
                                          "stream_name"_c == q.stream_name) |
                     query_builder::order_by("sampled_at"_c.desc()) | query_builder::limit(q.limit);

    const auto result =
        query_builder::execute_read_query<nats_stream_sample_entity, messaging::nats_stream_sample>(
            ctx,
            qry,
            [](const std::vector<nats_stream_sample_entity>& entities) {
                std::vector<messaging::nats_stream_sample> r;
                r.reserve(entities.size());
                for (const auto& entity : entities)
                    r.push_back(telemetry_mapper::to_domain(entity));
                return r;
            },
            lg(),
            "Querying NATS stream samples");

    BOOST_LOG_SEV(lg(), debug) << "Query returned " << result.size() << " stream sample(s)";
    return result;
}

void telemetry_repository::insert_service_sample(context ctx,
                                                 const messaging::service_sample& sample) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), trace) << "Inserting service heartbeat: " << sample.service_name
                               << " instance=" << sample.instance_id;

    query_builder::execute_write_query(
        ctx, telemetry_mapper::to_entity(sample), lg(), "Inserting service heartbeat");
}

std::vector<messaging::service_sample> telemetry_repository::list_service_samples(context ctx) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Listing latest service heartbeat samples";

    // Fetch all rows inside the running window so we can pick
    // the most recent per (service_name, instance_id) in C++.
    const auto cutoff = std::chrono::system_clock::now() - service_running_window;
    const auto cutoff_ts = cutoff;

    const auto qry = query_builder::read<std::vector<service_sample_entity>> |
                     query_builder::where("sampled_at"_c >= cutoff_ts) |
                     query_builder::order_by("sampled_at"_c.desc());

    const auto rows =
        query_builder::execute_read_query<service_sample_entity, service_sample_entity>(
            ctx,
            qry,
            [](const auto& entities) { return entities; },
            lg(),
            "Listing service heartbeat samples");

    // Keep only the most recent row per (service_name, instance_id).
    // Rows are ordered by sampled_at desc so try_emplace keeps the first seen.
    std::map<std::pair<std::string, std::string>, messaging::service_sample> latest;
    for (const auto& entity : rows) {
        latest.try_emplace({entity.service_name, entity.instance_id},
                           telemetry_mapper::to_domain(entity));
    }

    std::vector<messaging::service_sample> result;
    result.reserve(latest.size());
    for (auto& [_, sample] : latest)
        result.push_back(std::move(sample));

    BOOST_LOG_SEV(lg(), debug) << "Found " << result.size() << " active service instance(s)";
    return result;
}

std::vector<messaging::service_roster_slot>
telemetry_repository::list_service_roster(context ctx, std::chrono::system_clock::time_point now) {
    ores::telemetry::log::skip_telemetry_guard guard;
    BOOST_LOG_SEV(lg(), debug) << "Reading the services roster";

    const std::string sql =
        "SELECT service_name, display_name, description, service_account, slot, "
        "instance_id, host_id, version, sampled_at "
        "FROM ores_telemetry_service_roster_fn()";
    const auto rows = query_builder::execute_raw_multi_column_query(
        ctx, sql, lg(), "Reading the services roster");

    std::vector<messaging::service_roster_slot> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 9) {
            BOOST_LOG_SEV(lg(), warn)
                << "Skipping a roster row with " << row.size() << " column(s); the roster reads 9";
            continue;
        }

        messaging::service_roster_slot slot;
        slot.service_name = row[0].value_or("");
        slot.display_name = row[1].value_or("");
        slot.description = row[2].value_or("");
        slot.service_account = row[3];
        slot.slot = std::stoi(row[4].value_or("0"));
        if (row[8].has_value() && !row[8]->empty()) {
            slot.instance_id = row[5];
            slot.host_id = row[6];
            slot.version = row[7];
            slot.sampled_at = datetime::from_db_string(*row[8]);
            slot.state = now - *slot.sampled_at <= service_running_window ?
                             ores::telemetry::domain::service_state::running :
                             ores::telemetry::domain::service_state::lost;
        } else {
            slot.state = ores::telemetry::domain::service_state::missing;
        }
        result.push_back(std::move(slot));
    }

    BOOST_LOG_SEV(lg(), debug) << "Roster has " << result.size() << " expected instance(s)";
    return result;
}

}
