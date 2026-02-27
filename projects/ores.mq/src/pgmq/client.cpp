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
#include "ores.mq/pgmq/client.hpp"

#include <sstream>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::mq::pgmq {

using namespace ores::logging;
using namespace ores::database::repository;

// ---------------------------------------------------------------------------
// Internal timestamp parsing (private static helpers)
// ---------------------------------------------------------------------------

/**
 * @brief Parses a PostgreSQL timestamptz string into a system_clock::time_point.
 *
 * PostgreSQL returns timestamptz values in the format:
 *   "YYYY-MM-DD HH:MM:SS.ffffff+00"
 * We strip the fractional seconds and timezone suffix and parse the date/time
 * portion as UTC (consistent with the existing rfl Reflector for time_point).
 */
std::chrono::system_clock::time_point
client::parse_pg_timestamp(const std::string& s) {
    if (s.empty()) return {};

    // Strip everything from the first '.' or '+'/'-' (timezone) after pos 10
    std::string base = s;

    const auto dot = base.find('.');
    if (dot != std::string::npos) {
        base = base.substr(0, dot);
    } else {
        // Look for timezone offset after the time portion (pos > 10)
        const auto tz = base.find_first_of("+-", 11);
        if (tz != std::string::npos) {
            base = base.substr(0, tz);
        }
    }

    std::tm tm{};
    std::istringstream iss(base);
    iss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    if (iss.fail()) return {};

    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

/**
 * @brief Parses a multi-column pgmq message row into a raw_msg.
 *
 * pgmq.message_record column order:
 *   msg_id(0), read_ct(1), enqueued_at(2), last_read_at(3), vt(4),
 *   message(5), headers(6)
 */
client::raw_msg client::parse_raw_msg(
    const std::vector<std::optional<std::string>>& row) {
    if (row.size() < 6) {
        throw mq_exception("Unexpected row width from pgmq message query");
    }
    raw_msg m;
    m.msg_id      = std::stoll(row[0].value_or("0"));
    m.read_ct     = std::stoi(row[1].value_or("0"));
    m.enqueued_at = parse_pg_timestamp(row[2].value_or(""));
    // row[3] = last_read_at (skipped)
    m.vt          = parse_pg_timestamp(row[4].value_or(""));
    m.body        = row[5].value_or("{}");
    return m;
}

/**
 * @brief Parses a multi-column pgmq metrics row into a queue_metrics.
 *
 * Column order: queue_name(0), queue_length(1), newest_msg_age_sec(2),
 *               oldest_msg_age_sec(3), total_messages(4), scrape_time(5)
 */
queue_metrics client::parse_metrics_row(
    const std::vector<std::optional<std::string>>& row) {
    if (row.size() < 6) {
        throw mq_exception("Unexpected row width from pgmq metrics query");
    }
    queue_metrics m;
    m.queue_name   = row[0].value_or("");
    m.queue_length = std::stoll(row[1].value_or("0"));
    if (row[2].has_value()) m.newest_msg_age_sec = std::stoi(*row[2]);
    if (row[3].has_value()) m.oldest_msg_age_sec = std::stoi(*row[3]);
    m.total_messages = std::stoll(row[4].value_or("0"));
    m.scrape_time    = parse_pg_timestamp(row[5].value_or(""));
    return m;
}

// ---------------------------------------------------------------------------
// Queue management
// ---------------------------------------------------------------------------

void client::create(ores::database::context ctx, const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), info) << "Creating queue: " << queue_name;
    execute_parameterized_command(std::move(ctx),
        "SELECT pgmq.create($1)",
        {queue_name}, lg(), "pgmq create queue");
}

void client::create_unlogged(ores::database::context ctx,
                              const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), info) << "Creating unlogged queue: " << queue_name;
    execute_parameterized_command(std::move(ctx),
        "SELECT pgmq.create_unlogged($1)",
        {queue_name}, lg(), "pgmq create unlogged queue");
}

bool client::drop(ores::database::context ctx, const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), info) << "Dropping queue: " << queue_name;
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.drop_queue($1)",
        {queue_name}, lg(), "pgmq drop queue");
    return !rows.empty() && rows.front() == "t";
}

int64_t client::purge(ores::database::context ctx, const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), info) << "Purging queue: " << queue_name;
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.purge_queue($1)",
        {queue_name}, lg(), "pgmq purge queue");
    if (rows.empty()) return 0;
    return std::stoll(rows.front());
}

std::vector<queue_info> client::list_queues(ores::database::context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Listing queues.";
    // Columns: queue_name(0), created_at(1), is_unlogged(2), is_partitioned(3)
    auto rows = execute_raw_multi_column_query(ctx,
        "SELECT * FROM pgmq.list_queues()",
        lg(), "pgmq list queues");

    std::vector<queue_info> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 4) continue;
        queue_info qi;
        qi.queue_name     = row[0].value_or("");
        qi.created_at     = parse_pg_timestamp(row[1].value_or(""));
        qi.is_unlogged    = (row[2].value_or("f") == "t");
        qi.is_partitioned = (row[3].value_or("f") == "t");
        result.push_back(std::move(qi));
    }
    return result;
}

// ---------------------------------------------------------------------------
// Send
// ---------------------------------------------------------------------------

int64_t client::do_send(ores::database::context ctx,
                         const std::string& queue_name,
                         const std::string& json,
                         std::chrono::seconds delay) {
    BOOST_LOG_SEV(lg(), debug) << "Sending to queue: " << queue_name;
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.send($1, $2::jsonb, $3::integer)",
        {queue_name, json, std::to_string(static_cast<int>(delay.count()))},
        lg(), "pgmq send");
    if (rows.empty()) throw mq_exception("pgmq.send returned no rows");
    return std::stoll(rows.front());
}

std::vector<int64_t> client::do_send_batch(ores::database::context ctx,
                                            const std::string& queue_name,
                                            const std::vector<std::string>& jsons,
                                            std::chrono::seconds delay) {
    if (jsons.empty()) return {};

    // Build: SELECT pgmq.send_batch($1, ARRAY[($2)::jsonb, ...], $N::integer)
    // pgmq.send_batch returns SETOF bigint — no unnest() needed.
    std::string sql = "SELECT pgmq.send_batch($1, ARRAY[";
    std::vector<std::string> params = {queue_name};
    for (std::size_t i = 0; i < jsons.size(); ++i) {
        if (i > 0) sql += ", ";
        sql += "($" + std::to_string(i + 2) + ")::jsonb";
        params.push_back(jsons[i]);
    }
    const auto delay_idx = jsons.size() + 2;
    sql += "], $" + std::to_string(delay_idx) + "::integer)";
    params.push_back(std::to_string(static_cast<int>(delay.count())));

    auto rows = execute_parameterized_string_query(ctx, sql, params,
        lg(), "pgmq send batch");

    std::vector<int64_t> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows) ids.push_back(std::stoll(row));
    return ids;
}

// ---------------------------------------------------------------------------
// Read
// ---------------------------------------------------------------------------

std::vector<client::raw_msg> client::do_read(ores::database::context ctx,
                                               const std::string& queue_name,
                                               std::chrono::seconds vt, int qty) {
    BOOST_LOG_SEV(lg(), debug) << "Reading from queue: " << queue_name
                               << " vt=" << vt.count() << "s qty=" << qty;
    auto rows = execute_parameterized_multi_column_query(ctx,
        "SELECT * FROM pgmq.read($1, $2::integer, $3::integer)",
        {queue_name,
         std::to_string(static_cast<int>(vt.count())),
         std::to_string(qty)},
        lg(), "pgmq read");

    std::vector<raw_msg> result;
    result.reserve(rows.size());
    for (const auto& row : rows) result.push_back(parse_raw_msg(row));
    return result;
}

std::vector<client::raw_msg> client::do_read_with_poll(ores::database::context ctx,
                                                        const std::string& queue_name,
                                                        std::chrono::seconds vt,
                                                        int qty,
                                                        std::chrono::seconds max_poll,
                                                        std::chrono::milliseconds poll_interval) {
    BOOST_LOG_SEV(lg(), debug) << "Long-polling queue: " << queue_name
                               << " vt=" << vt.count() << "s qty=" << qty
                               << " max_poll=" << max_poll.count() << "s";
    auto rows = execute_parameterized_multi_column_query(ctx,
        "SELECT * FROM pgmq.read_with_poll($1, $2::integer, $3::integer,"
        " $4::integer, $5::integer)",
        {queue_name,
         std::to_string(static_cast<int>(vt.count())),
         std::to_string(qty),
         std::to_string(static_cast<int>(max_poll.count())),
         std::to_string(static_cast<int>(poll_interval.count()))},
        lg(), "pgmq read_with_poll");

    std::vector<raw_msg> result;
    result.reserve(rows.size());
    for (const auto& row : rows) result.push_back(parse_raw_msg(row));
    return result;
}

std::optional<client::raw_msg> client::do_pop(ores::database::context ctx,
                                               const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), debug) << "Popping from queue: " << queue_name;
    auto rows = execute_parameterized_multi_column_query(ctx,
        "SELECT * FROM pgmq.pop($1)",
        {queue_name}, lg(), "pgmq pop");
    if (rows.empty()) return std::nullopt;
    return parse_raw_msg(rows.front());
}

// ---------------------------------------------------------------------------
// Erase / archive
// ---------------------------------------------------------------------------

bool client::erase(ores::database::context ctx, const std::string& queue_name,
                   int64_t msg_id) {
    BOOST_LOG_SEV(lg(), debug) << "Erasing msg " << msg_id
                               << " from queue: " << queue_name;
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.delete($1, $2::bigint)",
        {queue_name, std::to_string(msg_id)}, lg(), "pgmq delete");
    return !rows.empty() && rows.front() == "t";
}

std::vector<int64_t> client::erase(ores::database::context ctx,
                                    const std::string& queue_name,
                                    const std::vector<int64_t>& msg_ids) {
    if (msg_ids.empty()) return {};
    BOOST_LOG_SEV(lg(), debug) << "Erasing " << msg_ids.size()
                               << " messages from queue: " << queue_name;

    // Build bigint array literal: {1,2,3}
    std::string array_lit = "{";
    for (std::size_t i = 0; i < msg_ids.size(); ++i) {
        if (i > 0) array_lit += ",";
        array_lit += std::to_string(msg_ids[i]);
    }
    array_lit += "}";

    // pgmq.delete(queue, bigint[]) returns SETOF bigint — no unnest() needed.
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.delete($1, $2::bigint[])",
        {queue_name, array_lit}, lg(), "pgmq delete batch");

    std::vector<int64_t> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows) ids.push_back(std::stoll(row));
    return ids;
}

bool client::archive(ores::database::context ctx, const std::string& queue_name,
                     int64_t msg_id) {
    BOOST_LOG_SEV(lg(), debug) << "Archiving msg " << msg_id
                               << " from queue: " << queue_name;
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.archive($1, $2::bigint)",
        {queue_name, std::to_string(msg_id)}, lg(), "pgmq archive");
    return !rows.empty() && rows.front() == "t";
}

std::vector<int64_t> client::archive(ores::database::context ctx,
                                      const std::string& queue_name,
                                      const std::vector<int64_t>& msg_ids) {
    if (msg_ids.empty()) return {};
    BOOST_LOG_SEV(lg(), debug) << "Archiving " << msg_ids.size()
                               << " messages from queue: " << queue_name;

    std::string array_lit = "{";
    for (std::size_t i = 0; i < msg_ids.size(); ++i) {
        if (i > 0) array_lit += ",";
        array_lit += std::to_string(msg_ids[i]);
    }
    array_lit += "}";

    // pgmq.archive(queue, bigint[]) returns SETOF bigint — no unnest() needed.
    auto rows = execute_parameterized_string_query(ctx,
        "SELECT pgmq.archive($1, $2::bigint[])",
        {queue_name, array_lit}, lg(), "pgmq archive batch");

    std::vector<int64_t> ids;
    ids.reserve(rows.size());
    for (const auto& row : rows) ids.push_back(std::stoll(row));
    return ids;
}

// ---------------------------------------------------------------------------
// Visibility timeout
// ---------------------------------------------------------------------------

std::optional<client::raw_msg> client::do_set_vt(ores::database::context ctx,
                                                   const std::string& queue_name,
                                                   int64_t msg_id,
                                                   std::chrono::seconds vt) {
    BOOST_LOG_SEV(lg(), debug) << "Setting VT for msg " << msg_id
                               << " in queue " << queue_name
                               << " to " << vt.count() << "s";
    auto rows = execute_parameterized_multi_column_query(ctx,
        "SELECT * FROM pgmq.set_vt($1, $2::bigint, $3::integer)",
        {queue_name, std::to_string(msg_id),
         std::to_string(static_cast<int>(vt.count()))},
        lg(), "pgmq set_vt");
    if (rows.empty()) return std::nullopt;
    return parse_raw_msg(rows.front());
}

// ---------------------------------------------------------------------------
// Metrics
// ---------------------------------------------------------------------------

queue_metrics client::metrics(ores::database::context ctx,
                               const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), debug) << "Getting metrics for queue: " << queue_name;
    auto rows = execute_parameterized_multi_column_query(ctx,
        "SELECT * FROM pgmq.metrics($1)",
        {queue_name}, lg(), "pgmq metrics");
    if (rows.empty()) throw mq_exception("pgmq.metrics returned no rows for: " + queue_name);
    return parse_metrics_row(rows.front());
}

std::vector<queue_metrics> client::metrics_all(ores::database::context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Getting metrics for all queues.";
    auto rows = execute_raw_multi_column_query(ctx,
        "SELECT * FROM pgmq.metrics_all()",
        lg(), "pgmq metrics_all");

    std::vector<queue_metrics> result;
    result.reserve(rows.size());
    for (const auto& row : rows) result.push_back(parse_metrics_row(row));
    return result;
}

// ---------------------------------------------------------------------------
// NOTIFY/LISTEN
// ---------------------------------------------------------------------------

void client::enable_notify(ores::database::context ctx,
                            const std::string& queue_name,
                            std::chrono::milliseconds throttle) {
    BOOST_LOG_SEV(lg(), info) << "Enabling notify for queue: " << queue_name
                              << " throttle=" << throttle.count() << "ms";
    execute_parameterized_command(std::move(ctx),
        "SELECT pgmq.enable_notify_insert($1, $2::integer)",
        {queue_name, std::to_string(static_cast<int>(throttle.count()))},
        lg(), "pgmq enable_notify_insert");
}

void client::disable_notify(ores::database::context ctx,
                             const std::string& queue_name) {
    BOOST_LOG_SEV(lg(), info) << "Disabling notify for queue: " << queue_name;
    execute_parameterized_command(std::move(ctx),
        "SELECT pgmq.disable_notify_insert($1)",
        {queue_name}, lg(), "pgmq disable_notify_insert");
}

}
