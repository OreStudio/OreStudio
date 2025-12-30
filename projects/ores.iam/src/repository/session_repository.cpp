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
#include "ores.iam/repository/session_repository.hpp"

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/session_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/session_mapper.hpp"
#include "ores.iam/repository/session_entity.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

session_repository::session_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::string session_repository::sql() {
    return generate_create_table_sql<session_entity>(lg());
}

void session_repository::create(const domain::session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Creating session: "
                               << boost::uuids::to_string(session.id);

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(session_mapper::map(session)))
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Session created successfully.";
}

void session_repository::update(const domain::session& session) {
    BOOST_LOG_SEV(lg(), debug) << "Updating session: "
                               << boost::uuids::to_string(session.id);

    auto entity = session_mapper::map(session);
    const auto query = sqlgen::update<session_entity>(
        "end_time"_c.set(entity.end_time),
        "bytes_sent"_c.set(entity.bytes_sent),
        "bytes_received"_c.set(entity.bytes_received),
        "country_code"_c.set(entity.country_code)
    ) | where("id"_c == entity.id.value() &&
              "start_time"_c == entity.start_time.value());

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Session updated successfully.";
}

void session_repository::update_bytes(const boost::uuids::uuid& session_id,
    const std::chrono::system_clock::time_point& start_time,
    std::uint64_t bytes_sent, std::uint64_t bytes_received) {

    BOOST_LOG_SEV(lg(), trace) << "Updating bytes for session: "
                               << boost::uuids::to_string(session_id);

    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto start_time_ts = timepoint_to_timestamp(start_time, lg());

    const auto query = sqlgen::update<session_entity>(
        "bytes_sent"_c.set(static_cast<std::int64_t>(bytes_sent)),
        "bytes_received"_c.set(static_cast<std::int64_t>(bytes_received))
    ) | where("id"_c == session_id_str &&
              "start_time"_c == start_time_ts);

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r, lg());
}

void session_repository::end_session(const boost::uuids::uuid& session_id,
    const std::chrono::system_clock::time_point& start_time,
    const std::chrono::system_clock::time_point& end_time,
    std::uint64_t bytes_sent, std::uint64_t bytes_received) {

    BOOST_LOG_SEV(lg(), debug) << "Ending session: "
                               << boost::uuids::to_string(session_id);

    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto start_time_ts = timepoint_to_timestamp(start_time, lg());
    const auto end_time_str = std::format("{:%Y-%m-%d %H:%M:%S}", end_time);

    const auto query = sqlgen::update<session_entity>(
        "end_time"_c.set(end_time_str),
        "bytes_sent"_c.set(static_cast<std::int64_t>(bytes_sent)),
        "bytes_received"_c.set(static_cast<std::int64_t>(bytes_received))
    ) | where("id"_c == session_id_str &&
              "start_time"_c == start_time_ts);

    const auto r = sqlgen::session(ctx_.connection_pool())
        .and_then(begin_transaction)
        .and_then(query)
        .and_then(commit);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Session ended successfully.";
}

std::optional<domain::session>
session_repository::read(const boost::uuids::uuid& session_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading session: "
                               << boost::uuids::to_string(session_id);

    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto query = sqlgen::read<std::vector<session_entity>> |
        where("id"_c == session_id_str) | limit(static_cast<std::size_t>(1));

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    if (r->empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Session not found.";
        return std::nullopt;
    }

    return session_mapper::map(r->front());
}

std::vector<domain::session>
session_repository::read_by_account(const boost::uuids::uuid& account_id,
    std::uint32_t limit, std::uint32_t offset) {

    BOOST_LOG_SEV(lg(), debug) << "Reading sessions for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    // Note: offset parameter is ignored as sqlgen doesn't support it directly

    std::vector<session_entity> entities;
    if (limit > 0) {
        const auto query = sqlgen::read<std::vector<session_entity>> |
            where("account_id"_c == account_id_str) |
            order_by("start_time"_c.desc()) |
            sqlgen::limit(static_cast<std::size_t>(limit));
        const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
        ensure_success(r, lg());
        entities = *r;
    } else {
        const auto query = sqlgen::read<std::vector<session_entity>> |
            where("account_id"_c == account_id_str) |
            order_by("start_time"_c.desc());
        const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
        ensure_success(r, lg());
        entities = *r;
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << entities.size() << " sessions.";
    return session_mapper::map(entities);
}

std::vector<domain::session>
session_repository::read_active_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading active sessions for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const std::string empty_end_time;
    const auto query = sqlgen::read<std::vector<session_entity>> |
        where("account_id"_c == account_id_str && "end_time"_c == empty_end_time) |
        order_by("start_time"_c.desc());

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Read " << r->size() << " active sessions.";
    return session_mapper::map(*r);
}

std::uint32_t
session_repository::count_active_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting active sessions for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const std::string empty_end_time;

    // HACK: Using single connection instead of session because sqlgen sessions
    // doesn't seem to support SELECT FROM with aggregations. Plain connections
    // work fine. See: https://github.com/getml/sqlgen/issues/99
    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<session_entity>(
        sqlgen::count().as<"count">()) |
        where("account_id"_c == account_id_str && "end_time"_c == empty_end_time) |
        sqlgen::to<count_result>;

    const auto r = ctx_.single_connection().and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Active count: " << r->count;
    return static_cast<std::uint32_t>(r->count);
}

std::uint32_t
session_repository::count_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting all sessions for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    // HACK: Using single connection instead of session because sqlgen sessions
    // doesn't seem to support SELECT FROM with aggregations. Plain connections
    // work fine. See: https://github.com/getml/sqlgen/issues/99
    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<session_entity>(
        sqlgen::count().as<"count">()) |
        where("account_id"_c == account_id_str) |
        sqlgen::to<count_result>;

    const auto r = ctx_.single_connection().and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Count: " << r->count;
    return static_cast<std::uint32_t>(r->count);
}

std::vector<domain::session>
session_repository::read_by_time_range(
    const std::chrono::system_clock::time_point& start,
    const std::chrono::system_clock::time_point& end,
    std::uint32_t limit_count) {

    BOOST_LOG_SEV(lg(), debug) << "Reading sessions in time range";

    const auto start_ts = timepoint_to_timestamp(start, lg());
    const auto end_ts = timepoint_to_timestamp(end, lg());

    const auto query = sqlgen::read<std::vector<session_entity>> |
        where("start_time"_c >= start_ts && "start_time"_c <= end_ts) |
        order_by("start_time"_c.desc()) |
        sqlgen::limit(static_cast<std::size_t>(limit_count));

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Read " << r->size() << " sessions.";
    return session_mapper::map(*r);
}

std::vector<domain::session>
session_repository::read_all_active() {
    BOOST_LOG_SEV(lg(), debug) << "Reading all active sessions";

    const std::string empty_end_time;
    const auto query = sqlgen::read<std::vector<session_entity>> |
        where("end_time"_c == empty_end_time) |
        order_by("start_time"_c.desc());

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Read " << r->size() << " active sessions.";
    return session_mapper::map(*r);
}

std::uint32_t session_repository::count_all_active() {
    BOOST_LOG_SEV(lg(), debug) << "Counting all active sessions";

    const std::string empty_end_time;

    // HACK: Using single connection instead of session because sqlgen sessions
    // doesn't seem to support SELECT FROM with aggregations. Plain connections
    // work fine. See: https://github.com/getml/sqlgen/issues/99
    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<session_entity>(
        sqlgen::count().as<"count">()) |
        where("end_time"_c == empty_end_time) |
        sqlgen::to<count_result>;

    const auto r = ctx_.single_connection().and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "All active count: " << r->count;
    return static_cast<std::uint32_t>(r->count);
}

std::vector<domain::session_statistics>
session_repository::read_daily_statistics(
    const boost::uuids::uuid& account_id,
    const std::chrono::system_clock::time_point& start,
    const std::chrono::system_clock::time_point& end) {

    BOOST_LOG_SEV(lg(), debug) << "Reading daily statistics for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", start);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", end);

    const auto query = sqlgen::read<std::vector<session_statistics_entity>> |
        where("account_id"_c == account_id_str &&
              "day"_c >= start_str && "day"_c <= end_str) |
        order_by("day"_c.desc());

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "Read " << r->size() << " statistics records.";
    return session_mapper::map(*r);
}

std::vector<domain::session_statistics>
session_repository::read_aggregate_daily_statistics(
    const std::chrono::system_clock::time_point& start,
    const std::chrono::system_clock::time_point& end) {

    BOOST_LOG_SEV(lg(), debug) << "Reading aggregate daily statistics";

    const auto start_str = std::format("{:%Y-%m-%d %H:%M:%S}", start);
    const auto end_str = std::format("{:%Y-%m-%d %H:%M:%S}", end);

    // For aggregate statistics, we would need a custom SQL query
    // that groups by day across all accounts. For now, return empty.
    // TODO: Implement with raw SQL query for aggregate view.
    BOOST_LOG_SEV(lg(), warn) << "Aggregate statistics not yet implemented";
    return {};
}

void session_repository::remove_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing sessions for account: "
                               << boost::uuids::to_string(account_id);

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::delete_from<session_entity> |
        where("account_id"_c == account_id_str);

    execute_delete_query(ctx_, query, lg(), "removing sessions from database");

    BOOST_LOG_SEV(lg(), debug) << "Sessions removed successfully.";
}

}
