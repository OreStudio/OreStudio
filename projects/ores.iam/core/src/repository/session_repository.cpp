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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/session_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/session_entity.hpp"
#include "ores.iam.core/repository/session_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <set>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <tuple>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string session_repository::sql() {
    return generate_create_table_sql<session_entity>(lg());
}

void session_repository::write(context ctx, const domain::session& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing session. " << "id: " << v.id
                               << " start_time: " << v.start_time;
    const auto query = sqlgen::insert_or_replace(session_mapper::map(v));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void session_repository::write(context ctx, const std::vector<domain::session>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sessions. Count: " << v.size();
    const auto query = sqlgen::insert_or_replace(session_mapper::map(v));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::session> session_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<session_entity>> | where("tenant_id"_c == tid) |
                       order_by("id"_c, "start_time"_c);

    return execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest sessions");
}

std::vector<domain::session>
session_repository::read_latest(context ctx, const std::string& id, const std::string& start_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest session. " << "id: " << id
                               << " start_time: " << start_time;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<session_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time);

    return execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest session by id.");
}


std::vector<domain::session>
session_repository::read_all(context ctx, const std::string& id, const std::string& start_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all session versions. " << "id: " << id
                               << " start_time: " << start_time;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<session_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time) |
                       order_by("id"_c, "start_time"_c);

    return execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading all session versions by id.");
}


session_repository::remove_status session_repository::remove(context ctx,
                                                             const std::string& id,
                                                             const std::string& start_time,
                                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing session. " << "id: " << id
                               << " start_time: " << start_time;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id, start_time);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<session_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time);

    execute_delete_query(ctx, query, lg(), "Removing session from database.");
    return remove_status::removed;
}

void session_repository::remove(context ctx, const std::string& id, const std::string& start_time) {
    static_cast<void>(remove(ctx, id, start_time, std::nullopt));
}

std::vector<domain::session>
session_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sessions with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<session_entity>> | where("tenant_id"_c == tid) |
                       order_by("id"_c, "start_time"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest sessions with pagination.");
}

std::uint32_t session_repository::get_total_session_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active session count";

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<session_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active session count: " << count;
    return count;
}

std::vector<domain::session> session_repository::read_latest(
    context ctx, const std::vector<std::string>& ids, const std::vector<std::string>& start_times) {
    if (ids.empty() || start_times.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<session_entity>> |
        where("tenant_id"_c == tid && "id"_c.in(ids) && "start_time"_c.in(start_times));
    auto result = execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest sessions by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (start_times.size() != ids.size())
        throw std::invalid_argument(
            "session_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < ids.size(); ++i)
        requested.emplace(ids[i], start_times[i]);
    std::vector<domain::session> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.id),
                                ores::platform::time::datetime::to_db_string(item.start_time))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void session_repository::remove(context ctx,
                                const std::vector<std::string>& ids,
                                const std::vector<std::string>& start_times) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (start_times.size() != ids.size())
        throw std::invalid_argument(
            "session_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < ids.size(); ++i)
        remove(ctx, ids[i], start_times[i]);
}


std::optional<domain::session> session_repository::read(context ctx,
                                                        const boost::uuids::uuid& session_id) {
    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto query = sqlgen::read<std::vector<session_entity>> | where("id"_c == session_id_str) |
                       limit(static_cast<std::size_t>(1));

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    if (r->empty()) {
        return std::nullopt;
    }

    return session_mapper::map(r->front());
}

void session_repository::update_bytes(context ctx,
                                      const boost::uuids::uuid& session_id,
                                      const std::chrono::system_clock::time_point& start_time,
                                      std::uint64_t bytes_sent,
                                      std::uint64_t bytes_received) {
    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto start_time_str = ores::platform::time::datetime::to_db_string(start_time);

    const auto query = sqlgen::update<session_entity>(
                           "bytes_sent"_c.set(static_cast<std::int64_t>(bytes_sent)),
                           "bytes_received"_c.set(static_cast<std::int64_t>(bytes_received))) |
                       where("id"_c == session_id_str && "start_time"_c == start_time_str);

    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(begin_transaction)
                       .and_then(query)
                       .and_then(commit);
    ensure_success(r, lg());
}

void session_repository::end_session(context ctx,
                                     const boost::uuids::uuid& session_id,
                                     const std::chrono::system_clock::time_point& start_time,
                                     const std::chrono::system_clock::time_point& end_time,
                                     std::uint64_t bytes_sent,
                                     std::uint64_t bytes_received) {
    const auto session_id_str = boost::lexical_cast<std::string>(session_id);
    const auto start_time_str = ores::platform::time::datetime::to_db_string(start_time);
    const auto end_time_str = ores::platform::time::datetime::to_db_string(end_time);

    const auto query = sqlgen::update<session_entity>(
                           "end_time"_c.set(end_time_str),
                           "bytes_sent"_c.set(static_cast<std::int64_t>(bytes_sent)),
                           "bytes_received"_c.set(static_cast<std::int64_t>(bytes_received))) |
                       where("id"_c == session_id_str && "start_time"_c == start_time_str);

    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(begin_transaction)
                       .and_then(query)
                       .and_then(commit);
    ensure_success(r, lg());
}

std::vector<domain::session>
session_repository::read_by_account(context ctx,
                                    const boost::uuids::uuid& account_id,
                                    std::uint32_t limit_count,
                                    std::uint32_t offset_count) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    std::vector<session_entity> entities;
    if (limit_count > 0) {
        const auto query = sqlgen::read<std::vector<session_entity>> |
                           where("account_id"_c == account_id_str) |
                           order_by("start_time"_c.desc()) |
                           sqlgen::offset(static_cast<std::size_t>(offset_count)) |
                           sqlgen::limit(static_cast<std::size_t>(limit_count));
        const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
        ensure_success(r, lg());
        entities = *r;
    } else {
        const auto query = sqlgen::read<std::vector<session_entity>> |
                           where("account_id"_c == account_id_str) |
                           order_by("start_time"_c.desc()) |
                           sqlgen::offset(static_cast<std::size_t>(offset_count));
        const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
        ensure_success(r, lg());
        entities = *r;
    }

    return session_mapper::map(entities);
}

std::vector<domain::session>
session_repository::read_active_by_account(context ctx, const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const std::string empty_end_time;
    const auto query = sqlgen::read<std::vector<session_entity>> |
                       where("account_id"_c == account_id_str && "end_time"_c == empty_end_time) |
                       order_by("start_time"_c.desc());

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    return session_mapper::map(*r);
}

std::uint32_t session_repository::count_by_account(context ctx,
                                                   const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<session_entity>(sqlgen::count().as<"count">()) |
                       where("account_id"_c == account_id_str) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    return static_cast<std::uint32_t>(r->count);
}

std::vector<domain::session> session_repository::read_all_active(context ctx) {
    const std::string empty_end_time;
    const auto query = sqlgen::read<std::vector<session_entity>> |
                       where("end_time"_c == empty_end_time) | order_by("start_time"_c.desc());

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    return session_mapper::map(*r);
}

}
