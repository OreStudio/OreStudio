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
 * Template: cpp_domain_type_repository_pq.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/domain/session_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/session_protocol.hpp"
#include "ores.iam.core/repository/session_entity.hpp"
#include "ores.iam.core/repository/session_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string session_repository::sql() {
    return query_builder::insert_statement_sql<session_entity>(lg());
}

bool session_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {
        "start_time",
    };
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order session_list_order(const ores::utility::domain::order& order,
                                        std::initializer_list<std::string> default_columns,
                                        bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id", "start_time"});
    if (!session_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of sessions cannot be ordered by " + order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id", "start_time"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
session_filter_condition(const std::optional<messaging::sessions_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->account_id)
        r.push_back(
            query_builder::equals("account_id", query_builder::filter_value(*filter->account_id)));
    if (filter->account_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->account_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("account_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition session_repository::replace_claim(context ctx,
                                                                      const domain::session& v) {
    const auto current = read_latest(ctx,
                                     boost::uuids::to_string(v.id),
                                     ores::platform::time::datetime::to_db_string(v.start_time));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::session session_repository::apply_claim(context ctx,
                                                const domain::session& v,
                                                const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "session_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx,
                     boost::uuids::to_string(v.id),
                     ores::platform::time::datetime::to_db_string(v.start_time))
             .empty())
        throw std::invalid_argument("session_repository::write: a current row already exists");
    return t;
}

void session_repository::write(context ctx, const domain::session& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void session_repository::write(context ctx, const std::vector<domain::session>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void session_repository::write(context ctx,
                               const domain::session& v,
                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing session. " << "id: " << v.id
                               << " start_time: " << v.start_time;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(session_mapper::map(t)),
                                    lg(),
                                    "Writing session to database.");
}

void session_repository::write(context ctx,
                               const std::vector<domain::session>& v,
                               const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sessions. Count: " << v.size();
    std::vector<domain::session> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(session_mapper::map(batch)),
                                    lg(),
                                    "Writing sessions to database.");
}

std::vector<domain::session> session_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("tenant_id"_c == tid) |
                       query_builder::order_by("id"_c, "start_time"_c);

    return query_builder::execute_read_query<session_entity, domain::session>(
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
    const auto query =
        query_builder::read<std::vector<session_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time);

    return query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest session by id.");
}

std::vector<domain::session> session_repository::read_latest_by_id(context ctx,
                                                                   const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest session by id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id);

    return query_builder::execute_read_query<session_entity, domain::session>(
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
    const auto query =
        query_builder::read<std::vector<session_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time) |
        query_builder::order_by("id"_c, "start_time"_c);

    return query_builder::execute_read_query<session_entity, domain::session>(
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
    const auto query =
        query_builder::delete_from<session_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "start_time"_c == start_time);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing session from database.");
    return remove_status::removed;
}

void session_repository::remove(context ctx, const std::string& id, const std::string& start_time) {
    static_cast<void>(remove(ctx, id, start_time, std::nullopt));
}

std::vector<domain::session>
session_repository::read_latest(context ctx,
                                std::uint32_t offset,
                                std::uint32_t limit,
                                const ores::utility::domain::order& order,
                                const std::optional<messaging::sessions_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sessions with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<session_entity, domain::session>(
        ctx,
        query,
        session_list_order(order, {"id", "start_time"}, false),
        session_filter_condition(filter),
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest sessions with pagination.");
}

std::uint32_t session_repository::get_total_session_count(
    context ctx, const std::optional<messaging::sessions_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active session count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<session_entity>(
        ctx, query, session_filter_condition(filter), lg(), "Counting sessions");
}

std::vector<domain::session> session_repository::read_latest(
    context ctx, const std::vector<std::string>& ids, const std::vector<std::string>& start_times) {
    if (ids.empty() || start_times.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) &&
                                            "start_time"_c.in(start_times));
    auto result = query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading latest sessions by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
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
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("id"_c == session_id_str) | query_builder::limit(1);

    const auto sessions = query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading session");
    if (sessions.empty())
        return std::nullopt;
    return sessions.front();
}

void session_repository::update_bytes(context ctx,
                                      const boost::uuids::uuid& session_id,
                                      const std::chrono::system_clock::time_point& start_time,
                                      std::uint64_t bytes_sent,
                                      std::uint64_t bytes_received) {
    query_builder::execute_parameterized_multi_column_query(
        ctx,
        "update ores_iam_sessions_tbl set bytes_sent = $1::bigint, bytes_received = $2::bigint"
        " where id = $3::uuid and start_time = $4::timestamptz",
        {std::to_string(bytes_sent),
         std::to_string(bytes_received),
         boost::lexical_cast<std::string>(session_id),
         ores::platform::time::datetime::to_db_string(start_time)},
        lg(),
        "Updating session bytes");
}

void session_repository::end_session(context ctx,
                                     const boost::uuids::uuid& session_id,
                                     const std::chrono::system_clock::time_point& start_time,
                                     const std::chrono::system_clock::time_point& end_time,
                                     std::uint64_t bytes_sent,
                                     std::uint64_t bytes_received) {
    query_builder::execute_parameterized_multi_column_query(
        ctx,
        "update ores_iam_sessions_tbl set end_time = $1::timestamptz, bytes_sent = $2::bigint,"
        " bytes_received = $3::bigint where id = $4::uuid and start_time = $5::timestamptz",
        {ores::platform::time::datetime::to_db_string(end_time),
         std::to_string(bytes_sent),
         std::to_string(bytes_received),
         boost::lexical_cast<std::string>(session_id),
         ores::platform::time::datetime::to_db_string(start_time)},
        lg(),
        "Ending session");
}

std::vector<domain::session>
session_repository::read_by_account(context ctx,
                                    const boost::uuids::uuid& account_id,
                                    std::uint32_t limit_count,
                                    std::uint32_t offset_count) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    auto query = query_builder::read<std::vector<session_entity>> |
                 query_builder::where("account_id"_c == account_id_str) |
                 query_builder::order_by("start_time"_c.desc()) |
                 query_builder::offset(offset_count);
    if (limit_count > 0)
        query = std::move(query) | query_builder::limit(limit_count);

    return query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading sessions by account");
}

std::vector<domain::session>
session_repository::read_active_by_account(context ctx, const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where("account_id"_c == account_id_str &&
                                            query_builder::is_null("end_time")) |
                       query_builder::order_by("start_time"_c.desc());

    return query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading active sessions by account");
}

std::uint32_t session_repository::count_by_account(context ctx,
                                                   const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = query_builder::read<std::vector<session_entity>>;
    return query_builder::execute_count_query(
        ctx, query, "account_id"_c == account_id_str, lg(), "Counting sessions by account");
}

std::vector<domain::session> session_repository::read_all_active(context ctx) {
    const auto query = query_builder::read<std::vector<session_entity>> |
                       query_builder::where(query_builder::is_null("end_time")) |
                       query_builder::order_by("start_time"_c.desc());

    return query_builder::execute_read_query<session_entity, domain::session>(
        ctx,
        query,
        [](const auto& entities) { return session_mapper::map(entities); },
        lg(),
        "Reading all active sessions");
}

}
