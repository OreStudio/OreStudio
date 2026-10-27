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
#include "ores.iam.core/repository/auth_event_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.iam.api/domain/auth_event.hpp"
#include "ores.iam.api/domain/auth_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/auth_event_entity.hpp"
#include "ores.iam.core/repository/auth_event_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <sqlgen/begin_transaction.hpp>
#include <sqlgen/commit.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string auth_event_repository::sql() {
    return generate_create_table_sql<auth_event_entity>(lg());
}

bool auth_event_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy auth_event_list_order(const ores::utility::domain::order& order,
                                               std::initializer_list<std::string> default_columns,
                                               bool default_descending) {
    if (order.field.empty())
        return make_order(
            default_columns, default_descending != order.descending, {"id", "event_time"});
    if (!auth_event_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of auth events cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id", "event_time"});
}

}

ores::utility::domain::precondition
auth_event_repository::replace_claim(context ctx, const domain::auth_event& v) {
    const auto current =
        read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.event_time));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::auth_event auth_event_repository::apply_claim(
    context ctx, const domain::auth_event& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "auth_event_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.event_time)).empty())
        throw std::invalid_argument("auth_event_repository::write: a current row already exists");
    return t;
}

void auth_event_repository::write(context ctx, const domain::auth_event& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void auth_event_repository::write(context ctx, const std::vector<domain::auth_event>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void auth_event_repository::write(context ctx,
                                  const domain::auth_event& v,
                                  const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing event. " << "id: " << v.id
                               << " event_time: " << v.event_time;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(auth_event_mapper::map(t)),
                     lg(),
                     "Writing event to database.");
}

void auth_event_repository::write(context ctx,
                                  const std::vector<domain::auth_event>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing auth events. Count: " << v.size();
    std::vector<domain::auth_event> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(auth_event_mapper::map(batch)),
                     lg(),
                     "Writing auth events to database.");
}

std::vector<domain::auth_event> auth_event_repository::read_latest(context ctx) {
    const auto query =
        sqlgen::read<std::vector<auth_event_entity>> | order_by("id"_c, "event_time"_c);

    return execute_read_query<auth_event_entity, domain::auth_event>(
        ctx,
        query,
        [](const auto& entities) { return auth_event_mapper::map(entities); },
        lg(),
        "Reading latest auth events");
}

std::vector<domain::auth_event> auth_event_repository::read_latest(context ctx,
                                                                   const std::string& id,
                                                                   const std::string& event_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest event. " << "id: " << id
                               << " event_time: " << event_time;
    const auto query = sqlgen::read<std::vector<auth_event_entity>> |
                       where("id"_c == id && "event_time"_c == event_time);

    return execute_read_query<auth_event_entity, domain::auth_event>(
        ctx,
        query,
        [](const auto& entities) { return auth_event_mapper::map(entities); },
        lg(),
        "Reading latest event by id.");
}


std::vector<domain::auth_event>
auth_event_repository::read_all(context ctx, const std::string& id, const std::string& event_time) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all event versions. " << "id: " << id
                               << " event_time: " << event_time;
    const auto query = sqlgen::read<std::vector<auth_event_entity>> |
                       where("id"_c == id && "event_time"_c == event_time) |
                       order_by("id"_c, "event_time"_c);

    return execute_read_query<auth_event_entity, domain::auth_event>(
        ctx,
        query,
        [](const auto& entities) { return auth_event_mapper::map(entities); },
        lg(),
        "Reading all event versions by id.");
}


auth_event_repository::remove_status
auth_event_repository::remove(context ctx,
                              const std::string& id,
                              const std::string& event_time,
                              std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing event. " << "id: " << id
                               << " event_time: " << event_time;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id, event_time);
    if (current.empty())
        return remove_status::missing;
    const auto query = sqlgen::delete_from<auth_event_entity> |
                       where("id"_c == id && "event_time"_c == event_time);

    execute_delete_query(ctx, query, lg(), "Removing event from database.");
    return remove_status::removed;
}

void auth_event_repository::remove(context ctx,
                                   const std::string& id,
                                   const std::string& event_time) {
    static_cast<void>(remove(ctx, id, event_time, std::nullopt));
}

std::vector<domain::auth_event>
auth_event_repository::read_latest(context ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest auth events with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<auth_event_entity>> | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_ordered_read_query<auth_event_entity, domain::auth_event>(
        ctx,
        query,
        auth_event_list_order(order, {"id", "event_time"}, false),
        std::nullopt,
        [](const auto& entities) { return auth_event_mapper::map(entities); },
        lg(),
        "Reading latest auth events with pagination.");
}

std::uint32_t auth_event_repository::get_total__count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active event count";

    const auto query = sqlgen::read<std::vector<auth_event_entity>>;

    return execute_count_query<auth_event_entity>(
        ctx, query, std::nullopt, lg(), "Counting auth events");
}

std::vector<domain::auth_event> auth_event_repository::read_latest(
    context ctx, const std::vector<std::string>& ids, const std::vector<std::string>& event_times) {
    if (ids.empty() || event_times.empty())
        return {};
    const auto query = sqlgen::read<std::vector<auth_event_entity>> |
                       where("id"_c.in(ids) && "event_time"_c.in(event_times));
    auto result = execute_read_query<auth_event_entity, domain::auth_event>(
        ctx,
        query,
        [](const auto& entities) { return auth_event_mapper::map(entities); },
        lg(),
        "Reading latest auth events by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (event_times.size() != ids.size())
        throw std::invalid_argument(
            "auth_event_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < ids.size(); ++i)
        requested.emplace(ids[i], event_times[i]);
    std::vector<domain::auth_event> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                item.id, ores::platform::time::datetime::to_db_string(item.event_time))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void auth_event_repository::remove(context ctx,
                                   const std::vector<std::string>& ids,
                                   const std::vector<std::string>& event_times) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (event_times.size() != ids.size())
        throw std::invalid_argument(
            "auth_event_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < ids.size(); ++i)
        remove(ctx, ids[i], event_times[i]);
}


}
