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
#include "ores.trading.core/repository/trade_party_role_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_party_role.hpp"
#include "ores.trading.api/domain/trade_party_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_party_role_protocol.hpp"
#include "ores.trading.core/repository/trade_party_role_entity.hpp"
#include "ores.trading.core/repository/trade_party_role_mapper.hpp"
#include "ores.trading.core/repository/trade_write_observation.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
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

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_party_role_repository::sql() {
    return query_builder::insert_statement_sql<trade_party_role_entity>(lg());
}

bool trade_party_role_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order trade_party_role_list_order(const ores::utility::domain::order& order,
                                                 std::initializer_list<std::string> default_columns,
                                                 bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"trade_id", "role"});
    if (!trade_party_role_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of trade party roles cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"trade_id", "role"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> trade_party_role_filter_condition(
    const std::optional<messaging::trade_party_roles_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->trade_id)
        r.push_back(
            query_builder::equals("trade_id", query_builder::filter_value(*filter->trade_id)));
    if (filter->trade_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->trade_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("trade_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
trade_party_role_repository::replace_claim(context ctx, const domain::trade_party_role& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id), v.role);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::trade_party_role
trade_party_role_repository::apply_claim(context ctx,
                                         const domain::trade_party_role& v,
                                         const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id), v.role);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void trade_party_role_repository::write(context ctx, const domain::trade_party_role& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void trade_party_role_repository::write(context ctx,
                                        const std::vector<domain::trade_party_role>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void trade_party_role_repository::write(context ctx,
                                        const domain::trade_party_role& v,
                                        const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade party role. " << "trade_id: " << v.trade_id
                               << " role: " << v.role;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, trade_party_role_mapper::map(t), lg(), "Writing trade party role to database.");
}

void trade_party_role_repository::write(
    context ctx,
    const std::vector<domain::trade_party_role>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade party roles. Count: " << v.size();
    std::vector<domain::trade_party_role> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, trade_party_role_mapper::map(batch), lg(), "Writing trade party roles to database.");
}

std::vector<domain::trade_party_role> trade_party_role_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c, "role"_c);

    return query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx,
        query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading latest trade party roles");
}

std::vector<domain::trade_party_role> trade_party_role_repository::read_latest(
    context ctx, const std::string& trade_id, const std::string& role) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade party role. " << "trade_id: " << trade_id
                               << " role: " << role;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "role"_c == role && "valid_to"_c == max.value());

    return query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx,
        query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading latest trade party role by trade_id.");
}


std::vector<domain::trade_party_role> trade_party_role_repository::read_all(
    context ctx, const std::string& trade_id, const std::string& role) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade party role versions. "
                               << "trade_id: " << trade_id << " role: " << role;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<trade_party_role_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id && "role"_c == role) |
        query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx,
        query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading all trade party role versions by trade_id.");
}

std::optional<domain::trade_party_role> trade_party_role_repository::read_at_version(
    context ctx, const std::string& trade_id, const std::string& role, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade party role at version. "
                               << "trade_id: " << trade_id << " role: " << role
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "role"_c == role && "version"_c == version) |
                       query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
            ctx,
            query,
            [](const auto& entities) { return trade_party_role_mapper::map(entities); },
            lg(),
            "Reading trade party role at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::trade_party_role> trade_party_role_repository::read_latest_by_trade_id(
    context ctx,
    const std::string& trade_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::trade_party_roles_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade party roles. trade_id: " << trade_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max.value()) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_party_role_entity,
                                                     domain::trade_party_role>(
        ctx,
        query,
        trade_party_role_list_order(order, {"trade_id", "role"}, false),
        trade_party_role_filter_condition(filter),
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading latest trade party roles by trade_id.");
}

std::uint32_t trade_party_role_repository::get_total_role_count_by_trade_id(
    context ctx,
    const std::string& trade_id,
    const std::optional<messaging::trade_party_roles_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade party roles count. trade_id: "
                               << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max.value());

    return query_builder::execute_count_query<trade_party_role_entity>(
        ctx,
        query,
        trade_party_role_filter_condition(filter),
        lg(),
        "Counting trade party roles by trade_id");
}


trade_party_role_repository::remove_status
trade_party_role_repository::remove(context ctx,
                                    const std::string& trade_id,
                                    const std::string& role,
                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade party role. " << "trade_id: " << trade_id
                               << " role: " << role;
    const auto current = read_latest(ctx, trade_id, role);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<trade_party_role_entity> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id && "role"_c == role &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing trade party role from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, role).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void trade_party_role_repository::remove(context ctx,
                                         const std::string& trade_id,
                                         const std::string& role) {
    static_cast<void>(remove(ctx, trade_id, role, std::nullopt));
}

std::vector<domain::trade_party_role> trade_party_role_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::trade_party_roles_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade party roles with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_party_role_entity,
                                                     domain::trade_party_role>(
        ctx,
        query,
        trade_party_role_list_order(order, {"trade_id", "role"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_party_role_filter_condition(filter)),
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(),
        "Reading latest trade party roles with pagination.");
}

std::uint32_t trade_party_role_repository::get_total_role_count(
    context ctx,
    const std::optional<messaging::trade_party_roles_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade party role count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<trade_party_role_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_party_role_filter_condition(filter)),
        lg(),
        "Counting trade party roles");
}

std::vector<domain::trade_party_role> trade_party_role_repository::read_latest(
    context ctx, const std::vector<std::string>& trade_ids, const std::vector<std::string>& roles) {
    if (trade_ids.empty() || roles.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_party_role_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "role"_c.in(roles) && "valid_to"_c == max.value());
    auto result =
        query_builder::execute_read_query<trade_party_role_entity, domain::trade_party_role>(
            ctx,
            query,
            [](const auto& entities) { return trade_party_role_mapper::map(entities); },
            lg(),
            "Reading latest trade party roles by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (roles.size() != trade_ids.size())
        throw std::invalid_argument(
            "trade_party_role_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], roles[i]);
    std::vector<domain::trade_party_role> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id), item.role)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void trade_party_role_repository::remove(context ctx,
                                         const std::vector<std::string>& trade_ids,
                                         const std::vector<std::string>& roles) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (roles.size() != trade_ids.size())
        throw std::invalid_argument(
            "trade_party_role_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], roles[i]);
}


}
