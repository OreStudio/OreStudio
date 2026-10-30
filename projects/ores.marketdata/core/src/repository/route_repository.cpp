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
#include "ores.marketdata.core/repository/route_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/route.hpp"
#include "ores.marketdata.api/domain/route_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.core/repository/route_entity.hpp"
#include "ores.marketdata.core/repository/route_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string route_repository::sql() {
    return query_builder::insert_statement_sql<route_entity>(lg());
}

bool route_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order route_list_order(const ores::utility::domain::order& order,
                                      std::initializer_list<std::string> default_columns,
                                      bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!route_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of routes cannot be ordered by " + order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
route_filter_condition(const std::optional<messaging::routes_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->party_id)
        r.push_back(
            query_builder::equals("party_id", query_builder::filter_value(*filter->party_id)));
    if (filter->publisher_code)
        r.push_back(query_builder::equals("publisher_code",
                                          query_builder::filter_value(*filter->publisher_code)));
    if (filter->asset_class)
        r.push_back(query_builder::equals("asset_class",
                                          query_builder::filter_value(*filter->asset_class)));
    if (filter->feed_id)
        r.push_back(
            query_builder::equals("feed_id", query_builder::filter_value(*filter->feed_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->party_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->party_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("party_id", std::move(values)));
    }
    if (filter->publisher_code_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->publisher_code_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("publisher_code", std::move(values)));
    }
    if (filter->asset_class_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->asset_class_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("asset_class", std::move(values)));
    }
    if (filter->feed_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->feed_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("feed_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition route_repository::replace_claim(context ctx,
                                                                    const domain::route& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::route route_repository::apply_claim(context ctx,
                                            const domain::route& v,
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void route_repository::write(context ctx, const domain::route& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void route_repository::write(context ctx, const std::vector<domain::route>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void route_repository::write(context ctx,
                             const domain::route& v,
                             const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing route. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, route_mapper::map(t), lg(), "Writing route to database.");
}

void route_repository::write(context ctx,
                             const std::vector<domain::route>& v,
                             const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing routes. Count: " << v.size();
    std::vector<domain::route> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, route_mapper::map(batch), lg(), "Writing routes to database.");
}

std::vector<domain::route> route_repository::read_latest(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<route_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest routes");
}

std::vector<domain::route> route_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest route. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest route by id.");
}

std::vector<domain::route>
route_repository::read_latest_by_publisher_code(context ctx, const std::string& publisher_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest route by publisher_code: " << publisher_code;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> |
        query_builder::where("tenant_id"_c == tid && "publisher_code"_c == publisher_code &&
                             "valid_to"_c == max);

    // A key names a row of one party: another party can hold the same key, so
    // the lookup keeps the rows of the session's party when it has one. A
    // finder with a parent column is pinned to its parent's party already.
    auto found = query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest route by publisher_code.");
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    return found;
}

std::vector<domain::route>
route_repository::read_any_by_publisher_code(context ctx, const std::string& publisher_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any route by publisher_code: " << publisher_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> |
        query_builder::where("tenant_id"_c == tid && "publisher_code"_c == publisher_code) |
        query_builder::order_by("valid_from"_c.desc());

    auto found = query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading any route by publisher_code.");
    // A key names a row of one party, and another party of the tenant may hold
    // the same key, so the read keeps the session's party's rows. The filter
    // runs before the newest row is taken because the party column may be
    // null, and a null party means the row belongs to the whole tenant and
    // stays readable by every party, as the latest-by finder also has it.
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    if (found.size() > 1)
        found.resize(1);
    return found;
}


std::vector<domain::route> route_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all route versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<route_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading all route versions by id.");
}

std::optional<domain::route>
route_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading route at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading route at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

route_repository::remove_status
route_repository::remove(context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing route. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<route_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing route from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void route_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::route>
route_repository::read_latest(context ctx,
                              std::uint32_t offset,
                              std::uint32_t limit,
                              const ores::utility::domain::order& order,
                              const std::optional<messaging::routes_filter>& filter,
                              const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest routes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<route_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<route_entity, domain::route>(
        ctx,
        query,
        route_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), route_filter_condition(filter)),
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest routes with pagination.");
}

std::uint32_t
route_repository::get_total_route_count(context ctx,
                                        const std::optional<messaging::routes_filter>& filter,
                                        const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active route count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> | query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<route_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), route_filter_condition(filter)),
        lg(),
        "Counting routes");
}

std::vector<domain::route> route_repository::read_latest(context ctx,
                                                         const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<route_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest routes by ids.");
    return result;
}

void route_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<route_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing routes.");
}


std::vector<domain::route> route_repository::read_latest_all_tenants(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto query = query_builder::read<std::vector<route_entity>> |
                       query_builder::where("valid_to"_c == max) | query_builder::order_by("id"_c);

    return query_builder::execute_read_query<route_entity, domain::route>(
        ctx,
        query,
        [](const auto& entities) { return route_mapper::map(entities); },
        lg(),
        "Reading latest routes across all tenants");
}

}
