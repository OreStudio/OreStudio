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
#include "ores.marketdata.core/repository/fx_forward_publish_point_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_point.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_point_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.api/messaging/fx_forward_publish_point_protocol.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_point_entity.hpp"
#include "ores.marketdata.core/repository/fx_forward_publish_point_mapper.hpp"
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

std::string fx_forward_publish_point_repository::sql() {
    return query_builder::insert_statement_sql<fx_forward_publish_point_entity>(lg());
}

bool fx_forward_publish_point_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order
fx_forward_publish_point_list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!fx_forward_publish_point_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of FX forward publish points cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> fx_forward_publish_point_filter_condition(
    const std::optional<messaging::fx_forward_publish_points_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->publish_id)
        r.push_back(
            query_builder::equals("publish_id", query_builder::filter_value(*filter->publish_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->publish_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->publish_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("publish_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
fx_forward_publish_point_repository::replace_claim(const context& ctx,
                                                   const domain::fx_forward_publish_point& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::fx_forward_publish_point
fx_forward_publish_point_repository::apply_claim(const context& ctx,
                                                 const domain::fx_forward_publish_point& v,
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

void fx_forward_publish_point_repository::write(const context& ctx,
                                                const domain::fx_forward_publish_point& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void fx_forward_publish_point_repository::write(
    const context& ctx, const std::vector<domain::fx_forward_publish_point>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void fx_forward_publish_point_repository::write(const context& ctx,
                                                const domain::fx_forward_publish_point& v,
                                                const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX forward publish point. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       fx_forward_publish_point_mapper::map(t),
                                       lg(),
                                       "Writing FX forward publish point to database.");
}

void fx_forward_publish_point_repository::write(
    const context& ctx,
    const std::vector<domain::fx_forward_publish_point>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX forward publish points. Count: " << v.size();
    std::vector<domain::fx_forward_publish_point> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       fx_forward_publish_point_mapper::map(batch),
                                       lg(),
                                       "Writing FX forward publish points to database.");
}

std::vector<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<fx_forward_publish_point_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<fx_forward_publish_point_entity,
                                             domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading latest FX forward publish points");
}

std::vector<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_latest(const context& ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX forward publish point. " << "id: " << id;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<fx_forward_publish_point_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max);

    return query_builder::execute_read_query<fx_forward_publish_point_entity,
                                             domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading latest FX forward publish point by id.");
}


std::vector<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_all(const context& ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all FX forward publish point versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<fx_forward_publish_point_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<fx_forward_publish_point_entity,
                                             domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading all FX forward publish point versions by id.");
}

std::optional<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_at_version(const context& ctx,
                                                     const std::string& id,
                                                     std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading FX forward publish point at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<fx_forward_publish_point_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<fx_forward_publish_point_entity,
                                                            domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading FX forward publish point at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

fx_forward_publish_point_repository::remove_status fx_forward_publish_point_repository::remove(
    const context& ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FX forward publish point. " << "id: " << id;
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
    const auto query = query_builder::delete_from<fx_forward_publish_point_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing FX forward publish point from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void fx_forward_publish_point_repository::remove(const context& ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::fx_forward_publish_point> fx_forward_publish_point_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::fx_forward_publish_points_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX forward publish points with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<fx_forward_publish_point_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<fx_forward_publish_point_entity,
                                                     domain::fx_forward_publish_point>(
        ctx,
        query,
        fx_forward_publish_point_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                fx_forward_publish_point_filter_condition(filter)),
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading latest FX forward publish points with pagination.");
}

std::uint32_t fx_forward_publish_point_repository::get_total_point_count(
    const context& ctx,
    const std::optional<messaging::fx_forward_publish_points_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active FX forward publish point count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<fx_forward_publish_point_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<fx_forward_publish_point_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                fx_forward_publish_point_filter_condition(filter)),
        lg(),
        "Counting FX forward publish points");
}

std::vector<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_latest(const context& ctx,
                                                 const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<fx_forward_publish_point_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<fx_forward_publish_point_entity,
                                                    domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading latest FX forward publish points by ids.");
    return result;
}

void fx_forward_publish_point_repository::remove(const context& ctx,
                                                 const std::vector<std::string>& ids) {
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
        query_builder::delete_from<fx_forward_publish_point_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max);
    query_builder::execute_delete_query(
        ctx, query, lg(), "Batch removing FX forward publish points.");
}


std::vector<domain::fx_forward_publish_point>
fx_forward_publish_point_repository::read_latest_by_publish(const context& ctx,
                                                            const boost::uuids::uuid& publish_id) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto pid = boost::uuids::to_string(publish_id);
    const auto query =
        query_builder::read<std::vector<fx_forward_publish_point_entity>> |
        query_builder::where("tenant_id"_c == tid && "publish_id"_c == pid && "valid_to"_c == max);
    return query_builder::execute_read_query<fx_forward_publish_point_entity,
                                             domain::fx_forward_publish_point>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_publish_point_mapper::map(entities); },
        lg(),
        "Reading the rows of one FX forward publish.");
}

}
