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
#include "ores.trading.core/repository/trade_link_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_link.hpp"
#include "ores.trading.api/domain/trade_link_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_link_entity.hpp"
#include "ores.trading.core/repository/trade_link_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
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

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_link_repository::sql() {
    return generate_create_table_sql<trade_link_entity>(lg());
}

bool trade_link_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns,
                          default_descending != order.descending,
                          {"from_trade_id", "to_trade_id", "link_type"});
    if (!trade_link_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of trade links cannot be ordered by " + order.field +
                                    ".");
    return make_order(
        {order.field}, order.descending, {"from_trade_id", "to_trade_id", "link_type"});
}

}

ores::utility::domain::precondition
trade_link_repository::replace_claim(context ctx, const domain::trade_link& v) {
    const auto current = read_latest(ctx,
                                     boost::uuids::to_string(v.from_trade_id),
                                     boost::uuids::to_string(v.to_trade_id),
                                     v.link_type);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::trade_link trade_link_repository::apply_claim(
    context ctx, const domain::trade_link& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx,
                                             boost::uuids::to_string(v.from_trade_id),
                                             boost::uuids::to_string(v.to_trade_id),
                                             v.link_type);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void trade_link_repository::write(context ctx, const domain::trade_link& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void trade_link_repository::write(context ctx, const std::vector<domain::trade_link>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void trade_link_repository::write(context ctx,
                                  const domain::trade_link& v,
                                  const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade link. " << "from_trade_id: " << v.from_trade_id
                               << " to_trade_id: " << v.to_trade_id
                               << " link_type: " << v.link_type;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, trade_link_mapper::map(t), lg(), "Writing trade link to database.");
}

void trade_link_repository::write(context ctx,
                                  const std::vector<domain::trade_link>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade links. Count: " << v.size();
    std::vector<domain::trade_link> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, trade_link_mapper::map(batch), lg(), "Writing trade links to database.");
}

std::vector<domain::trade_link> trade_link_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("from_trade_id"_c, "to_trade_id"_c, "link_type"_c);

    return execute_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading latest trade links");
}

std::vector<domain::trade_link> trade_link_repository::read_latest(context ctx,
                                                                   const std::string& from_trade_id,
                                                                   const std::string& to_trade_id,
                                                                   const std::string& link_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade link. "
                               << "from_trade_id: " << from_trade_id
                               << " to_trade_id: " << to_trade_id << " link_type: " << link_type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> |
                       where("tenant_id"_c == tid && "from_trade_id"_c == from_trade_id &&
                             "to_trade_id"_c == to_trade_id && "link_type"_c == link_type &&
                             "valid_to"_c == max.value());

    return execute_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading latest trade link by from_trade_id.");
}


std::vector<domain::trade_link> trade_link_repository::read_all(context ctx,
                                                                const std::string& from_trade_id,
                                                                const std::string& to_trade_id,
                                                                const std::string& link_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade link versions. "
                               << "from_trade_id: " << from_trade_id
                               << " to_trade_id: " << to_trade_id << " link_type: " << link_type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> |
                       where("tenant_id"_c == tid && "from_trade_id"_c == from_trade_id &&
                             "to_trade_id"_c == to_trade_id && "link_type"_c == link_type) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading all trade link versions by from_trade_id.");
}

std::optional<domain::trade_link>
trade_link_repository::read_at_version(context ctx,
                                       const std::string& from_trade_id,
                                       const std::string& to_trade_id,
                                       const std::string& link_type,
                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade link at version. "
                               << "from_trade_id: " << from_trade_id
                               << " to_trade_id: " << to_trade_id << " link_type: " << link_type
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> |
                       where("tenant_id"_c == tid && "from_trade_id"_c == from_trade_id &&
                             "to_trade_id"_c == to_trade_id && "link_type"_c == link_type &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading trade link at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

trade_link_repository::remove_status
trade_link_repository::remove(context ctx,
                              const std::string& from_trade_id,
                              const std::string& to_trade_id,
                              const std::string& link_type,
                              std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade link. " << "from_trade_id: " << from_trade_id
                               << " to_trade_id: " << to_trade_id << " link_type: " << link_type;
    const auto current = read_latest(ctx, from_trade_id, to_trade_id, link_type);
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
    const auto query = sqlgen::delete_from<trade_link_entity> |
                       where("tenant_id"_c == tid && "from_trade_id"_c == from_trade_id &&
                             "to_trade_id"_c == to_trade_id && "link_type"_c == link_type &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing trade link from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, from_trade_id, to_trade_id, link_type).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void trade_link_repository::remove(context ctx,
                                   const std::string& from_trade_id,
                                   const std::string& to_trade_id,
                                   const std::string& link_type) {
    static_cast<void>(remove(ctx, from_trade_id, to_trade_id, link_type, std::nullopt));
}

std::vector<domain::trade_link>
trade_link_repository::read_latest(context ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order,
                                   const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade links with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> | where("tenant_id"_c == tid) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        list_order(order, {"from_trade_id", "to_trade_id", "link_type"}, false),
        narrowed(valid_at(as_of), std::nullopt),
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading latest trade links with pagination.");
}

std::uint32_t trade_link_repository::get_total_link_count(context ctx,
                                                          const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade link count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<trade_link_entity>(
        ctx, query, narrowed(valid_at(as_of), std::nullopt), lg(), "Counting trade links");
}

std::vector<domain::trade_link>
trade_link_repository::read_latest(context ctx,
                                   const std::vector<std::string>& from_trade_ids,
                                   const std::vector<std::string>& to_trade_ids,
                                   const std::vector<std::string>& link_types) {
    if (from_trade_ids.empty() || to_trade_ids.empty() || link_types.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_link_entity>> |
                       where("tenant_id"_c == tid && "from_trade_id"_c.in(from_trade_ids) &&
                             "to_trade_id"_c.in(to_trade_ids) && "link_type"_c.in(link_types) &&
                             "valid_to"_c == max.value());
    auto result = execute_read_query<trade_link_entity, domain::trade_link>(
        ctx,
        query,
        [](const auto& entities) { return trade_link_mapper::map(entities); },
        lg(),
        "Reading latest trade links by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (to_trade_ids.size() != from_trade_ids.size() || link_types.size() != from_trade_ids.size())
        throw std::invalid_argument(
            "trade_link_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < from_trade_ids.size(); ++i)
        requested.emplace(from_trade_ids[i], to_trade_ids[i], link_types[i]);
    std::vector<domain::trade_link> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.from_trade_id),
                                               boost::uuids::to_string(item.to_trade_id),
                                               item.link_type)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void trade_link_repository::remove(context ctx,
                                   const std::vector<std::string>& from_trade_ids,
                                   const std::vector<std::string>& to_trade_ids,
                                   const std::vector<std::string>& link_types) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (to_trade_ids.size() != from_trade_ids.size() || link_types.size() != from_trade_ids.size())
        throw std::invalid_argument(
            "trade_link_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < from_trade_ids.size(); ++i)
        remove(ctx, from_trade_ids[i], to_trade_ids[i], link_types[i]);
}


}
