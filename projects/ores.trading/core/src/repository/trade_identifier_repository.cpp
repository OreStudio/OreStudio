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
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/domain/trade_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.core/repository/trade_identifier_entity.hpp"
#include "ores.trading.core/repository/trade_identifier_mapper.hpp"
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

std::string trade_identifier_repository::sql() {
    return query_builder::insert_statement_sql<trade_identifier_entity>(lg());
}

bool trade_identifier_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order trade_identifier_list_order(const ores::utility::domain::order& order,
                                                 std::initializer_list<std::string> default_columns,
                                                 bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"trade_id", "id_type"});
    if (!trade_identifier_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of trade identifiers cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"trade_id", "id_type"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> trade_identifier_filter_condition(
    const std::optional<messaging::trade_identifiers_filter>& filter) {
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
trade_identifier_repository::replace_claim(context ctx, const domain::trade_identifier& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id), v.id_type);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::trade_identifier
trade_identifier_repository::apply_claim(context ctx,
                                         const domain::trade_identifier& v,
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id), v.id_type);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void trade_identifier_repository::write(context ctx, const domain::trade_identifier& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void trade_identifier_repository::write(context ctx,
                                        const std::vector<domain::trade_identifier>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void trade_identifier_repository::write(context ctx,
                                        const domain::trade_identifier& v,
                                        const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade identifier. " << "trade_id: " << v.trade_id
                               << " id_type: " << v.id_type;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, trade_identifier_mapper::map(t), lg(), "Writing trade identifier to database.");
}

void trade_identifier_repository::write(
    context ctx,
    const std::vector<domain::trade_identifier>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade identifiers. Count: " << v.size();
    std::vector<domain::trade_identifier> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, trade_identifier_mapper::map(batch), lg(), "Writing trade identifiers to database.");
}

std::vector<domain::trade_identifier> trade_identifier_repository::read_latest(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "id_type"_c);

    return query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
        ctx,
        query,
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading latest trade identifiers");
}

std::vector<domain::trade_identifier> trade_identifier_repository::read_latest(
    context ctx, const std::string& trade_id, const std::string& id_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade identifier. " << "trade_id: " << trade_id
                               << " id_type: " << id_type;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "id_type"_c == id_type && "valid_to"_c == max);

    return query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
        ctx,
        query,
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading latest trade identifier by trade_id.");
}


std::vector<domain::trade_identifier> trade_identifier_repository::read_all(
    context ctx, const std::string& trade_id, const std::string& id_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade identifier versions. "
                               << "trade_id: " << trade_id << " id_type: " << id_type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "id_type"_c == id_type) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
        ctx,
        query,
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading all trade identifier versions by trade_id.");
}

std::optional<domain::trade_identifier> trade_identifier_repository::read_at_version(
    context ctx, const std::string& trade_id, const std::string& id_type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade identifier at version. "
                               << "trade_id: " << trade_id << " id_type: " << id_type
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "id_type"_c == id_type && "version"_c == version) |
                       query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
            ctx,
            query,
            [](const auto& entities) { return trade_identifier_mapper::map(entities); },
            lg(),
            "Reading trade identifier at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::trade_identifier> trade_identifier_repository::read_latest_by_trade_id(
    context ctx,
    const std::string& trade_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::trade_identifiers_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade identifiers. trade_id: " << trade_id
                               << " offset: " << offset << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_identifier_entity,
                                                     domain::trade_identifier>(
        ctx,
        query,
        trade_identifier_list_order(order, {"trade_id", "id_type"}, false),
        trade_identifier_filter_condition(filter),
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading latest trade identifiers by trade_id.");
}

std::uint32_t trade_identifier_repository::get_total_identifier_count_by_trade_id(
    context ctx,
    const std::string& trade_id,
    const std::optional<messaging::trade_identifiers_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade identifiers count. trade_id: "
                               << trade_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_count_query<trade_identifier_entity>(
        ctx,
        query,
        trade_identifier_filter_condition(filter),
        lg(),
        "Counting trade identifiers by trade_id");
}


trade_identifier_repository::remove_status
trade_identifier_repository::remove(context ctx,
                                    const std::string& trade_id,
                                    const std::string& id_type,
                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade identifier. " << "trade_id: " << trade_id
                               << " id_type: " << id_type;
    const auto current = read_latest(ctx, trade_id, id_type);
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
    const auto query = query_builder::delete_from<trade_identifier_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "id_type"_c == id_type && "valid_to"_c == max &&
                                            "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing trade identifier from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, id_type).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void trade_identifier_repository::remove(context ctx,
                                         const std::string& trade_id,
                                         const std::string& id_type) {
    static_cast<void>(remove(ctx, trade_id, id_type, std::nullopt));
}

std::vector<domain::trade_identifier> trade_identifier_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::trade_identifiers_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade identifiers with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_identifier_entity,
                                                     domain::trade_identifier>(
        ctx,
        query,
        trade_identifier_list_order(order, {"trade_id", "id_type"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_identifier_filter_condition(filter)),
        [](const auto& entities) { return trade_identifier_mapper::map(entities); },
        lg(),
        "Reading latest trade identifiers with pagination.");
}

std::uint32_t trade_identifier_repository::get_total_identifier_count(
    context ctx,
    const std::optional<messaging::trade_identifiers_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade identifier count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<trade_identifier_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_identifier_filter_condition(filter)),
        lg(),
        "Counting trade identifiers");
}

std::vector<domain::trade_identifier>
trade_identifier_repository::read_latest(context ctx,
                                         const std::vector<std::string>& trade_ids,
                                         const std::vector<std::string>& id_types) {
    if (trade_ids.empty() || id_types.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "id_type"_c.in(id_types) && "valid_to"_c == max);
    auto result =
        query_builder::execute_read_query<trade_identifier_entity, domain::trade_identifier>(
            ctx,
            query,
            [](const auto& entities) { return trade_identifier_mapper::map(entities); },
            lg(),
            "Reading latest trade identifiers by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (id_types.size() != trade_ids.size())
        throw std::invalid_argument(
            "trade_identifier_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], id_types[i]);
    std::vector<domain::trade_identifier> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.trade_id), item.id_type)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void trade_identifier_repository::remove(context ctx,
                                         const std::vector<std::string>& trade_ids,
                                         const std::vector<std::string>& id_types) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (id_types.size() != trade_ids.size())
        throw std::invalid_argument(
            "trade_identifier_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], id_types[i]);
}


}
