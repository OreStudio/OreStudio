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
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/swap_leg_protocol.hpp"
#include "ores.trading.core/repository/swap_leg_entity.hpp"
#include "ores.trading.core/repository/swap_leg_mapper.hpp"
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
#include <sqlgen/dynamic/Condition.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/dynamic/Value.hpp>
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

std::string swap_leg_repository::sql() {
    return generate_create_table_sql<swap_leg_entity>(lg());
}

bool swap_leg_repository::is_sortable(std::string_view field) {
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
        return make_order(
            default_columns, default_descending != order.descending, {"trade_id", "leg_number"});
    if (!swap_leg_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of swap legs cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"trade_id", "leg_number"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::swap_legs_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->trade_id)
        r.push_back(equals("trade_id", filter_value(*filter->trade_id)));
    if (filter->trade_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->trade_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("trade_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition swap_leg_repository::replace_claim(context ctx,
                                                                       const domain::swap_leg& v) {
    const auto current = read_latest(
        ctx, boost::uuids::to_string(v.identity.trade_id), std::to_string(v.identity.leg_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::swap_leg swap_leg_repository::apply_claim(
    context ctx, const domain::swap_leg& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.identity.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.identity.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx,
                                             boost::uuids::to_string(v.identity.trade_id),
                                             std::to_string(v.identity.leg_number));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void swap_leg_repository::write(context ctx, const domain::swap_leg& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void swap_leg_repository::write(context ctx, const std::vector<domain::swap_leg>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void swap_leg_repository::write(context ctx,
                                const domain::swap_leg& v,
                                const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing swap leg. " << "trade_id: " << v.identity.trade_id
                               << " leg_number: " << v.identity.leg_number;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, swap_leg_mapper::map(t), lg(), "Writing swap leg to database.");
}

void swap_leg_repository::write(context ctx,
                                const std::vector<domain::swap_leg>& v,
                                const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing swap legs. Count: " << v.size();
    std::vector<domain::swap_leg> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx, swap_leg_mapper::map(batch), lg(), "Writing swap legs to database.");
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "leg_number"_c);

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs");
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest(context ctx,
                                                               const std::string& trade_id,
                                                               const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest swap leg. " << "trade_id: " << trade_id
                               << " leg_number: " << leg_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "leg_number"_c == leg_number && "valid_to"_c == max.value());

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap leg by trade_id.");
}


std::vector<domain::swap_leg> swap_leg_repository::read_all(context ctx,
                                                            const std::string& trade_id,
                                                            const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all swap leg versions. " << "trade_id: " << trade_id
                               << " leg_number: " << leg_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "leg_number"_c == leg_number) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading all swap leg versions by trade_id.");
}

std::optional<domain::swap_leg> swap_leg_repository::read_at_version(context ctx,
                                                                     const std::string& trade_id,
                                                                     const std::string& leg_number,
                                                                     std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading swap leg at version. " << "trade_id: " << trade_id
                               << " leg_number: " << leg_number << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "leg_number"_c == leg_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading swap leg at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::swap_leg> swap_leg_repository::read_latest_by_trade_id(
    context ctx,
    const std::string& trade_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::swap_legs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest swap legs. trade_id: " << trade_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        list_order(order, {"trade_id", "leg_number"}, false),
        filter_condition(filter),
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs by trade_id.");
}

std::uint32_t swap_leg_repository::get_total_swap_leg_count_by_trade_id(
    context ctx,
    const std::string& trade_id,
    const std::optional<messaging::swap_legs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active swap legs count. trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "valid_to"_c == max.value());

    return execute_count_query<swap_leg_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting swap legs by trade_id");
}


swap_leg_repository::remove_status
swap_leg_repository::remove(context ctx,
                            const std::string& trade_id,
                            const std::string& leg_number,
                            std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing swap leg. " << "trade_id: " << trade_id
                               << " leg_number: " << leg_number;
    const auto current = read_latest(ctx, trade_id, leg_number);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().identity.version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().identity.version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<swap_leg_entity> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "leg_number"_c == leg_number &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing swap leg from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, leg_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void swap_leg_repository::remove(context ctx,
                                 const std::string& trade_id,
                                 const std::string& leg_number) {
    static_cast<void>(remove(ctx, trade_id, leg_number, std::nullopt));
}

std::vector<domain::swap_leg>
swap_leg_repository::read_latest(context ctx,
                                 std::uint32_t offset,
                                 std::uint32_t limit,
                                 const ores::utility::domain::order& order,
                                 const std::optional<messaging::swap_legs_filter>& filter,
                                 const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest swap legs with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> | where("tenant_id"_c == tid) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        list_order(order, {"trade_id", "leg_number"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs with pagination.");
}

std::uint32_t swap_leg_repository::get_total_swap_leg_count(
    context ctx,
    const std::optional<messaging::swap_legs_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active swap leg count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<swap_leg_entity>(ctx,
                                                query,
                                                narrowed(valid_at(as_of), filter_condition(filter)),
                                                lg(),
                                                "Counting swap legs");
}

std::vector<domain::swap_leg>
swap_leg_repository::read_latest(context ctx,
                                 const std::vector<std::string>& trade_ids,
                                 const std::vector<std::string>& leg_numbers) {
    if (trade_ids.empty() || leg_numbers.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "leg_number"_c.in(leg_numbers) && "valid_to"_c == max.value());
    auto result = execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading latest swap legs by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (leg_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "swap_leg_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], leg_numbers[i]);
    std::vector<domain::swap_leg> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.identity.trade_id),
                                               std::to_string(item.identity.leg_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void swap_leg_repository::remove(context ctx,
                                 const std::vector<std::string>& trade_ids,
                                 const std::vector<std::string>& leg_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (leg_numbers.size() != trade_ids.size())
        throw std::invalid_argument(
            "swap_leg_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], leg_numbers[i]);
}


std::vector<domain::swap_leg>
swap_leg_repository::read_by_instruments_batch(context ctx,
                                               const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value()) |
        order_by("trade_id"_c, "leg_number"_c);
    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx,
        query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(),
        "Reading swap legs for multiple instruments.");
}

}
