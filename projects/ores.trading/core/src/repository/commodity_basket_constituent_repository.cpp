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
#include "ores.trading.core/repository/commodity_basket_constituent_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent.hpp"
#include "ores.trading.api/domain/commodity_basket_constituent_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/commodity_basket_constituent_protocol.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_entity.hpp"
#include "ores.trading.core/repository/commodity_basket_constituent_mapper.hpp"
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

std::string commodity_basket_constituent_repository::sql() {
    return query_builder::insert_statement_sql<commodity_basket_constituent_entity>(lg());
}

bool commodity_basket_constituent_repository::is_sortable(std::string_view field) {
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
commodity_basket_constituent_list_order(const ores::utility::domain::order& order,
                                        std::initializer_list<std::string> default_columns,
                                        bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"trade_id", "sequence_number"});
    if (!commodity_basket_constituent_repository::is_sortable(order.field))
        throw std::invalid_argument(
            "A list of commodity basket constituents cannot be ordered by " + order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"trade_id", "sequence_number"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> commodity_basket_constituent_filter_condition(
    const std::optional<messaging::commodity_basket_constituents_filter>& filter) {
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

ores::utility::domain::precondition commodity_basket_constituent_repository::replace_claim(
    context ctx, const domain::commodity_basket_constituent& v) {
    const auto current =
        read_latest(ctx, boost::uuids::to_string(v.trade_id), std::to_string(v.sequence_number));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::commodity_basket_constituent commodity_basket_constituent_repository::apply_claim(
    context ctx,
    const domain::commodity_basket_constituent& v,
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
            const auto current = read_latest(
                ctx, boost::uuids::to_string(v.trade_id), std::to_string(v.sequence_number));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void commodity_basket_constituent_repository::write(context ctx,
                                                    const domain::commodity_basket_constituent& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void commodity_basket_constituent_repository::write(
    context ctx, const std::vector<domain::commodity_basket_constituent>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void commodity_basket_constituent_repository::write(
    context ctx,
    const domain::commodity_basket_constituent& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing commodity basket constituent. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       commodity_basket_constituent_mapper::map(t),
                                       lg(),
                                       "Writing commodity basket constituent to database.");
}

void commodity_basket_constituent_repository::write(
    context ctx,
    const std::vector<domain::commodity_basket_constituent>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing commodity basket constituents. Count: " << v.size();
    std::vector<domain::commodity_basket_constituent> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       commodity_basket_constituent_mapper::map(batch),
                                       lg(),
                                       "Writing commodity basket constituents to database.");
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(context ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "sequence_number"_c);

    return query_builder::execute_read_query<commodity_basket_constituent_entity,
                                             domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents");
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(context ctx,
                                                     const std::string& trade_id,
                                                     const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<commodity_basket_constituent_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max);

    return query_builder::execute_read_query<commodity_basket_constituent_entity,
                                             domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituent by trade_id.");
}


std::vector<domain::commodity_basket_constituent> commodity_basket_constituent_repository::read_all(
    context ctx, const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all commodity basket constituent versions. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "sequence_number"_c == sequence_number) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<commodity_basket_constituent_entity,
                                             domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading all commodity basket constituent versions by trade_id.");
}

std::optional<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_at_version(context ctx,
                                                         const std::string& trade_id,
                                                         const std::string& sequence_number,
                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading commodity basket constituent at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<commodity_basket_constituent_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<commodity_basket_constituent_entity,
                                                            domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading commodity basket constituent at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest_by_trade_id(
    context ctx,
    const std::string& trade_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::commodity_basket_constituents_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest commodity basket constituents. trade_id: "
                               << trade_id << " offset: " << offset << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<commodity_basket_constituent_entity,
                                                     domain::commodity_basket_constituent>(
        ctx,
        query,
        commodity_basket_constituent_list_order(order, {"trade_id", "sequence_number"}, false),
        commodity_basket_constituent_filter_condition(filter),
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents by trade_id.");
}

std::uint32_t
commodity_basket_constituent_repository::get_total_commodity_basket_constituent_count_by_trade_id(
    context ctx,
    const std::string& trade_id,
    const std::optional<messaging::commodity_basket_constituents_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active commodity basket constituents count. trade_id: " << trade_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max);

    return query_builder::execute_count_query<commodity_basket_constituent_entity>(
        ctx,
        query,
        commodity_basket_constituent_filter_condition(filter),
        lg(),
        "Counting commodity basket constituents by trade_id");
}


commodity_basket_constituent_repository::remove_status
commodity_basket_constituent_repository::remove(context ctx,
                                                const std::string& trade_id,
                                                const std::string& sequence_number,
                                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing commodity basket constituent. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto current = read_latest(ctx, trade_id, sequence_number);
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
    const auto query = query_builder::delete_from<commodity_basket_constituent_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "sequence_number"_c == sequence_number &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing commodity basket constituent from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id, sequence_number).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void commodity_basket_constituent_repository::remove(context ctx,
                                                     const std::string& trade_id,
                                                     const std::string& sequence_number) {
    static_cast<void>(remove(ctx, trade_id, sequence_number, std::nullopt));
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::commodity_basket_constituents_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest commodity basket constituents with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<commodity_basket_constituent_entity,
                                                     domain::commodity_basket_constituent>(
        ctx,
        query,
        commodity_basket_constituent_list_order(order, {"trade_id", "sequence_number"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                commodity_basket_constituent_filter_condition(filter)),
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents with pagination.");
}

std::uint32_t commodity_basket_constituent_repository::get_total_commodity_basket_constituent_count(
    context ctx,
    const std::optional<messaging::commodity_basket_constituents_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active commodity basket constituent count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<commodity_basket_constituent_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                commodity_basket_constituent_filter_condition(filter)),
        lg(),
        "Counting commodity basket constituents");
}

std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_latest(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    if (trade_ids.empty() || sequence_numbers.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<commodity_basket_constituent_entity>> |
        query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                             "sequence_number"_c.in(sequence_numbers) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<commodity_basket_constituent_entity,
                                                    domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading latest commodity basket constituents by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("commodity_basket_constituent_repository::read_latest: key "
                                    "column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        requested.emplace(trade_ids[i], sequence_numbers[i]);
    std::vector<domain::commodity_basket_constituent> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(boost::uuids::to_string(item.trade_id),
                                               std::to_string(item.sequence_number))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void commodity_basket_constituent_repository::remove(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("commodity_basket_constituent_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], sequence_numbers[i]);
}


std::vector<domain::commodity_basket_constituent>
commodity_basket_constituent_repository::read_by_instruments_batch(
    context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<commodity_basket_constituent_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("trade_id"_c, "sequence_number"_c);
    return query_builder::execute_read_query<commodity_basket_constituent_entity,
                                             domain::commodity_basket_constituent>(
        ctx,
        query,
        [](const auto& entities) { return commodity_basket_constituent_mapper::map(entities); },
        lg(),
        "Reading commodity basket constituents for multiple instruments.");
}

}
