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
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/range_bound.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/domain/trade_booking_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.trading.core/repository/trade_booking_entity.hpp"
#include "ores.trading.core/repository/trade_booking_mapper.hpp"
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

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_booking_repository::sql() {
    return query_builder::insert_statement_sql<trade_booking_entity>(lg());
}

bool trade_booking_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order trade_booking_list_order(const ores::utility::domain::order& order,
                                              std::initializer_list<std::string> default_columns,
                                              bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"trade_id"});
    if (!trade_booking_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of trade bookings cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"trade_id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition>
trade_booking_filter_condition(const std::optional<messaging::trade_bookings_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->trade_date)
        r.push_back(*filter->trade_date ?
                        query_builder::equals("trade_date",
                                              query_builder::filter_value(**filter->trade_date)) :
                        query_builder::is_null("trade_date"));
    if (filter->execution_timestamp)
        r.push_back(
            *filter->execution_timestamp ?
                query_builder::equals("execution_timestamp",
                                      query_builder::filter_value(**filter->execution_timestamp)) :
                query_builder::is_null("execution_timestamp"));
    if (filter->trade_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->trade_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("trade_id", std::move(values)));
    }
    if (filter->trade_date_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->trade_date_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("trade_date", std::move(values)));
    }
    if (filter->execution_timestamp_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->execution_timestamp_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("execution_timestamp", std::move(values)));
    }
    if (filter->trade_date_from) {
        const auto bound = range_bound(*filter->trade_date_from, range_end::from, false);
        if (!bound)
            throw std::invalid_argument("The bound " + *filter->trade_date_from +
                                        " of trade_date_from is not a valid date or time.");
        r.push_back(query_builder::at_least("trade_date", query_builder::filter_value(*bound)));
    }
    if (filter->trade_date_to) {
        const auto bound = range_bound(*filter->trade_date_to, range_end::to, false);
        if (!bound)
            throw std::invalid_argument("The bound " + *filter->trade_date_to +
                                        " of trade_date_to is not a valid date or time.");
        r.push_back(query_builder::at_most("trade_date", query_builder::filter_value(*bound)));
    }
    if (filter->execution_timestamp_from) {
        const auto bound = range_bound(*filter->execution_timestamp_from, range_end::from, true);
        if (!bound)
            throw std::invalid_argument(
                "The bound " + *filter->execution_timestamp_from +
                " of execution_timestamp_from is not a valid date or time.");
        r.push_back(
            query_builder::at_least("execution_timestamp", query_builder::filter_value(*bound)));
    }
    if (filter->execution_timestamp_to) {
        const auto bound = range_bound(*filter->execution_timestamp_to, range_end::to, true);
        if (!bound)
            throw std::invalid_argument("The bound " + *filter->execution_timestamp_to +
                                        " of execution_timestamp_to is not a valid date or time.");
        r.push_back(
            query_builder::at_most("execution_timestamp", query_builder::filter_value(*bound)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition
trade_booking_repository::replace_claim(context ctx, const domain::trade_booking& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::trade_booking trade_booking_repository::apply_claim(
    context ctx, const domain::trade_booking& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.trade_id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void trade_booking_repository::write(context ctx, const domain::trade_booking& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void trade_booking_repository::write(context ctx, const std::vector<domain::trade_booking>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void trade_booking_repository::write(context ctx,
                                     const domain::trade_booking& v,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade booking. " << "trade_id: " << v.trade_id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(
        ctx, trade_booking_mapper::map(t), lg(), "Writing trade booking to database.");
}

void trade_booking_repository::write(
    context ctx,
    const std::vector<domain::trade_booking>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade bookings. Count: " << v.size();
    std::vector<domain::trade_booking> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(
        ctx, trade_booking_mapper::map(batch), lg(), "Writing trade bookings to database.");
}

std::vector<domain::trade_booking> trade_booking_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("trade_id"_c);

    return query_builder::execute_read_query<trade_booking_entity, domain::trade_booking>(
        ctx,
        query,
        [](const auto& entities) { return trade_booking_mapper::map(entities); },
        lg(),
        "Reading latest trade bookings");
}

std::vector<domain::trade_booking>
trade_booking_repository::read_latest(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade booking. " << "trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max.value());

    return query_builder::execute_read_query<trade_booking_entity, domain::trade_booking>(
        ctx,
        query,
        [](const auto& entities) { return trade_booking_mapper::map(entities); },
        lg(),
        "Reading latest trade booking by trade_id.");
}


std::vector<domain::trade_booking> trade_booking_repository::read_all(context ctx,
                                                                      const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade booking versions. "
                               << "trade_id: " << trade_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<trade_booking_entity, domain::trade_booking>(
        ctx,
        query,
        [](const auto& entities) { return trade_booking_mapper::map(entities); },
        lg(),
        "Reading all trade booking versions by trade_id.");
}

std::optional<domain::trade_booking> trade_booking_repository::read_at_version(
    context ctx, const std::string& trade_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade booking at version. " << "trade_id: " << trade_id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "version"_c == version) |
                       query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<trade_booking_entity, domain::trade_booking>(
            ctx,
            query,
            [](const auto& entities) { return trade_booking_mapper::map(entities); },
            lg(),
            "Reading trade booking at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


trade_booking_repository::remove_status trade_booking_repository::remove(
    context ctx, const std::string& trade_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade booking. " << "trade_id: " << trade_id;
    const auto current = read_latest(ctx, trade_id);
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
    const auto query = query_builder::delete_from<trade_booking_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                            "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing trade booking from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void trade_booking_repository::remove(context ctx, const std::string& trade_id) {
    static_cast<void>(remove(ctx, trade_id, std::nullopt));
}

std::vector<domain::trade_booking>
trade_booking_repository::read_latest(context ctx,
                                      std::uint32_t offset,
                                      std::uint32_t limit,
                                      const ores::utility::domain::order& order,
                                      const std::optional<messaging::trade_bookings_filter>& filter,
                                      const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade bookings with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<trade_booking_entity, domain::trade_booking>(
        ctx,
        query,
        trade_booking_list_order(order, {"trade_id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_booking_filter_condition(filter)),
        [](const auto& entities) { return trade_booking_mapper::map(entities); },
        lg(),
        "Reading latest trade bookings with pagination.");
}

std::uint32_t trade_booking_repository::get_total_trade_booking_count(
    context ctx,
    const std::optional<messaging::trade_bookings_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade booking count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<trade_booking_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                trade_booking_filter_condition(filter)),
        lg(),
        "Counting trade bookings");
}

std::vector<domain::trade_booking>
trade_booking_repository::read_latest(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<trade_booking_entity>> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value());
    auto result = query_builder::execute_read_query<trade_booking_entity, domain::trade_booking>(
        ctx,
        query,
        [](const auto& entities) { return trade_booking_mapper::map(entities); },
        lg(),
        "Reading latest trade bookings by ids.");
    return result;
}

void trade_booking_repository::remove(context ctx, const std::vector<std::string>& trade_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (trade_ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::delete_from<trade_booking_entity> |
                       query_builder::where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) &&
                                            "valid_to"_c == max.value());
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing trade bookings.");
}


}
