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
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/fra_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/fra_instrument_protocol.hpp"
#include "ores.trading.core/repository/fra_instrument_entity.hpp"
#include "ores.trading.core/repository/fra_instrument_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
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
#include <utility>
#include <vector>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string fra_instrument_repository::sql() {
    return generate_create_table_sql<fra_instrument_entity>(lg());
}

bool fra_instrument_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"trade_id"});
    if (!fra_instrument_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of FRA instruments cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"trade_id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::fra_instruments_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->trade_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->trade_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("trade_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
fra_instrument_repository::replace_claim(context ctx, const domain::fra_instrument& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.trade_id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::fra_instrument
fra_instrument_repository::apply_claim(context ctx,
                                       const domain::fra_instrument& v,
                                       const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.trade_id));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void fra_instrument_repository::write(context ctx, const domain::fra_instrument& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void fra_instrument_repository::write(context ctx, const std::vector<domain::fra_instrument>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void fra_instrument_repository::write(context ctx,
                                      const domain::fra_instrument& v,
                                      const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FRA instrument. " << "trade_id: " << v.identity.trade_id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, fra_instrument_mapper::map(t), lg(), "Writing FRA instrument to database.");
}

void fra_instrument_repository::write(
    context ctx,
    const std::vector<domain::fra_instrument>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FRA instruments. Count: " << v.size();
    std::vector<domain::fra_instrument> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, fra_instrument_mapper::map(batch), lg(), "Writing FRA instruments to database.");
}

std::vector<domain::fra_instrument> fra_instrument_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_instrument_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c);

    return execute_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FRA instruments");
}

std::vector<domain::fra_instrument>
fra_instrument_repository::read_latest(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FRA instrument. " << "trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<fra_instrument_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "valid_to"_c == max.value());

    return execute_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FRA instrument by trade_id.");
}


std::vector<domain::fra_instrument>
fra_instrument_repository::read_all(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all FRA instrument versions. "
                               << "trade_id: " << trade_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_instrument_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading all FRA instrument versions by trade_id.");
}

std::optional<domain::fra_instrument> fra_instrument_repository::read_at_version(
    context ctx, const std::string& trade_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading FRA instrument at version. " << "trade_id: " << trade_id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<fra_instrument_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading FRA instrument at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


fra_instrument_repository::remove_status fra_instrument_repository::remove(
    context ctx, const std::string& trade_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FRA instrument. " << "trade_id: " << trade_id;
    const auto current = read_latest(ctx, trade_id);
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
    const auto query = sqlgen::delete_from<fra_instrument_entity> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing FRA instrument from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void fra_instrument_repository::remove(context ctx, const std::string& trade_id) {
    static_cast<void>(remove(ctx, trade_id, std::nullopt));
}

std::vector<domain::fra_instrument> fra_instrument_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::fra_instruments_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FRA instruments with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_instrument_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        list_order(order, {"trade_id"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FRA instruments with pagination.");
}

std::uint32_t fra_instrument_repository::get_total_fra_instrument_count(
    context ctx,
    const std::optional<messaging::fra_instruments_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active FRA instrument count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<fra_instrument_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<fra_instrument_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), filter_condition(filter)),
        lg(),
        "Counting FRA instruments");
}

std::vector<domain::fra_instrument>
fra_instrument_repository::read_latest(context ctx, const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<fra_instrument_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<fra_instrument_entity, domain::fra_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fra_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FRA instruments by ids.");
    return result;
}

void fra_instrument_repository::remove(context ctx, const std::vector<std::string>& trade_ids) {
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
    const auto query =
        sqlgen::delete_from<fra_instrument_entity> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing FRA instruments.");
}


}
