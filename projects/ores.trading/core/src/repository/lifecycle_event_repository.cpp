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
#include "ores.trading.core/repository/lifecycle_event_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/lifecycle_event.hpp"
#include "ores.trading.api/domain/lifecycle_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/lifecycle_event_protocol.hpp"
#include "ores.trading.core/repository/lifecycle_event_entity.hpp"
#include "ores.trading.core/repository/lifecycle_event_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
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

std::string lifecycle_event_repository::sql() {
    return generate_create_table_sql<lifecycle_event_entity>(lg());
}

bool lifecycle_event_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"code"});
    if (!lifecycle_event_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of lifecycle events cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"code"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::lifecycle_events_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->code_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->code_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("code", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
lifecycle_event_repository::replace_claim(context ctx, const domain::lifecycle_event& v) {
    const auto current = read_latest(ctx, v.code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::lifecycle_event
lifecycle_event_repository::apply_claim(context ctx,
                                        const domain::lifecycle_event& v,
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
            const auto current = read_latest(ctx, v.code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void lifecycle_event_repository::write(context ctx, const domain::lifecycle_event& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void lifecycle_event_repository::write(context ctx, const std::vector<domain::lifecycle_event>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void lifecycle_event_repository::write(context ctx,
                                       const domain::lifecycle_event& v,
                                       const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing lifecycle event. " << "code: " << v.code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, lifecycle_event_mapper::map(t), lg(), "Writing lifecycle event to database.");
}

void lifecycle_event_repository::write(
    context ctx,
    const std::vector<domain::lifecycle_event>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing lifecycle events. Count: " << v.size();
    std::vector<domain::lifecycle_event> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, lifecycle_event_mapper::map(batch), lg(), "Writing lifecycle events to database.");
}

std::vector<domain::lifecycle_event> lifecycle_event_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle events");
}

std::vector<domain::lifecycle_event>
lifecycle_event_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest lifecycle event. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<lifecycle_event_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle event by code.");
}


std::vector<domain::lifecycle_event> lifecycle_event_repository::read_all(context ctx,
                                                                          const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all lifecycle event versions. " << "code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading all lifecycle event versions by code.");
}

std::optional<domain::lifecycle_event> lifecycle_event_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading lifecycle event at version. " << "code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading lifecycle event at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

lifecycle_event_repository::remove_status lifecycle_event_repository::remove(
    context ctx, const std::string& code, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing lifecycle event. " << "code: " << code;
    const auto current = read_latest(ctx, code);
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
    const auto query = sqlgen::delete_from<lifecycle_event_entity> |
                       where("tenant_id"_c == tid && "code"_c == code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing lifecycle event from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void lifecycle_event_repository::remove(context ctx, const std::string& code) {
    static_cast<void>(remove(ctx, code, std::nullopt));
}

std::vector<domain::lifecycle_event> lifecycle_event_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::lifecycle_events_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest lifecycle events with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        list_order(order, {"code"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle events with pagination.");
}

std::uint32_t lifecycle_event_repository::get_total_event_count(
    context ctx,
    const std::optional<messaging::lifecycle_events_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active lifecycle event count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<lifecycle_event_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<lifecycle_event_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), filter_condition(filter)),
        lg(),
        "Counting lifecycle events");
}

std::vector<domain::lifecycle_event>
lifecycle_event_repository::read_latest(context ctx, const std::vector<std::string>& codes) {
    if (codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<lifecycle_event_entity>> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    auto result = execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle events by ids.");
    return result;
}

void lifecycle_event_repository::remove(context ctx, const std::vector<std::string>& codes) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (codes.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lifecycle_event_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing lifecycle events.");
}


}
