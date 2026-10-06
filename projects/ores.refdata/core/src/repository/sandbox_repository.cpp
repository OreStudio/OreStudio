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
#include "ores.refdata.core/repository/sandbox_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/sandbox.hpp"
#include "ores.refdata.api/domain/sandbox_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/messaging/sandbox_protocol.hpp"
#include "ores.refdata.core/repository/sandbox_entity.hpp"
#include "ores.refdata.core/repository/sandbox_mapper.hpp"
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

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string sandbox_repository::sql() {
    return generate_create_table_sql<sandbox_entity>(lg());
}

bool sandbox_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!sandbox_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of sandboxes cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::sandboxes_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->anchor_portfolio_id)
        r.push_back(equals("anchor_portfolio_id", filter_value(*filter->anchor_portfolio_id)));
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    if (filter->anchor_portfolio_id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->anchor_portfolio_id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("anchor_portfolio_id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition sandbox_repository::replace_claim(context ctx,
                                                                      const domain::sandbox& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::sandbox sandbox_repository::apply_claim(context ctx,
                                                const domain::sandbox& v,
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

void sandbox_repository::write(context ctx, const domain::sandbox& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void sandbox_repository::write(context ctx, const std::vector<domain::sandbox>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void sandbox_repository::write(context ctx,
                               const domain::sandbox& v,
                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sandbox. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, sandbox_mapper::map(t), lg(), "Writing sandbox to database.");
}

void sandbox_repository::write(context ctx,
                               const std::vector<domain::sandbox>& v,
                               const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sandboxes. Count: " << v.size();
    std::vector<domain::sandbox> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx, sandbox_mapper::map(batch), lg(), "Writing sandboxes to database.");
}

std::vector<domain::sandbox> sandbox_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandboxes");
}

std::vector<domain::sandbox> sandbox_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sandbox. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandbox by id.");
}

std::vector<domain::sandbox> sandbox_repository::read_latest_by_name(context ctx,
                                                                     const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sandbox by name: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<sandbox_entity>> |
        where("tenant_id"_c == tid && "name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandbox by name.");
}

std::vector<domain::sandbox> sandbox_repository::read_any_by_name(context ctx,
                                                                  const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any sandbox by name: " << name;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "name"_c == name) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading any sandbox by name.");
}


std::vector<domain::sandbox> sandbox_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all sandbox versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading all sandbox versions by id.");
}

std::optional<domain::sandbox>
sandbox_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading sandbox at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading sandbox at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::sandbox> sandbox_repository::read_latest_by_anchor_portfolio_id(
    context ctx,
    const std::string& anchor_portfolio_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::sandboxes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sandboxes. anchor_portfolio_id: "
                               << anchor_portfolio_id << " offset: " << offset
                               << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<sandbox_entity>> |
        where("tenant_id"_c == tid && "anchor_portfolio_id"_c == anchor_portfolio_id &&
              "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandboxes by anchor_portfolio_id.");
}

std::uint32_t sandbox_repository::get_total_sandbox_count_by_anchor_portfolio_id(
    context ctx,
    const std::string& anchor_portfolio_id,
    const std::optional<messaging::sandboxes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active sandboxes count. anchor_portfolio_id: "
                               << anchor_portfolio_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<sandbox_entity>> |
        where("tenant_id"_c == tid && "anchor_portfolio_id"_c == anchor_portfolio_id &&
              "valid_to"_c == max.value());

    return execute_count_query<sandbox_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting sandboxes by anchor_portfolio_id");
}


sandbox_repository::remove_status sandbox_repository::remove(context ctx,
                                                             const std::string& id,
                                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing sandbox. " << "id: " << id;
    const auto current = read_latest(ctx, id);
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
    const auto query = sqlgen::delete_from<sandbox_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing sandbox from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void sandbox_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::sandbox>
sandbox_repository::read_latest(context ctx,
                                std::uint32_t offset,
                                std::uint32_t limit,
                                const ores::utility::domain::order& order,
                                const std::optional<messaging::sandboxes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sandboxes with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandboxes with pagination.");
}

std::uint32_t sandbox_repository::get_total_sandbox_count(
    context ctx, const std::optional<messaging::sandboxes_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active sandbox count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value());

    return execute_count_query<sandbox_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting sandboxes");
}

std::vector<domain::sandbox> sandbox_repository::read_latest(context ctx,
                                                             const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<sandbox_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<sandbox_entity, domain::sandbox>(
        ctx,
        query,
        [](const auto& entities) { return sandbox_mapper::map(entities); },
        lg(),
        "Reading latest sandboxes by ids.");
    return result;
}

void sandbox_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<sandbox_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing sandboxes.");
}


}
