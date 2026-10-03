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
#include "ores.trading.core/repository/composite_leg_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.trading.api/domain/composite_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/composite_leg_entity.hpp"
#include "ores.trading.core/repository/composite_leg_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string composite_leg_repository::sql() {
    return generate_create_table_sql<composite_leg_entity>(lg());
}

bool composite_leg_repository::is_sortable(std::string_view field) {
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
    if (!composite_leg_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of composite legs cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id"});
}

}

ores::utility::domain::precondition
composite_leg_repository::replace_claim(context ctx, const domain::composite_leg& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::composite_leg composite_leg_repository::apply_claim(
    context ctx, const domain::composite_leg& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.id));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void composite_leg_repository::write(context ctx, const domain::composite_leg& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void composite_leg_repository::write(context ctx, const std::vector<domain::composite_leg>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void composite_leg_repository::write(context ctx,
                                     const domain::composite_leg& v,
                                     const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing composite leg. " << "id: " << v.identity.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, composite_leg_mapper::map(t), lg(), "Writing composite leg to database.");
}

void composite_leg_repository::write(
    context ctx,
    const std::vector<domain::composite_leg>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing composite legs. Count: " << v.size();
    std::vector<domain::composite_leg> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, composite_leg_mapper::map(batch), lg(), "Writing composite legs to database.");
}

std::vector<domain::composite_leg> composite_leg_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("id"_c);
        return execute_read_query<composite_leg_entity, domain::composite_leg>(
            ctx,
            query,
            [](const auto& entities) { return composite_leg_mapper::map(entities); },
            lg(),
            "Reading latest composite legs (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<composite_leg_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading latest composite legs");
}

std::vector<domain::composite_leg> composite_leg_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest composite leg. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value());

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading latest composite leg by id.");
}


std::vector<domain::composite_leg> composite_leg_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all composite leg versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading all composite leg versions by id.");
}

std::optional<domain::composite_leg> composite_leg_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading composite leg at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading composite leg at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::composite_leg>
composite_leg_repository::read_latest_by_trade_id(context ctx,
                                                  const std::string& trade_id,
                                                  std::uint32_t offset,
                                                  std::uint32_t limit,
                                                  const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest composite legs. trade_id: " << trade_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c == trade_id && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading latest composite legs by trade_id.");
}

std::uint32_t
composite_leg_repository::get_total_composite_leg_count_by_trade_id(context ctx,
                                                                    const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active composite legs count. trade_id: "
                               << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::select_from<composite_leg_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c == trade_id && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active composite legs count by trade_id: " << count;
    return count;
}


composite_leg_repository::remove_status composite_leg_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing composite leg. " << "id: " << id;
    const auto current = read_latest(ctx, id);
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
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<composite_leg_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing composite leg from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void composite_leg_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::composite_leg>
composite_leg_repository::read_latest(context ctx,
                                      std::uint32_t offset,
                                      std::uint32_t limit,
                                      const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest composite legs with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<composite_leg_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading latest composite legs with pagination.");
}

std::uint32_t composite_leg_repository::get_total_composite_leg_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active composite leg count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<composite_leg_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active composite leg count: " << count;
    return count;
}

std::vector<domain::composite_leg>
composite_leg_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c.in(ids) &&
                             "valid_to"_c == max.value());
    auto result = execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx,
        query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(),
        "Reading latest composite legs by ids.");
    return result;
}

void composite_leg_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<composite_leg_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c.in(ids) &&
                             "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing composite legs.");
}


}
