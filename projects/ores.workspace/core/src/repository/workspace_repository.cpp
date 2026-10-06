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
#include "ores.workspace.core/repository/workspace_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/hierarchy.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include "ores.workspace.api/domain/workspace_json_io.hpp" // IWYU pragma: keep.
#include "ores.workspace.api/messaging/workspace_protocol.hpp"
#include "ores.workspace.core/repository/workspace_entity.hpp"
#include "ores.workspace.core/repository/workspace_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
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

namespace ores::workspace::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string workspace_repository::sql() {
    return generate_create_table_sql<workspace_entity>(lg());
}

bool workspace_repository::is_sortable(std::string_view field) {
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
    if (!workspace_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of workspaces cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::workspaces_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
workspace_repository::replace_claim(context ctx, const domain::workspace& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::workspace workspace_repository::apply_claim(
    context ctx, const domain::workspace& v, const ores::utility::domain::precondition& claim) {
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

void workspace_repository::write(context ctx, const domain::workspace& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void workspace_repository::write(context ctx, const std::vector<domain::workspace>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void workspace_repository::write(context ctx,
                                 const domain::workspace& v,
                                 const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workspace. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, workspace_mapper::map(t), lg(), "Writing workspace to database.");
}

void workspace_repository::write(context ctx,
                                 const std::vector<domain::workspace>& v,
                                 const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workspaces. Count: " << v.size();
    std::vector<domain::workspace> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx, workspace_mapper::map(batch), lg(), "Writing workspaces to database.");
}

std::vector<domain::workspace> workspace_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading latest workspaces");
}

std::vector<domain::workspace> workspace_repository::read_latest(context ctx,
                                                                 const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest workspace. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading latest workspace by id.");
}


std::vector<domain::workspace> workspace_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all workspace versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading all workspace versions by id.");
}

std::optional<domain::workspace>
workspace_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading workspace at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading workspace at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


workspace_repository::remove_status workspace_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workspace. " << "id: " << id;
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
    const auto query = sqlgen::delete_from<workspace_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing workspace from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void workspace_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::workspace>
workspace_repository::read_latest(context ctx,
                                  std::uint32_t offset,
                                  std::uint32_t limit,
                                  const ores::utility::domain::order& order,
                                  const std::optional<messaging::workspaces_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest workspaces with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading latest workspaces with pagination.");
}

std::uint32_t workspace_repository::get_total_workspace_count(
    context ctx, const std::optional<messaging::workspaces_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active workspace count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value());

    return execute_count_query<workspace_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting workspaces");
}

std::vector<domain::workspace>
workspace_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<workspace_entity, domain::workspace>(
        ctx,
        query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(),
        "Reading latest workspaces by ids.");
    return result;
}

void workspace_repository::remove(context ctx, const std::vector<std::string>& ids) {
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
    const auto query = sqlgen::delete_from<workspace_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing workspaces.");
}

std::vector<ores::utility::domain::hierarchy_flat_row> workspace_repository::get_hierarchy(
    context ctx, const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Reading workspace hierarchy. Root: " << root_id
                               << " from_root: " << from_root;

    const auto tenant_str = boost::uuids::to_string(ctx.tenant_id().to_uuid());
    const auto root_str = boost::uuids::to_string(root_id);
    const std::string sql = "SELECT * FROM ores_workspaces_hierarchy_fn('" + tenant_str +
                            "'::uuid, '" + root_str + "'::uuid, " + (from_root ? "true" : "false") +
                            ")";

    const auto rows = execute_raw_multi_column_query(ctx, sql, lg(), "Reading workspace hierarchy");

    std::vector<ores::utility::domain::hierarchy_flat_row> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() >= 3 && row[0]) {
            ores::utility::domain::hierarchy_flat_row r;
            r.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            if (row[1])
                r.parent_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
            if (row[2])
                r.name = *row[2];
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read " << result.size() << " workspace hierarchy rows.";
    return result;
}


std::vector<std::string> workspace_repository::resolution_order(context ctx,
                                                                const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace chain for: " << workspace_id;
    const auto tid = ctx.tenant_id().to_string();
    const std::string sql =
        "SELECT unnest(ores_workspace_resolution_order_fn($1::uuid, $2::uuid))::text";
    return execute_parameterized_string_query(
        ctx, sql, {workspace_id, tid}, lg(), "Resolving workspace resolution order");
}

void workspace_repository::set_trade_scope(context ctx,
                                           const std::string& workspace_id,
                                           const std::vector<boost::uuids::uuid>& trade_ids) {
    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id
                               << " count: " << trade_ids.size();
    execute_parameterized_command(
        ctx,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id},
        lg(),
        "Clearing existing trade scope");

    for (const auto& tid : trade_ids) {
        execute_parameterized_command(
            ctx,
            "INSERT INTO ores_workspace_trade_scope_tbl (workspace_id, trade_id)"
            " VALUES ($1::uuid, $2::uuid)",
            {workspace_id, boost::uuids::to_string(tid)},
            lg(),
            "Inserting trade scope entry");
    }
}

void workspace_repository::clear_trade_scope(context ctx, const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;
    execute_parameterized_command(
        ctx,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id},
        lg(),
        "Clearing trade scope");
}

}
