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
#include "ores.dq.core/repository/lei_relationship_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/lei_relationship_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/lei_relationship_entity.hpp"
#include "ores.dq.core/repository/lei_relationship_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lei_relationship_repository::sql() {
    return generate_create_table_sql<lei_relationship_entity>(lg());
}

bool lei_relationship_repository::is_sortable(std::string_view field) {
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
                          {"relationship_start_node_node_id"});
    if (!lei_relationship_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of LEI relationships cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"relationship_start_node_node_id"});
}

}

ores::utility::domain::precondition
lei_relationship_repository::replace_claim(context ctx, const domain::lei_relationship& v) {
    const auto current = read_latest(ctx, v.relationship_start_node_node_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::lei_relationship
lei_relationship_repository::apply_claim(context ctx,
                                         const domain::lei_relationship& v,
                                         const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "lei_relationship_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, v.relationship_start_node_node_id).empty())
        throw std::invalid_argument(
            "lei_relationship_repository::write: a current row already exists");
    return t;
}

void lei_relationship_repository::write(context ctx, const domain::lei_relationship& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void lei_relationship_repository::write(context ctx,
                                        const std::vector<domain::lei_relationship>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void lei_relationship_repository::write(context ctx,
                                        const domain::lei_relationship& v,
                                        const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << v.relationship_start_node_node_id;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(lei_relationship_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void lei_relationship_repository::write(
    context ctx,
    const std::vector<domain::lei_relationship>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationships. Count: " << v.size();
    std::vector<domain::lei_relationship> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(lei_relationship_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(context ctx) {
    const auto query = sqlgen::read<std::vector<lei_relationship_entity>> |
                       order_by("relationship_start_node_node_id"_c);

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships");
}

std::vector<domain::lei_relationship>
lei_relationship_repository::read_latest(context ctx,
                                         const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c == relationship_start_node_node_id);

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationship by relationship_start_node_node_id.");
}


std::vector<domain::lei_relationship>
lei_relationship_repository::read_all(context ctx,
                                      const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all LEI relationship versions. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c == relationship_start_node_node_id) |
        order_by("relationship_start_node_node_id"_c);

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading all LEI relationship versions by relationship_start_node_node_id.");
}


lei_relationship_repository::remove_status
lei_relationship_repository::remove(context ctx,
                                    const std::string& relationship_start_node_node_id,
                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, relationship_start_node_node_id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<lei_relationship_entity> |
                       where("tenant_id"_c == tid && "relationship_start_node_node_id"_c ==
                                                         relationship_start_node_node_id);

    execute_delete_query(ctx, query, lg(), "Removing LEI relationship from database.");
    return remove_status::removed;
}

void lei_relationship_repository::remove(context ctx,
                                         const std::string& relationship_start_node_node_id) {
    static_cast<void>(remove(ctx, relationship_start_node_node_id, std::nullopt));
}

std::vector<domain::lei_relationship>
lei_relationship_repository::read_latest(context ctx,
                                         std::uint32_t offset,
                                         std::uint32_t limit,
                                         const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationships with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<lei_relationship_entity>> | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_ordered_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        list_order(order, {"relationship_start_node_node_id"}, false),
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships with pagination.");
}

std::uint32_t lei_relationship_repository::get_total_relationship_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active LEI relationship count";

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<lei_relationship_entity>(sqlgen::count().as<"count">()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active LEI relationship count: " << count;
    return count;
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(
    context ctx, const std::vector<std::string>& relationship_start_node_node_ids) {
    if (relationship_start_node_node_ids.empty())
        return {};
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c.in(relationship_start_node_node_ids));
    auto result = execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships by ids.");
    return result;
}

void lei_relationship_repository::remove(
    context ctx, const std::vector<std::string>& relationship_start_node_node_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (relationship_start_node_node_ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<lei_relationship_entity> |
                       where("tenant_id"_c == tid && "relationship_start_node_node_id"_c.in(
                                                         relationship_start_node_node_ids));
    execute_delete_query(ctx, query, lg(), "Batch removing LEI relationships.");
}


}
