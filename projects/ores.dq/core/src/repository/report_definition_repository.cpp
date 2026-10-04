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
#include "ores.dq.core/repository/report_definition_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/report_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/report_definition_entity.hpp"
#include "ores.dq.core/repository/report_definition_mapper.hpp"
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

std::string report_definition_repository::sql() {
    return generate_create_table_sql<report_definition_entity>(lg());
}

bool report_definition_repository::is_sortable(std::string_view field) {
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
    if (!report_definition_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of report definitions cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

}

ores::utility::domain::precondition
report_definition_repository::replace_claim(context ctx, const domain::report_definition& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::report_definition
report_definition_repository::apply_claim(context ctx,
                                          const domain::report_definition& v,
                                          const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "report_definition_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "report_definition_repository::write: a current row already exists");
    return t;
}

void report_definition_repository::write(context ctx, const domain::report_definition& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void report_definition_repository::write(context ctx,
                                         const std::vector<domain::report_definition>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void report_definition_repository::write(context ctx,
                                         const domain::report_definition& v,
                                         const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report definition. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(report_definition_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void report_definition_repository::write(
    context ctx,
    const std::vector<domain::report_definition>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report definitions. Count: " << v.size();
    std::vector<domain::report_definition> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(report_definition_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::report_definition> report_definition_repository::read_latest(context ctx) {
    const auto query = sqlgen::read<std::vector<report_definition_entity>> | order_by("id"_c);

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx,
        query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(),
        "Reading latest report definitions");
}

std::vector<domain::report_definition>
report_definition_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report definition. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<report_definition_entity>> | where("id"_c == id);

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx,
        query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(),
        "Reading latest report definition by id.");
}


std::vector<domain::report_definition>
report_definition_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report definition versions. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<report_definition_entity>> | where("id"_c == id) |
                       order_by("id"_c);

    return execute_read_query<report_definition_entity, domain::report_definition>(
        ctx,
        query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(),
        "Reading all report definition versions by id.");
}


report_definition_repository::remove_status report_definition_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report definition. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<report_definition_entity> | where("tenant_id"_c == tid && "id"_c == id);

    execute_delete_query(ctx, query, lg(), "Removing report definition from database.");
    return remove_status::removed;
}

void report_definition_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::report_definition>
report_definition_repository::read_latest(context ctx,
                                          std::uint32_t offset,
                                          std::uint32_t limit,
                                          const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report definitions with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<report_definition_entity>> |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<report_definition_entity, domain::report_definition>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(),
        "Reading latest report definitions with pagination.");
}

std::uint32_t report_definition_repository::get_total_definition_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active report definition count";

    struct count_result {
        long long count;
    };

    const auto query =
        sqlgen::select_from<report_definition_entity>(sqlgen::count().as<"count">()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active report definition count: " << count;
    return count;
}

std::vector<domain::report_definition>
report_definition_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto query = sqlgen::read<std::vector<report_definition_entity>> | where("id"_c.in(ids));
    auto result = execute_read_query<report_definition_entity, domain::report_definition>(
        ctx,
        query,
        [](const auto& entities) { return report_definition_mapper::map(entities); },
        lg(),
        "Reading latest report definitions by ids.");
    return result;
}

void report_definition_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<report_definition_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids));
    execute_delete_query(ctx, query, lg(), "Batch removing report definitions.");
}


}
