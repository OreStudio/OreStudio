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
#include "ores.dq.core/repository/netting_set_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/netting_set_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/netting_set_entity.hpp"
#include "ores.dq.core/repository/netting_set_mapper.hpp"
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

std::string netting_set_repository::sql() {
    return generate_create_table_sql<netting_set_entity>(lg());
}

bool netting_set_repository::is_sortable(std::string_view field) {
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
    if (!netting_set_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of netting sets cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"code"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::netting_sets_filter>& filter) {
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
netting_set_repository::replace_claim(context ctx, const domain::netting_set& v) {
    const auto current = read_latest(ctx, v.code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::netting_set netting_set_repository::apply_claim(
    context ctx, const domain::netting_set& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "netting_set_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !read_latest(ctx, v.code).empty())
        throw std::invalid_argument("netting_set_repository::write: a current row already exists");
    return t;
}

void netting_set_repository::write(context ctx, const domain::netting_set& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void netting_set_repository::write(context ctx, const std::vector<domain::netting_set>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void netting_set_repository::write(context ctx,
                                   const domain::netting_set& v,
                                   const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing netting set. " << "code: " << v.code;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(netting_set_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void netting_set_repository::write(context ctx,
                                   const std::vector<domain::netting_set>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing netting sets. Count: " << v.size();
    std::vector<domain::netting_set> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(netting_set_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::netting_set> netting_set_repository::read_latest(context ctx) {
    const auto query = sqlgen::read<std::vector<netting_set_entity>> | order_by("code"_c);

    return execute_read_query<netting_set_entity, domain::netting_set>(
        ctx,
        query,
        [](const auto& entities) { return netting_set_mapper::map(entities); },
        lg(),
        "Reading latest netting sets");
}

std::vector<domain::netting_set> netting_set_repository::read_latest(context ctx,
                                                                     const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest netting set. " << "code: " << code;
    const auto query = sqlgen::read<std::vector<netting_set_entity>> | where("code"_c == code);

    return execute_read_query<netting_set_entity, domain::netting_set>(
        ctx,
        query,
        [](const auto& entities) { return netting_set_mapper::map(entities); },
        lg(),
        "Reading latest netting set by code.");
}


std::vector<domain::netting_set> netting_set_repository::read_all(context ctx,
                                                                  const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all netting set versions. " << "code: " << code;
    const auto query = sqlgen::read<std::vector<netting_set_entity>> | where("code"_c == code) |
                       order_by("code"_c);

    return execute_read_query<netting_set_entity, domain::netting_set>(
        ctx,
        query,
        [](const auto& entities) { return netting_set_mapper::map(entities); },
        lg(),
        "Reading all netting set versions by code.");
}


netting_set_repository::remove_status netting_set_repository::remove(
    context ctx, const std::string& code, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing netting set. " << "code: " << code;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, code);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<netting_set_entity> | where("tenant_id"_c == tid && "code"_c == code);

    execute_delete_query(ctx, query, lg(), "Removing netting set from database.");
    return remove_status::removed;
}

void netting_set_repository::remove(context ctx, const std::string& code) {
    static_cast<void>(remove(ctx, code, std::nullopt));
}

std::vector<domain::netting_set>
netting_set_repository::read_latest(context ctx,
                                    std::uint32_t offset,
                                    std::uint32_t limit,
                                    const ores::utility::domain::order& order,
                                    const std::optional<messaging::netting_sets_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest netting sets with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<netting_set_entity>> | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_ordered_read_query<netting_set_entity, domain::netting_set>(
        ctx,
        query,
        list_order(order, {"code"}, false),
        filter_condition(filter),
        [](const auto& entities) { return netting_set_mapper::map(entities); },
        lg(),
        "Reading latest netting sets with pagination.");
}

std::uint32_t netting_set_repository::get_total_netting_set_count(
    context ctx, const std::optional<messaging::netting_sets_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active netting set count";

    const auto query = sqlgen::read<std::vector<netting_set_entity>>;

    return execute_count_query<netting_set_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting netting sets");
}

std::vector<domain::netting_set>
netting_set_repository::read_latest(context ctx, const std::vector<std::string>& codes) {
    if (codes.empty())
        return {};
    const auto query = sqlgen::read<std::vector<netting_set_entity>> | where("code"_c.in(codes));
    auto result = execute_read_query<netting_set_entity, domain::netting_set>(
        ctx,
        query,
        [](const auto& entities) { return netting_set_mapper::map(entities); },
        lg(),
        "Reading latest netting sets by ids.");
    return result;
}

void netting_set_repository::remove(context ctx, const std::vector<std::string>& codes) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (codes.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<netting_set_entity> | where("tenant_id"_c == tid && "code"_c.in(codes));
    execute_delete_query(ctx, query, lg(), "Batch removing netting sets.");
}


}
