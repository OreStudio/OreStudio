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
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/market_series_identity.hpp"
#include "ores.marketdata.api/domain/market_series_identity_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_series_identity_entity.hpp"
#include "ores.marketdata.core/repository/market_series_identity_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/begin_transaction.hpp>
#include <sqlgen/commit.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/insert.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_series_identity_repository::sql() {
    return generate_create_table_sql<market_series_identity_entity>(lg());
}

bool market_series_identity_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"series_id"});
    if (!market_series_identity_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of market series identities cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"series_id"});
}

}

ores::utility::domain::precondition
market_series_identity_repository::replace_claim(context ctx,
                                                 const domain::market_series_identity& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.series_id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::market_series_identity
market_series_identity_repository::apply_claim(context ctx,
                                               const domain::market_series_identity& v,
                                               const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "market_series_identity_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.series_id)).empty())
        throw std::invalid_argument(
            "market_series_identity_repository::write: a current row already exists");
    return t;
}

void market_series_identity_repository::write(context ctx,
                                              const domain::market_series_identity& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void market_series_identity_repository::write(
    context ctx, const std::vector<domain::market_series_identity>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void market_series_identity_repository::write(context ctx,
                                              const domain::market_series_identity& v,
                                              const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market series identity. "
                               << "series_id: " << v.series_id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(market_series_identity_mapper::map(t)),
                     lg(),
                     "Writing market series identity to database.");
}

void market_series_identity_repository::write(
    context ctx,
    const std::vector<domain::market_series_identity>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market series identities. Count: " << v.size();
    std::vector<domain::market_series_identity> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(market_series_identity_mapper::map(batch)),
                     lg(),
                     "Writing market series identities to database.");
}

std::vector<domain::market_series_identity>
market_series_identity_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("tenant_id"_c == tid) | order_by("series_id"_c);

    return execute_read_query<market_series_identity_entity, domain::market_series_identity>(
        ctx,
        query,
        [](const auto& entities) { return market_series_identity_mapper::map(entities); },
        lg(),
        "Reading latest market series identities");
}

std::vector<domain::market_series_identity>
market_series_identity_repository::read_latest(context ctx, const std::string& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market series identity. "
                               << "series_id: " << series_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("tenant_id"_c == tid && "series_id"_c == series_id);

    return execute_read_query<market_series_identity_entity, domain::market_series_identity>(
        ctx,
        query,
        [](const auto& entities) { return market_series_identity_mapper::map(entities); },
        lg(),
        "Reading latest market series identity by series_id.");
}


std::vector<domain::market_series_identity>
market_series_identity_repository::read_all(context ctx, const std::string& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all market series identity versions. "
                               << "series_id: " << series_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("tenant_id"_c == tid && "series_id"_c == series_id) |
                       order_by("series_id"_c);

    return execute_read_query<market_series_identity_entity, domain::market_series_identity>(
        ctx,
        query,
        [](const auto& entities) { return market_series_identity_mapper::map(entities); },
        lg(),
        "Reading all market series identity versions by series_id.");
}


market_series_identity_repository::remove_status market_series_identity_repository::remove(
    context ctx, const std::string& series_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market series identity. " << "series_id: " << series_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, series_id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_series_identity_entity> |
                       where("tenant_id"_c == tid && "series_id"_c == series_id);

    execute_delete_query(ctx, query, lg(), "Removing market series identity from database.");
    return remove_status::removed;
}

void market_series_identity_repository::remove(context ctx, const std::string& series_id) {
    static_cast<void>(remove(ctx, series_id, std::nullopt));
}

std::vector<domain::market_series_identity>
market_series_identity_repository::read_latest(context ctx,
                                               std::uint32_t offset,
                                               std::uint32_t limit,
                                               const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market series identities with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<market_series_identity_entity,
                                      domain::market_series_identity>(
        ctx,
        query,
        list_order(order, {"series_id"}, false),
        std::nullopt,
        [](const auto& entities) { return market_series_identity_mapper::map(entities); },
        lg(),
        "Reading latest market series identities with pagination.");
}

std::uint32_t
market_series_identity_repository::get_total_market_series_identity_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market series identity count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<market_series_identity_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<market_series_identity_entity>(
        ctx, query, std::nullopt, lg(), "Counting market series identities");
}

std::vector<domain::market_series_identity>
market_series_identity_repository::read_latest(context ctx,
                                               const std::vector<std::string>& series_ids) {
    if (series_ids.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_identity_entity>> |
                       where("tenant_id"_c == tid && "series_id"_c.in(series_ids));
    auto result = execute_read_query<market_series_identity_entity, domain::market_series_identity>(
        ctx,
        query,
        [](const auto& entities) { return market_series_identity_mapper::map(entities); },
        lg(),
        "Reading latest market series identities by ids.");
    return result;
}

void market_series_identity_repository::remove(context ctx,
                                               const std::vector<std::string>& series_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (series_ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_series_identity_entity> |
                       where("tenant_id"_c == tid && "series_id"_c.in(series_ids));
    execute_delete_query(ctx, query, lg(), "Batch removing market series identities.");
}


}
