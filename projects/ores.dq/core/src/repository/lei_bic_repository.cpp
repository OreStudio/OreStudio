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
#include "ores.dq.core/repository/lei_bic_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/lei_bic.hpp"
#include "ores.dq.api/domain/lei_bic_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/lei_bic_protocol.hpp"
#include "ores.dq.core/repository/lei_bic_entity.hpp"
#include "ores.dq.core/repository/lei_bic_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/begin_transaction.hpp>
#include <sqlgen/commit.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/Condition.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/dynamic/Value.hpp>
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
#include <utility>
#include <vector>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lei_bic_repository::sql() {
    return generate_create_table_sql<lei_bic_entity>(lg());
}

bool lei_bic_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy lei_bic_list_order(const ores::utility::domain::order& order,
                                            std::initializer_list<std::string> default_columns,
                                            bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns, default_descending != order.descending, {"lei"});
    if (!lei_bic_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of LEI BIC cannot be ordered by " + order.field + ".");
    return make_order({order.field}, order.descending, {"lei"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
lei_bic_filter_condition(const std::optional<messaging::lei_bic_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->lei_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->lei_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("lei", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition lei_bic_repository::replace_claim(context ctx,
                                                                      const domain::lei_bic& v) {
    const auto current = read_latest(ctx, v.lei);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::lei_bic lei_bic_repository::apply_claim(context ctx,
                                                const domain::lei_bic& v,
                                                const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "lei_bic_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !read_latest(ctx, v.lei).empty())
        throw std::invalid_argument("lei_bic_repository::write: a current row already exists");
    return t;
}

void lei_bic_repository::write(context ctx, const domain::lei_bic& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void lei_bic_repository::write(context ctx, const std::vector<domain::lei_bic>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void lei_bic_repository::write(context ctx,
                               const domain::lei_bic& v,
                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI BIC. " << "lei: " << v.lei;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(lei_bic_mapper::map(t)),
                     lg(),
                     "Writing LEI BIC to database.");
}

void lei_bic_repository::write(context ctx,
                               const std::vector<domain::lei_bic>& v,
                               const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI BIC. Count: " << v.size();
    std::vector<domain::lei_bic> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(lei_bic_mapper::map(batch)),
                     lg(),
                     "Writing LEI BIC to database.");
}

std::vector<domain::lei_bic> lei_bic_repository::read_latest(context ctx) {
    const auto query = sqlgen::read<std::vector<lei_bic_entity>> | order_by("lei"_c);

    return execute_read_query<lei_bic_entity, domain::lei_bic>(
        ctx,
        query,
        [](const auto& entities) { return lei_bic_mapper::map(entities); },
        lg(),
        "Reading latest LEI BIC");
}

std::vector<domain::lei_bic> lei_bic_repository::read_latest(context ctx, const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI BIC. " << "lei: " << lei;
    const auto query = sqlgen::read<std::vector<lei_bic_entity>> | where("lei"_c == lei);

    return execute_read_query<lei_bic_entity, domain::lei_bic>(
        ctx,
        query,
        [](const auto& entities) { return lei_bic_mapper::map(entities); },
        lg(),
        "Reading latest LEI BIC by lei.");
}


std::vector<domain::lei_bic> lei_bic_repository::read_all(context ctx, const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all LEI BIC versions. " << "lei: " << lei;
    const auto query =
        sqlgen::read<std::vector<lei_bic_entity>> | where("lei"_c == lei) | order_by("lei"_c);

    return execute_read_query<lei_bic_entity, domain::lei_bic>(
        ctx,
        query,
        [](const auto& entities) { return lei_bic_mapper::map(entities); },
        lg(),
        "Reading all LEI BIC versions by lei.");
}


lei_bic_repository::remove_status lei_bic_repository::remove(context ctx,
                                                             const std::string& lei,
                                                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI BIC. " << "lei: " << lei;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, lei);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lei_bic_entity> | where("tenant_id"_c == tid && "lei"_c == lei);

    execute_delete_query(ctx, query, lg(), "Removing LEI BIC from database.");
    return remove_status::removed;
}

void lei_bic_repository::remove(context ctx, const std::string& lei) {
    static_cast<void>(remove(ctx, lei, std::nullopt));
}

std::vector<domain::lei_bic>
lei_bic_repository::read_latest(context ctx,
                                std::uint32_t offset,
                                std::uint32_t limit,
                                const ores::utility::domain::order& order,
                                const std::optional<messaging::lei_bic_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI BIC with offset: " << offset
                               << " and limit: " << limit;
    const auto query =
        sqlgen::read<std::vector<lei_bic_entity>> | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<lei_bic_entity, domain::lei_bic>(
        ctx,
        query,
        lei_bic_list_order(order, {"lei"}, false),
        lei_bic_filter_condition(filter),
        [](const auto& entities) { return lei_bic_mapper::map(entities); },
        lg(),
        "Reading latest LEI BIC with pagination.");
}

std::uint32_t lei_bic_repository::get_total_lei_bic_count(
    context ctx, const std::optional<messaging::lei_bic_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active LEI BIC count";

    const auto query = sqlgen::read<std::vector<lei_bic_entity>>;

    return execute_count_query<lei_bic_entity>(
        ctx, query, lei_bic_filter_condition(filter), lg(), "Counting LEI BIC");
}

std::vector<domain::lei_bic> lei_bic_repository::read_latest(context ctx,
                                                             const std::vector<std::string>& leis) {
    if (leis.empty())
        return {};
    const auto query = sqlgen::read<std::vector<lei_bic_entity>> | where("lei"_c.in(leis));
    auto result = execute_read_query<lei_bic_entity, domain::lei_bic>(
        ctx,
        query,
        [](const auto& entities) { return lei_bic_mapper::map(entities); },
        lg(),
        "Reading latest LEI BIC by ids.");
    return result;
}

void lei_bic_repository::remove(context ctx, const std::vector<std::string>& leis) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (leis.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lei_bic_entity> | where("tenant_id"_c == tid && "lei"_c.in(leis));
    execute_delete_query(ctx, query, lg(), "Batch removing LEI BIC.");
}


}
