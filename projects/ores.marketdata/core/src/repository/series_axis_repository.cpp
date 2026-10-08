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
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/series_axis.hpp"
#include "ores.marketdata.api/domain/series_axis_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_axis_entity.hpp"
#include "ores.marketdata.core/repository/series_axis_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
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
#include <tuple>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string series_axis_repository::sql() {
    return generate_create_table_sql<series_axis_entity>(lg());
}

bool series_axis_repository::is_sortable(std::string_view field) {
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
        return make_order(
            default_columns, default_descending != order.descending, {"series_id", "axis_field"});
    if (!series_axis_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of series axes cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"series_id", "axis_field"});
}

}

ores::utility::domain::precondition
series_axis_repository::replace_claim(context ctx, const domain::series_axis& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.series_id), v.axis_field);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::series_axis series_axis_repository::apply_claim(
    context ctx, const domain::series_axis& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "series_axis_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.series_id), v.axis_field).empty())
        throw std::invalid_argument("series_axis_repository::write: a current row already exists");
    return t;
}

void series_axis_repository::write(context ctx, const domain::series_axis& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void series_axis_repository::write(context ctx, const std::vector<domain::series_axis>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void series_axis_repository::write(context ctx,
                                   const domain::series_axis& v,
                                   const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series axis. " << "series_id: " << v.series_id
                               << " axis_field: " << v.axis_field;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(series_axis_mapper::map(t)),
                     lg(),
                     "Writing series axis to database.");
}

void series_axis_repository::write(context ctx,
                                   const std::vector<domain::series_axis>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series axes. Count: " << v.size();
    std::vector<domain::series_axis> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(series_axis_mapper::map(batch)),
                     lg(),
                     "Writing series axes to database.");
}

std::vector<domain::series_axis> series_axis_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_axis_entity>> | where("tenant_id"_c == tid) |
                       order_by("series_id"_c, "axis_field"_c);

    return execute_read_query<series_axis_entity, domain::series_axis>(
        ctx,
        query,
        [](const auto& entities) { return series_axis_mapper::map(entities); },
        lg(),
        "Reading latest series axes");
}

std::vector<domain::series_axis> series_axis_repository::read_latest(
    context ctx, const std::string& series_id, const std::string& axis_field) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series axis. " << "series_id: " << series_id
                               << " axis_field: " << axis_field;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<series_axis_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "axis_field"_c == axis_field);

    return execute_read_query<series_axis_entity, domain::series_axis>(
        ctx,
        query,
        [](const auto& entities) { return series_axis_mapper::map(entities); },
        lg(),
        "Reading latest series axis by series_id.");
}


std::vector<domain::series_axis> series_axis_repository::read_all(context ctx,
                                                                  const std::string& series_id,
                                                                  const std::string& axis_field) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all series axis versions. " << "series_id: " << series_id
                               << " axis_field: " << axis_field;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<series_axis_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "axis_field"_c == axis_field) |
        order_by("series_id"_c, "axis_field"_c);

    return execute_read_query<series_axis_entity, domain::series_axis>(
        ctx,
        query,
        [](const auto& entities) { return series_axis_mapper::map(entities); },
        lg(),
        "Reading all series axis versions by series_id.");
}


series_axis_repository::remove_status
series_axis_repository::remove(context ctx,
                               const std::string& series_id,
                               const std::string& axis_field,
                               std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series axis. " << "series_id: " << series_id
                               << " axis_field: " << axis_field;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, series_id, axis_field);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<series_axis_entity> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "axis_field"_c == axis_field);

    execute_delete_query(ctx, query, lg(), "Removing series axis from database.");
    return remove_status::removed;
}

void series_axis_repository::remove(context ctx,
                                    const std::string& series_id,
                                    const std::string& axis_field) {
    static_cast<void>(remove(ctx, series_id, axis_field, std::nullopt));
}

std::vector<domain::series_axis>
series_axis_repository::read_latest(context ctx,
                                    std::uint32_t offset,
                                    std::uint32_t limit,
                                    const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series axes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_axis_entity>> | where("tenant_id"_c == tid) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<series_axis_entity, domain::series_axis>(
        ctx,
        query,
        list_order(order, {"series_id", "axis_field"}, false),
        std::nullopt,
        [](const auto& entities) { return series_axis_mapper::map(entities); },
        lg(),
        "Reading latest series axes with pagination.");
}

std::uint32_t series_axis_repository::get_total_series_axis_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active series axis count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_axis_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<series_axis_entity>(
        ctx, query, std::nullopt, lg(), "Counting series axes");
}

std::vector<domain::series_axis>
series_axis_repository::read_latest(context ctx,
                                    const std::vector<std::string>& series_ids,
                                    const std::vector<std::string>& axis_fields) {
    if (series_ids.empty() || axis_fields.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_axis_entity>> |
                       where("tenant_id"_c == tid && "series_id"_c.in(series_ids) &&
                             "axis_field"_c.in(axis_fields));
    auto result = execute_read_query<series_axis_entity, domain::series_axis>(
        ctx,
        query,
        [](const auto& entities) { return series_axis_mapper::map(entities); },
        lg(),
        "Reading latest series axes by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (axis_fields.size() != series_ids.size())
        throw std::invalid_argument(
            "series_axis_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < series_ids.size(); ++i)
        requested.emplace(series_ids[i], axis_fields[i]);
    std::vector<domain::series_axis> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.series_id), item.axis_field)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void series_axis_repository::remove(context ctx,
                                    const std::vector<std::string>& series_ids,
                                    const std::vector<std::string>& axis_fields) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (axis_fields.size() != series_ids.size())
        throw std::invalid_argument(
            "series_axis_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < series_ids.size(); ++i)
        remove(ctx, series_ids[i], axis_fields[i]);
}


}
