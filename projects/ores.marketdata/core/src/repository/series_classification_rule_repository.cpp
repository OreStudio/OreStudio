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
#include "ores.marketdata.core/repository/series_classification_rule_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/series_classification_rule_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_classification_rule_entity.hpp"
#include "ores.marketdata.core/repository/series_classification_rule_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string series_classification_rule_repository::sql() {
    return generate_create_table_sql<series_classification_rule_entity>(lg());
}

void series_classification_rule_repository::write(context ctx,
                                                  const domain::series_classification_rule& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series classification rule. "
                               << "series_type: " << v.series_type << " metric: " << v.metric;
    execute_write_query(ctx,
                        series_classification_rule_mapper::map(v),
                        lg(),
                        "Writing series classification rule to database.");
}

void series_classification_rule_repository::write(
    context ctx, const std::vector<domain::series_classification_rule>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series classification rules. Count: " << v.size();
    execute_write_query(ctx,
                        series_classification_rule_mapper::map(v),
                        lg(),
                        "Writing series classification rules to database.");
}

std::vector<domain::series_classification_rule>
series_classification_rule_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_classification_rule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("series_type"_c, "metric"_c);

    return execute_read_query<series_classification_rule_entity,
                              domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rules");
}

std::vector<domain::series_classification_rule> series_classification_rule_repository::read_latest(
    context ctx, const std::string& series_type, const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series classification rule. "
                               << "series_type: " << series_type << " metric: " << metric;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_classification_rule_entity>> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type &&
                             "metric"_c == metric && "valid_to"_c == max.value());

    return execute_read_query<series_classification_rule_entity,
                              domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rule by series_type.");
}


std::vector<domain::series_classification_rule> series_classification_rule_repository::read_all(
    context ctx, const std::string& series_type, const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all series classification rule versions. "
                               << "series_type: " << series_type << " metric: " << metric;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<series_classification_rule_entity>> |
        where("tenant_id"_c == tid && "series_type"_c == series_type && "metric"_c == metric) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<series_classification_rule_entity,
                              domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading all series classification rule versions by series_type.");
}

std::optional<domain::series_classification_rule>
series_classification_rule_repository::read_at_version(context ctx,
                                                       const std::string& series_type,
                                                       const std::string& metric,
                                                       std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading series classification rule at version. "
                               << "series_type: " << series_type << " metric: " << metric
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_classification_rule_entity>> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type &&
                             "metric"_c == metric && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<series_classification_rule_entity, domain::series_classification_rule>(
            ctx,
            query,
            [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
            lg(),
            "Reading series classification rule at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void series_classification_rule_repository::remove(context ctx,
                                                   const std::string& series_type,
                                                   const std::string& metric) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series classification rule. "
                               << "series_type: " << series_type << " metric: " << metric;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<series_classification_rule_entity> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type &&
                             "metric"_c == metric && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing series classification rule from database.");
}

std::vector<domain::series_classification_rule> series_classification_rule_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series classification rules with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_classification_rule_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("series_type"_c, "metric"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<series_classification_rule_entity,
                              domain::series_classification_rule>(
        ctx,
        query,
        [](const auto& entities) { return series_classification_rule_mapper::map(entities); },
        lg(),
        "Reading latest series classification rules with pagination.");
}

std::uint32_t series_classification_rule_repository::get_total_rule_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active series classification rule count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<series_classification_rule_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active series classification rule count: " << count;
    return count;
}

void series_classification_rule_repository::remove(context ctx,
                                                   const std::vector<std::string>& series_types,
                                                   const std::vector<std::string>& metrics) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (metrics.size() != series_types.size())
        throw std::invalid_argument("series_classification_rule_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < series_types.size(); ++i)
        remove(ctx, series_types[i], metrics[i]);
}


}
