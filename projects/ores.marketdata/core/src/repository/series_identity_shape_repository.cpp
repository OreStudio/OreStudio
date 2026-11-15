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
#include "ores.marketdata.core/repository/series_identity_shape_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/series_identity_shape.hpp"
#include "ores.marketdata.api/domain/series_identity_shape_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_identity_shape_entity.hpp"
#include "ores.marketdata.core/repository/series_identity_shape_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::marketdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string series_identity_shape_repository::sql() {
    return query_builder::insert_statement_sql<series_identity_shape_entity>(lg());
}

bool series_identity_shape_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order
series_identity_shape_list_order(const ores::utility::domain::order& order,
                                 std::initializer_list<std::string> default_columns,
                                 bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns,
            default_descending != order.descending,
            {"asset_class", "series_subclass", "index_family", "field"});
    if (!series_identity_shape_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of series identity shapes cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field},
                                     order.descending,
                                     {"asset_class", "series_subclass", "index_family", "field"});
}

}

ores::utility::domain::precondition
series_identity_shape_repository::replace_claim(const context& ctx,
                                                const domain::series_identity_shape& v) {
    const auto current =
        read_latest(ctx, v.asset_class, v.series_subclass, v.index_family, v.field);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::series_identity_shape
series_identity_shape_repository::apply_claim(const context& ctx,
                                              const domain::series_identity_shape& v,
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
            const auto current =
                read_latest(ctx, v.asset_class, v.series_subclass, v.index_family, v.field);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void series_identity_shape_repository::write(const context& ctx,
                                             const domain::series_identity_shape& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void series_identity_shape_repository::write(const context& ctx,
                                             const std::vector<domain::series_identity_shape>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void series_identity_shape_repository::write(const context& ctx,
                                             const domain::series_identity_shape& v,
                                             const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series identity shape. "
                               << "asset_class: " << v.asset_class
                               << " series_subclass: " << v.series_subclass
                               << " index_family: " << v.index_family << " field: " << v.field;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       series_identity_shape_mapper::map(t),
                                       lg(),
                                       "Writing series identity shape to database.");
}

void series_identity_shape_repository::write(
    const context& ctx,
    const std::vector<domain::series_identity_shape>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series identity shapes. Count: " << v.size();
    std::vector<domain::series_identity_shape> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       series_identity_shape_mapper::map(batch),
                                       lg(),
                                       "Writing series identity shapes to database.");
}

std::vector<domain::series_identity_shape>
series_identity_shape_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_identity_shape_entity>> |
        query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
        query_builder::order_by("asset_class"_c, "series_subclass"_c, "index_family"_c, "field"_c);

    return query_builder::execute_read_query<series_identity_shape_entity,
                                             domain::series_identity_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading latest series identity shapes");
}

std::vector<domain::series_identity_shape>
series_identity_shape_repository::read_latest(const context& ctx,
                                              const std::string& asset_class,
                                              const std::string& series_subclass,
                                              const std::string& index_family,
                                              const std::string& field) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series identity shape. "
                               << "asset_class: " << asset_class
                               << " series_subclass: " << series_subclass
                               << " index_family: " << index_family << " field: " << field;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_identity_shape_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class"_c == asset_class &&
                             "series_subclass"_c == series_subclass &&
                             "index_family"_c == index_family && "field"_c == field &&
                             "valid_to"_c == max);

    return query_builder::execute_read_query<series_identity_shape_entity,
                                             domain::series_identity_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading latest series identity shape by asset_class.");
}


std::vector<domain::series_identity_shape>
series_identity_shape_repository::read_all(const context& ctx,
                                           const std::string& asset_class,
                                           const std::string& series_subclass,
                                           const std::string& index_family,
                                           const std::string& field) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all series identity shape versions. "
                               << "asset_class: " << asset_class
                               << " series_subclass: " << series_subclass
                               << " index_family: " << index_family << " field: " << field;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_identity_shape_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class"_c == asset_class &&
                             "series_subclass"_c == series_subclass &&
                             "index_family"_c == index_family && "field"_c == field) |
        query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<series_identity_shape_entity,
                                             domain::series_identity_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading all series identity shape versions by asset_class.");
}

std::optional<domain::series_identity_shape>
series_identity_shape_repository::read_at_version(const context& ctx,
                                                  const std::string& asset_class,
                                                  const std::string& series_subclass,
                                                  const std::string& index_family,
                                                  const std::string& field,
                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading series identity shape at version. "
                               << "asset_class: " << asset_class
                               << " series_subclass: " << series_subclass
                               << " index_family: " << index_family << " field: " << field
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_identity_shape_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class"_c == asset_class &&
                             "series_subclass"_c == series_subclass &&
                             "index_family"_c == index_family && "field"_c == field &&
                             "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<series_identity_shape_entity,
                                                            domain::series_identity_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading series identity shape at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

series_identity_shape_repository::remove_status
series_identity_shape_repository::remove(const context& ctx,
                                         const std::string& asset_class,
                                         const std::string& series_subclass,
                                         const std::string& index_family,
                                         const std::string& field,
                                         std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series identity shape. "
                               << "asset_class: " << asset_class
                               << " series_subclass: " << series_subclass
                               << " index_family: " << index_family << " field: " << field;
    const auto current = read_latest(ctx, asset_class, series_subclass, index_family, field);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<series_identity_shape_entity> |
        query_builder::where("tenant_id"_c == tid && "asset_class"_c == asset_class &&
                             "series_subclass"_c == series_subclass &&
                             "index_family"_c == index_family && "field"_c == field &&
                             "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing series identity shape from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, asset_class, series_subclass, index_family, field).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void series_identity_shape_repository::remove(const context& ctx,
                                              const std::string& asset_class,
                                              const std::string& series_subclass,
                                              const std::string& index_family,
                                              const std::string& field) {
    static_cast<void>(remove(ctx, asset_class, series_subclass, index_family, field, std::nullopt));
}

std::vector<domain::series_identity_shape>
series_identity_shape_repository::read_latest(const context& ctx,
                                              std::uint32_t offset,
                                              std::uint32_t limit,
                                              const ores::utility::domain::order& order,
                                              const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series identity shapes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_identity_shape_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<series_identity_shape_entity,
                                                     domain::series_identity_shape>(
        ctx,
        query,
        series_identity_shape_list_order(
            order, {"asset_class", "series_subclass", "index_family", "field"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading latest series identity shapes with pagination.");
}

std::uint32_t
series_identity_shape_repository::get_total_shape_count(const context& ctx,
                                                        const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active series identity shape count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<series_identity_shape_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<series_identity_shape_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of), std::nullopt),
        lg(),
        "Counting series identity shapes");
}

std::vector<domain::series_identity_shape>
series_identity_shape_repository::read_latest(const context& ctx,
                                              const std::vector<std::string>& asset_classs,
                                              const std::vector<std::string>& series_subclasss,
                                              const std::vector<std::string>& index_familys,
                                              const std::vector<std::string>& fields) {
    if (asset_classs.empty() || series_subclasss.empty() || index_familys.empty() || fields.empty())
        return {};
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<series_identity_shape_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class"_c.in(asset_classs) &&
                             "series_subclass"_c.in(series_subclasss) &&
                             "index_family"_c.in(index_familys) && "field"_c.in(fields) &&
                             "valid_to"_c == max);
    auto result = query_builder::execute_read_query<series_identity_shape_entity,
                                                    domain::series_identity_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_identity_shape_mapper::map(entities); },
        lg(),
        "Reading latest series identity shapes by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    if (series_subclasss.size() != asset_classs.size() ||
        index_familys.size() != asset_classs.size() || fields.size() != asset_classs.size())
        throw std::invalid_argument("series_identity_shape_repository::read_latest: key column "
                                    "vectors must be the same length");
    std::set<std::tuple<std::string, std::string, std::string, std::string>> requested;
    for (std::size_t i = 0; i < asset_classs.size(); ++i)
        requested.emplace(asset_classs[i], series_subclasss[i], index_familys[i], fields[i]);
    std::vector<domain::series_identity_shape> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                item.asset_class, item.series_subclass, item.index_family, item.field)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void series_identity_shape_repository::remove(const context& ctx,
                                              const std::vector<std::string>& asset_classs,
                                              const std::vector<std::string>& series_subclasss,
                                              const std::vector<std::string>& index_familys,
                                              const std::vector<std::string>& fields) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (series_subclasss.size() != asset_classs.size() ||
        index_familys.size() != asset_classs.size() || fields.size() != asset_classs.size())
        throw std::invalid_argument(
            "series_identity_shape_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < asset_classs.size(); ++i)
        remove(ctx, asset_classs[i], series_subclasss[i], index_familys[i], fields[i]);
}


}
