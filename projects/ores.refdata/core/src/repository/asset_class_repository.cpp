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
#include "ores.refdata.core/repository/asset_class_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/asset_class.hpp"
#include "ores.refdata.api/domain/asset_class_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/asset_class_entity.hpp"
#include "ores.refdata.core/repository/asset_class_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <set>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
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

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string asset_class_repository::sql() {
    return generate_create_table_sql<asset_class_entity>(lg());
}

bool asset_class_repository::is_sortable(std::string_view field) {
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
                          {"code", "coding_scheme_code"});
    if (!asset_class_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of asset classes cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"code", "coding_scheme_code"});
}

}

ores::utility::domain::precondition
asset_class_repository::replace_claim(context ctx, const domain::asset_class& v) {
    const auto current = read_latest(ctx, v.code, v.coding_scheme_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::asset_class asset_class_repository::apply_claim(
    context ctx, const domain::asset_class& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, v.code, v.coding_scheme_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void asset_class_repository::write(context ctx, const domain::asset_class& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void asset_class_repository::write(context ctx, const std::vector<domain::asset_class>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void asset_class_repository::write(context ctx,
                                   const domain::asset_class& v,
                                   const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset class. " << "code: " << v.code
                               << " coding_scheme_code: " << v.coding_scheme_code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, asset_class_mapper::map(t), lg(), "Writing asset class to database.");
}

void asset_class_repository::write(context ctx,
                                   const std::vector<domain::asset_class>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing asset classes. Count: " << v.size();
    std::vector<domain::asset_class> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, asset_class_mapper::map(batch), lg(), "Writing asset classes to database.");
}

std::vector<domain::asset_class> asset_class_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<asset_class_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c, "coding_scheme_code"_c);

    return execute_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset classes");
}

std::vector<domain::asset_class> asset_class_repository::read_latest(
    context ctx, const std::string& code, const std::string& coding_scheme_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset class. " << "code: " << code
                               << " coding_scheme_code: " << coding_scheme_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<asset_class_entity>> |
        where("tenant_id"_c == tid && "code"_c == code &&
              "coding_scheme_code"_c == coding_scheme_code && "valid_to"_c == max.value());

    return execute_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset class by code.");
}


std::vector<domain::asset_class> asset_class_repository::read_all(
    context ctx, const std::string& code, const std::string& coding_scheme_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all asset class versions. " << "code: " << code
                               << " coding_scheme_code: " << coding_scheme_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<asset_class_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code &&
                             "coding_scheme_code"_c == coding_scheme_code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading all asset class versions by code.");
}

std::optional<domain::asset_class>
asset_class_repository::read_at_version(context ctx,
                                        const std::string& code,
                                        const std::string& coding_scheme_code,
                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading asset class at version. " << "code: " << code
                               << " coding_scheme_code: " << coding_scheme_code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<asset_class_entity>> |
        where("tenant_id"_c == tid && "code"_c == code &&
              "coding_scheme_code"_c == coding_scheme_code && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading asset class at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

asset_class_repository::remove_status
asset_class_repository::remove(context ctx,
                               const std::string& code,
                               const std::string& coding_scheme_code,
                               std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing asset class. " << "code: " << code
                               << " coding_scheme_code: " << coding_scheme_code;
    const auto current = read_latest(ctx, code, coding_scheme_code);
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
    const auto query = sqlgen::delete_from<asset_class_entity> |
                       where("tenant_id"_c == tid && "code"_c == code &&
                             "coding_scheme_code"_c == coding_scheme_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing asset class from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, code, coding_scheme_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void asset_class_repository::remove(context ctx,
                                    const std::string& code,
                                    const std::string& coding_scheme_code) {
    static_cast<void>(remove(ctx, code, coding_scheme_code, std::nullopt));
}

std::vector<domain::asset_class>
asset_class_repository::read_latest(context ctx,
                                    std::uint32_t offset,
                                    std::uint32_t limit,
                                    const ores::utility::domain::order& order,
                                    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest asset classes with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<asset_class_entity>> | where("tenant_id"_c == tid) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        list_order(order, {"code", "coding_scheme_code"}, false),
        narrowed(valid_at(as_of), std::nullopt),
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset classes with pagination.");
}

std::uint32_t
asset_class_repository::get_total_class_count(context ctx,
                                              const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active asset class count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<asset_class_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<asset_class_entity>(
        ctx, query, narrowed(valid_at(as_of), std::nullopt), lg(), "Counting asset classes");
}

std::vector<domain::asset_class>
asset_class_repository::read_latest(context ctx,
                                    const std::vector<std::string>& codes,
                                    const std::vector<std::string>& coding_scheme_codes) {
    if (codes.empty() || coding_scheme_codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<asset_class_entity>> |
        where("tenant_id"_c == tid && "code"_c.in(codes) &&
              "coding_scheme_code"_c.in(coding_scheme_codes) && "valid_to"_c == max.value());
    auto result = execute_read_query<asset_class_entity, domain::asset_class>(
        ctx,
        query,
        [](const auto& entities) { return asset_class_mapper::map(entities); },
        lg(),
        "Reading latest asset classes by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (coding_scheme_codes.size() != codes.size())
        throw std::invalid_argument(
            "asset_class_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < codes.size(); ++i)
        requested.emplace(codes[i], coding_scheme_codes[i]);
    std::vector<domain::asset_class> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(item.code, item.coding_scheme_code)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void asset_class_repository::remove(context ctx,
                                    const std::vector<std::string>& codes,
                                    const std::vector<std::string>& coding_scheme_codes) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (coding_scheme_codes.size() != codes.size())
        throw std::invalid_argument(
            "asset_class_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < codes.size(); ++i)
        remove(ctx, codes[i], coding_scheme_codes[i]);
}


}
