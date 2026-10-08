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
#include "ores.trading.core/repository/structure_template_role_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/structure_template_role.hpp"
#include "ores.trading.api/domain/structure_template_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/structure_template_role_entity.hpp"
#include "ores.trading.core/repository/structure_template_role_mapper.hpp"
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

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string structure_template_role_repository::sql() {
    return generate_create_table_sql<structure_template_role_entity>(lg());
}

bool structure_template_role_repository::is_sortable(std::string_view field) {
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
            default_columns, default_descending != order.descending, {"template_code", "role"});
    if (!structure_template_role_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of structure template roles cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"template_code", "role"});
}

}

ores::utility::domain::precondition
structure_template_role_repository::replace_claim(context ctx,
                                                  const domain::structure_template_role& v) {
    const auto current = read_latest(ctx, v.template_code, v.role);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::structure_template_role
structure_template_role_repository::apply_claim(context ctx,
                                                const domain::structure_template_role& v,
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
            const auto current = read_latest(ctx, v.template_code, v.role);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void structure_template_role_repository::write(context ctx,
                                               const domain::structure_template_role& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void structure_template_role_repository::write(
    context ctx, const std::vector<domain::structure_template_role>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void structure_template_role_repository::write(context ctx,
                                               const domain::structure_template_role& v,
                                               const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing structure template role. "
                               << "template_code: " << v.template_code << " role: " << v.role;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        structure_template_role_mapper::map(t),
                        lg(),
                        "Writing structure template role to database.");
}

void structure_template_role_repository::write(
    context ctx,
    const std::vector<domain::structure_template_role>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing structure template roles. Count: " << v.size();
    std::vector<domain::structure_template_role> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        structure_template_role_mapper::map(batch),
                        lg(),
                        "Writing structure template roles to database.");
}

std::vector<domain::structure_template_role>
structure_template_role_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<structure_template_role_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("template_code"_c, "role"_c);

    return execute_read_query<structure_template_role_entity, domain::structure_template_role>(
        ctx,
        query,
        [](const auto& entities) { return structure_template_role_mapper::map(entities); },
        lg(),
        "Reading latest structure template roles");
}

std::vector<domain::structure_template_role> structure_template_role_repository::read_latest(
    context ctx, const std::string& template_code, const std::string& role) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest structure template role. "
                               << "template_code: " << template_code << " role: " << role;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<structure_template_role_entity>> |
                       where("tenant_id"_c == tid && "template_code"_c == template_code &&
                             "role"_c == role && "valid_to"_c == max.value());

    return execute_read_query<structure_template_role_entity, domain::structure_template_role>(
        ctx,
        query,
        [](const auto& entities) { return structure_template_role_mapper::map(entities); },
        lg(),
        "Reading latest structure template role by template_code.");
}


std::vector<domain::structure_template_role> structure_template_role_repository::read_all(
    context ctx, const std::string& template_code, const std::string& role) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all structure template role versions. "
                               << "template_code: " << template_code << " role: " << role;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<structure_template_role_entity>> |
        where("tenant_id"_c == tid && "template_code"_c == template_code && "role"_c == role) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<structure_template_role_entity, domain::structure_template_role>(
        ctx,
        query,
        [](const auto& entities) { return structure_template_role_mapper::map(entities); },
        lg(),
        "Reading all structure template role versions by template_code.");
}

std::optional<domain::structure_template_role> structure_template_role_repository::read_at_version(
    context ctx, const std::string& template_code, const std::string& role, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading structure template role at version. "
                               << "template_code: " << template_code << " role: " << role
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<structure_template_role_entity>> |
                       where("tenant_id"_c == tid && "template_code"_c == template_code &&
                             "role"_c == role && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<structure_template_role_entity, domain::structure_template_role>(
            ctx,
            query,
            [](const auto& entities) { return structure_template_role_mapper::map(entities); },
            lg(),
            "Reading structure template role at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


structure_template_role_repository::remove_status
structure_template_role_repository::remove(context ctx,
                                           const std::string& template_code,
                                           const std::string& role,
                                           std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing structure template role. "
                               << "template_code: " << template_code << " role: " << role;
    const auto current = read_latest(ctx, template_code, role);
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
    const auto query =
        sqlgen::delete_from<structure_template_role_entity> |
        where("tenant_id"_c == tid && "template_code"_c == template_code && "role"_c == role &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing structure template role from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, template_code, role).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void structure_template_role_repository::remove(context ctx,
                                                const std::string& template_code,
                                                const std::string& role) {
    static_cast<void>(remove(ctx, template_code, role, std::nullopt));
}

std::vector<domain::structure_template_role>
structure_template_role_repository::read_latest(context ctx,
                                                std::uint32_t offset,
                                                std::uint32_t limit,
                                                const ores::utility::domain::order& order,
                                                const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest structure template roles with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<structure_template_role_entity>> |
                       where("tenant_id"_c == tid) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<structure_template_role_entity,
                                      domain::structure_template_role>(
        ctx,
        query,
        list_order(order, {"template_code", "role"}, false),
        narrowed(valid_at(as_of), std::nullopt),
        [](const auto& entities) { return structure_template_role_mapper::map(entities); },
        lg(),
        "Reading latest structure template roles with pagination.");
}

std::uint32_t structure_template_role_repository::get_total_template_role_count(
    context ctx, const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active structure template role count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<structure_template_role_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<structure_template_role_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), std::nullopt),
        lg(),
        "Counting structure template roles");
}

std::vector<domain::structure_template_role>
structure_template_role_repository::read_latest(context ctx,
                                                const std::vector<std::string>& template_codes,
                                                const std::vector<std::string>& roles) {
    if (template_codes.empty() || roles.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<structure_template_role_entity>> |
                       where("tenant_id"_c == tid && "template_code"_c.in(template_codes) &&
                             "role"_c.in(roles) && "valid_to"_c == max.value());
    auto result =
        execute_read_query<structure_template_role_entity, domain::structure_template_role>(
            ctx,
            query,
            [](const auto& entities) { return structure_template_role_mapper::map(entities); },
            lg(),
            "Reading latest structure template roles by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (roles.size() != template_codes.size())
        throw std::invalid_argument("structure_template_role_repository::read_latest: key column "
                                    "vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < template_codes.size(); ++i)
        requested.emplace(template_codes[i], roles[i]);
    std::vector<domain::structure_template_role> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(item.template_code, item.role)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void structure_template_role_repository::remove(context ctx,
                                                const std::vector<std::string>& template_codes,
                                                const std::vector<std::string>& roles) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (roles.size() != template_codes.size())
        throw std::invalid_argument("structure_template_role_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < template_codes.size(); ++i)
        remove(ctx, template_codes[i], roles[i]);
}


}
