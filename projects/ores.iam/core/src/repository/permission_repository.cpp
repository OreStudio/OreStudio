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
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.database/repository/valid_at.hpp"
#include "ores.iam.api/domain/permission_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/permission_entity.hpp"
#include "ores.iam.core/repository/permission_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string permission_repository::sql() {
    return generate_create_table_sql<permission_entity>(lg());
}

bool permission_repository::is_sortable(std::string_view field) {
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
    if (!permission_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of permissions cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::permissions_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->id_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
permission_repository::replace_claim(context ctx, const domain::permission& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::permission permission_repository::apply_claim(
    context ctx, const domain::permission& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "permission_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument("permission_repository::write: a current row already exists");
    return t;
}

void permission_repository::write(context ctx, const domain::permission& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void permission_repository::write(context ctx, const std::vector<domain::permission>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void permission_repository::write(context ctx,
                                  const domain::permission& v,
                                  const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permission. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, permission_mapper::map(t), lg(), "Writing permission to database.");
}

void permission_repository::write(context ctx,
                                  const std::vector<domain::permission>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permissions. Count: " << v.size();
    std::vector<domain::permission> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, permission_mapper::map(batch), lg(), "Writing permissions to database.");
}

std::vector<domain::permission> permission_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions");
}

std::vector<domain::permission> permission_repository::read_latest(context ctx,
                                                                   const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permission by id.");
}

std::vector<domain::permission>
permission_repository::read_latest_by_code(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission by code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<permission_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permission by code.");
}

std::vector<domain::permission> permission_repository::read_any_by_code(context ctx,
                                                                        const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any permission by code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading any permission by code.");
}


std::vector<domain::permission> permission_repository::read_all(context ctx,
                                                                const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all permission versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading all permission versions by id.");
}


permission_repository::remove_status permission_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permission. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<permission_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing permission from database.");
    return remove_status::removed;
}

void permission_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::permission>
permission_repository::read_latest(context ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order,
                                   const std::optional<messaging::permissions_filter>& filter,
                                   const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permissions with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> | where("tenant_id"_c == tid) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        narrowed(valid_at(as_of), filter_condition(filter)),
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions with pagination.");
}

std::uint32_t permission_repository::get_total_permission_count(
    context ctx,
    const std::optional<messaging::permissions_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active permission count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> | where("tenant_id"_c == tid);

    return execute_count_query<permission_entity>(
        ctx,
        query,
        narrowed(valid_at(as_of), filter_condition(filter)),
        lg(),
        "Counting permissions");
}

std::vector<domain::permission>
permission_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions by ids.");
    return result;
}

void permission_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<permission_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing permissions.");
}


}
