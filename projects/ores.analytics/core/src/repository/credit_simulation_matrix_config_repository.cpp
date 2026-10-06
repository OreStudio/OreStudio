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
#include "ores.analytics.core/repository/credit_simulation_matrix_config_repository.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_matrix_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/messaging/credit_simulation_matrix_config_protocol.hpp"
#include "ores.analytics.core/repository/credit_simulation_matrix_config_entity.hpp"
#include "ores.analytics.core/repository/credit_simulation_matrix_config_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/dynamic/Condition.hpp>
#include <sqlgen/dynamic/OrderBy.hpp>
#include <sqlgen/dynamic/Value.hpp>
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

namespace ores::analytics::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string credit_simulation_matrix_config_repository::sql() {
    return generate_create_table_sql<credit_simulation_matrix_config_entity>(lg());
}

bool credit_simulation_matrix_config_repository::is_sortable(std::string_view field) {
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
    if (!credit_simulation_matrix_config_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of transition matrices cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::credit_simulation_matrix_configs_filter>& filter) {
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

ores::utility::domain::precondition credit_simulation_matrix_config_repository::replace_claim(
    context ctx, const domain::credit_simulation_matrix_config& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::credit_simulation_matrix_config credit_simulation_matrix_config_repository::apply_claim(
    context ctx,
    const domain::credit_simulation_matrix_config& v,
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
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void credit_simulation_matrix_config_repository::write(
    context ctx, const domain::credit_simulation_matrix_config& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void credit_simulation_matrix_config_repository::write(
    context ctx, const std::vector<domain::credit_simulation_matrix_config>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void credit_simulation_matrix_config_repository::write(
    context ctx,
    const domain::credit_simulation_matrix_config& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing transition matrix. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        credit_simulation_matrix_config_mapper::map(t),
                        lg(),
                        "Writing transition matrix to database.");
}

void credit_simulation_matrix_config_repository::write(
    context ctx,
    const std::vector<domain::credit_simulation_matrix_config>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing transition matrices. Count: " << v.size();
    std::vector<domain::credit_simulation_matrix_config> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        credit_simulation_matrix_config_mapper::map(batch),
                        lg(),
                        "Writing transition matrices to database.");
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<credit_simulation_matrix_config_entity,
                              domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading latest transition matrices");
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest transition matrix. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<credit_simulation_matrix_config_entity,
                              domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading latest transition matrix by id.");
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_latest_by_name(context ctx,
                                                                const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest transition matrix by name: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
        where("tenant_id"_c == tid && "name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<credit_simulation_matrix_config_entity,
                              domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading latest transition matrix by name.");
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_any_by_name(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any transition matrix by name: " << name;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "name"_c == name) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<credit_simulation_matrix_config_entity,
                              domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading any transition matrix by name.");
}


std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all transition matrix versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<credit_simulation_matrix_config_entity,
                              domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading all transition matrix versions by id.");
}

std::optional<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_at_version(context ctx,
                                                            const std::string& id,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading transition matrix at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<credit_simulation_matrix_config_entity,
                                             domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading transition matrix at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

credit_simulation_matrix_config_repository::remove_status
credit_simulation_matrix_config_repository::remove(context ctx,
                                                   const std::string& id,
                                                   std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing transition matrix. " << "id: " << id;
    const auto current = read_latest(ctx, id);
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
    const auto query = sqlgen::delete_from<credit_simulation_matrix_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing transition matrix from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void credit_simulation_matrix_config_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::credit_simulation_matrix_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest transition matrices with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<credit_simulation_matrix_config_entity,
                                      domain::credit_simulation_matrix_config>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading latest transition matrices with pagination.");
}

std::uint32_t credit_simulation_matrix_config_repository::get_total_matrix_count(
    context ctx, const std::optional<messaging::credit_simulation_matrix_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active transition matrix count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value());

    return execute_count_query<credit_simulation_matrix_config_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting transition matrices");
}

std::vector<domain::credit_simulation_matrix_config>
credit_simulation_matrix_config_repository::read_latest(context ctx,
                                                        const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_simulation_matrix_config_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<credit_simulation_matrix_config_entity,
                                     domain::credit_simulation_matrix_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_matrix_config_mapper::map(entities); },
        lg(),
        "Reading latest transition matrices by ids.");
    return result;
}

void credit_simulation_matrix_config_repository::remove(context ctx,
                                                        const std::vector<std::string>& ids) {
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
    const auto query = sqlgen::delete_from<credit_simulation_matrix_config_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing transition matrices.");
}


}
