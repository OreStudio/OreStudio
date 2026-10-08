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
#include "ores.dq.core/repository/risk_report_config_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/list_filter.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/risk_report_config.hpp"
#include "ores.dq.api/domain/risk_report_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/risk_report_config_protocol.hpp"
#include "ores.dq.core/repository/risk_report_config_entity.hpp"
#include "ores.dq.core/repository/risk_report_config_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
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

std::string risk_report_config_repository::sql() {
    return generate_create_table_sql<risk_report_config_entity>(lg());
}

bool risk_report_config_repository::is_sortable(std::string_view field) {
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
    if (!risk_report_config_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of risk report configs cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::risk_report_configs_filter>& filter) {
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
risk_report_config_repository::replace_claim(context ctx, const domain::risk_report_config& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::risk_report_config
risk_report_config_repository::apply_claim(context ctx,
                                           const domain::risk_report_config& v,
                                           const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "risk_report_config_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "risk_report_config_repository::write: a current row already exists");
    return t;
}

void risk_report_config_repository::write(context ctx, const domain::risk_report_config& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void risk_report_config_repository::write(context ctx,
                                          const std::vector<domain::risk_report_config>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void risk_report_config_repository::write(context ctx,
                                          const domain::risk_report_config& v,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report config. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(risk_report_config_mapper::map(t)),
                     lg(),
                     "Writing risk report config to database.");
}

void risk_report_config_repository::write(
    context ctx,
    const std::vector<domain::risk_report_config>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing risk report configs. Count: " << v.size();
    std::vector<domain::risk_report_config> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_op(ctx,
                     sqlgen::insert_or_replace(risk_report_config_mapper::map(batch)),
                     lg(),
                     "Writing risk report configs to database.");
}

std::vector<domain::risk_report_config> risk_report_config_repository::read_latest(context ctx) {
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> | order_by("id"_c);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs");
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report config. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> | where("id"_c == id);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report config by id.");
}


std::vector<domain::risk_report_config>
risk_report_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all risk report config versions. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> | where("id"_c == id) |
                       order_by("id"_c);

    return execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading all risk report config versions by id.");
}


risk_report_config_repository::remove_status risk_report_config_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing risk report config. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<risk_report_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id);

    execute_delete_query(ctx, query, lg(), "Removing risk report config from database.");
    return remove_status::removed;
}

void risk_report_config_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::risk_report_config> risk_report_config_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::risk_report_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest risk report configs with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        filter_condition(filter),
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs with pagination.");
}

std::uint32_t risk_report_config_repository::get_total_config_count(
    context ctx, const std::optional<messaging::risk_report_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active risk report config count";

    const auto query = sqlgen::read<std::vector<risk_report_config_entity>>;

    return execute_count_query<risk_report_config_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting risk report configs");
}

std::vector<domain::risk_report_config>
risk_report_config_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto query = sqlgen::read<std::vector<risk_report_config_entity>> | where("id"_c.in(ids));
    auto result = execute_read_query<risk_report_config_entity, domain::risk_report_config>(
        ctx,
        query,
        [](const auto& entities) { return risk_report_config_mapper::map(entities); },
        lg(),
        "Reading latest risk report configs by ids.");
    return result;
}

void risk_report_config_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<risk_report_config_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids));
    execute_delete_query(ctx, query, lg(), "Batch removing risk report configs.");
}


}
