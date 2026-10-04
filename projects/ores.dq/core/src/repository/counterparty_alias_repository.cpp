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
#include "ores.dq.core/repository/counterparty_alias_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/counterparty_alias_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/counterparty_alias_entity.hpp"
#include "ores.dq.core/repository/counterparty_alias_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_alias_repository::sql() {
    return generate_create_table_sql<counterparty_alias_entity>(lg());
}

bool counterparty_alias_repository::is_sortable(std::string_view field) {
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
        return make_order(default_columns, default_descending != order.descending, {"id_value"});
    if (!counterparty_alias_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of counterparty aliases cannot be ordered by " +
                                    order.field + ".");
    return make_order({order.field}, order.descending, {"id_value"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<sqlgen::dynamic::Condition>
filter_condition(const std::optional<messaging::counterparty_aliases_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<sqlgen::dynamic::Condition> r;
    if (filter->id_value_one_of) {
        std::vector<sqlgen::dynamic::Value> values;
        for (const auto& v : *filter->id_value_one_of)
            values.push_back(filter_value(v));
        r.push_back(one_of("id_value", std::move(values)));
    }
    return all_of(std::move(r));
}

}

ores::utility::domain::precondition
counterparty_alias_repository::replace_claim(context ctx, const domain::counterparty_alias& v) {
    const auto current = read_latest(ctx, v.id_value);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::counterparty_alias
counterparty_alias_repository::apply_claim(context ctx,
                                           const domain::counterparty_alias& v,
                                           const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "counterparty_alias_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !read_latest(ctx, v.id_value).empty())
        throw std::invalid_argument(
            "counterparty_alias_repository::write: a current row already exists");
    return t;
}

void counterparty_alias_repository::write(context ctx, const domain::counterparty_alias& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void counterparty_alias_repository::write(context ctx,
                                          const std::vector<domain::counterparty_alias>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void counterparty_alias_repository::write(context ctx,
                                          const domain::counterparty_alias& v,
                                          const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty alias. " << "id_value: " << v.id_value;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(counterparty_alias_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void counterparty_alias_repository::write(
    context ctx,
    const std::vector<domain::counterparty_alias>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty aliases. Count: " << v.size();
    std::vector<domain::counterparty_alias> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(counterparty_alias_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::counterparty_alias> counterparty_alias_repository::read_latest(context ctx) {
    const auto query =
        sqlgen::read<std::vector<counterparty_alias_entity>> | order_by("id_value"_c);

    return execute_read_query<counterparty_alias_entity, domain::counterparty_alias>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_alias_mapper::map(entities); },
        lg(),
        "Reading latest counterparty aliases");
}

std::vector<domain::counterparty_alias>
counterparty_alias_repository::read_latest(context ctx, const std::string& id_value) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty alias. " << "id_value: " << id_value;
    const auto query =
        sqlgen::read<std::vector<counterparty_alias_entity>> | where("id_value"_c == id_value);

    return execute_read_query<counterparty_alias_entity, domain::counterparty_alias>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_alias_mapper::map(entities); },
        lg(),
        "Reading latest counterparty alias by id_value.");
}


std::vector<domain::counterparty_alias>
counterparty_alias_repository::read_all(context ctx, const std::string& id_value) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty alias versions. "
                               << "id_value: " << id_value;
    const auto query = sqlgen::read<std::vector<counterparty_alias_entity>> |
                       where("id_value"_c == id_value) | order_by("id_value"_c);

    return execute_read_query<counterparty_alias_entity, domain::counterparty_alias>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_alias_mapper::map(entities); },
        lg(),
        "Reading all counterparty alias versions by id_value.");
}


counterparty_alias_repository::remove_status counterparty_alias_repository::remove(
    context ctx, const std::string& id_value, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty alias. " << "id_value: " << id_value;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id_value);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_alias_entity> |
                       where("tenant_id"_c == tid && "id_value"_c == id_value);

    execute_delete_query(ctx, query, lg(), "Removing counterparty alias from database.");
    return remove_status::removed;
}

void counterparty_alias_repository::remove(context ctx, const std::string& id_value) {
    static_cast<void>(remove(ctx, id_value, std::nullopt));
}

std::vector<domain::counterparty_alias> counterparty_alias_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::counterparty_aliases_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty aliases with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<counterparty_alias_entity>> |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_ordered_read_query<counterparty_alias_entity, domain::counterparty_alias>(
        ctx,
        query,
        list_order(order, {"id_value"}, false),
        filter_condition(filter),
        [](const auto& entities) { return counterparty_alias_mapper::map(entities); },
        lg(),
        "Reading latest counterparty aliases with pagination.");
}

std::uint32_t counterparty_alias_repository::get_total_counterparty_alias_count(
    context ctx, const std::optional<messaging::counterparty_aliases_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty alias count";

    const auto query = sqlgen::read<std::vector<counterparty_alias_entity>>;

    return execute_count_query<counterparty_alias_entity>(
        ctx, query, filter_condition(filter), lg(), "Counting counterparty aliases");
}

std::vector<domain::counterparty_alias>
counterparty_alias_repository::read_latest(context ctx, const std::vector<std::string>& id_values) {
    if (id_values.empty())
        return {};
    const auto query =
        sqlgen::read<std::vector<counterparty_alias_entity>> | where("id_value"_c.in(id_values));
    auto result = execute_read_query<counterparty_alias_entity, domain::counterparty_alias>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_alias_mapper::map(entities); },
        lg(),
        "Reading latest counterparty aliases by ids.");
    return result;
}

void counterparty_alias_repository::remove(context ctx, const std::vector<std::string>& id_values) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (id_values.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_alias_entity> |
                       where("tenant_id"_c == tid && "id_value"_c.in(id_values));
    execute_delete_query(ctx, query, lg(), "Batch removing counterparty aliases.");
}


}
