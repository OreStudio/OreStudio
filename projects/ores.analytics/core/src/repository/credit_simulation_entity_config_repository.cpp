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
#include "ores.analytics.core/repository/credit_simulation_entity_config_repository.hpp"
#include "ores.analytics.api/domain/credit_simulation_entity_config.hpp"
#include "ores.analytics.api/domain/credit_simulation_entity_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/messaging/credit_simulation_entity_config_protocol.hpp"
#include "ores.analytics.core/repository/credit_simulation_entity_config_entity.hpp"
#include "ores.analytics.core/repository/credit_simulation_entity_config_mapper.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::analytics::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string credit_simulation_entity_config_repository::sql() {
    return query_builder::insert_statement_sql<credit_simulation_entity_config_entity>(lg());
}

bool credit_simulation_entity_config_repository::is_sortable(std::string_view field) {
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
credit_simulation_entity_config_list_order(const ores::utility::domain::order& order,
                                           std::initializer_list<std::string> default_columns,
                                           bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id"});
    if (!credit_simulation_entity_config_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of credit simulation entities cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> credit_simulation_entity_config_filter_condition(
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->credit_simulation_config_id)
        r.push_back(query_builder::equals(
            "credit_simulation_config_id",
            query_builder::filter_value(*filter->credit_simulation_config_id)));
    if (filter->transition_matrix_id)
        r.push_back(query_builder::equals(
            "transition_matrix_id", query_builder::filter_value(*filter->transition_matrix_id)));
    if (filter->id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("id", std::move(values)));
    }
    if (filter->credit_simulation_config_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->credit_simulation_config_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("credit_simulation_config_id", std::move(values)));
    }
    if (filter->transition_matrix_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->transition_matrix_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("transition_matrix_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

ores::utility::domain::precondition credit_simulation_entity_config_repository::replace_claim(
    context ctx, const domain::credit_simulation_entity_config& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::credit_simulation_entity_config credit_simulation_entity_config_repository::apply_claim(
    context ctx,
    const domain::credit_simulation_entity_config& v,
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

void credit_simulation_entity_config_repository::write(
    context ctx, const domain::credit_simulation_entity_config& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void credit_simulation_entity_config_repository::write(
    context ctx, const std::vector<domain::credit_simulation_entity_config>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void credit_simulation_entity_config_repository::write(
    context ctx,
    const domain::credit_simulation_entity_config& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing credit simulation entity. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    query_builder::execute_write_query(ctx,
                                       credit_simulation_entity_config_mapper::map(t),
                                       lg(),
                                       "Writing credit simulation entity to database.");
}

void credit_simulation_entity_config_repository::write(
    context ctx,
    const std::vector<domain::credit_simulation_entity_config>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing credit simulation entities. Count: " << v.size();
    std::vector<domain::credit_simulation_entity_config> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    query_builder::execute_write_query(ctx,
                                       credit_simulation_entity_config_mapper::map(batch),
                                       lg(),
                                       "Writing credit simulation entities to database.");
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       query_builder::order_by("id"_c);

    return query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                             domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entities");
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest credit simulation entity. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                             domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entity by id.");
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest_by_name(context ctx,
                                                                const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest credit simulation entity by name: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid && "name"_c == name &&
                                            "valid_to"_c == max.value());

    // A key names a row of one party: another party can hold the same key, so
    // the lookup keeps the rows of the session's party when it has one. A
    // finder with a parent column is pinned to its parent's party already.
    auto found = query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                                   domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entity by name.");
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    return found;
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_any_by_name(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any credit simulation entity by name: " << name;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid && "name"_c == name) |
                       query_builder::order_by("valid_from"_c.desc());

    auto found = query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                                   domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading any credit simulation entity by name.");
    // A key names a row of one party, and another party of the tenant may hold
    // the same key, so the read keeps the session's party's rows. The filter
    // runs before the newest row is taken because the party column may be
    // null, and a null party means the row belongs to the whole tenant and
    // stays readable by every party, as the latest-by finder also has it.
    if (const auto party = ctx.party_id(); party.has_value())
        std::erase_if(found, [&](const auto& row) { return row.party_id != *party; });
    if (found.size() > 1)
        found.resize(1);
    return found;
}


std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all credit simulation entity versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id) |
                       query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                             domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading all credit simulation entity versions by id.");
}

std::optional<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_at_version(context ctx,
                                                            const std::string& id,
                                                            std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading credit simulation entity at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
        query_builder::limit(1);

    const auto entities =
        query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                          domain::credit_simulation_entity_config>(
            ctx,
            query,
            [](const auto& entities) {
                return credit_simulation_entity_config_mapper::map(entities);
            },
            lg(),
            "Reading credit simulation entity at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest_by_credit_simulation_config_id(
    context ctx,
    const std::string& credit_simulation_config_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest credit simulation entities. credit_simulation_config_id: "
        << credit_simulation_config_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
        query_builder::where("tenant_id"_c == tid &&
                             "credit_simulation_config_id"_c == credit_simulation_config_id &&
                             "valid_to"_c == max.value()) |
        query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<credit_simulation_entity_config_entity,
                                                     domain::credit_simulation_entity_config>(
        ctx,
        query,
        credit_simulation_entity_config_list_order(order, {"id"}, false),
        credit_simulation_entity_config_filter_condition(filter),
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entities by credit_simulation_config_id.");
}

std::uint32_t
credit_simulation_entity_config_repository::get_total_entity_count_by_credit_simulation_config_id(
    context ctx,
    const std::string& credit_simulation_config_id,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active credit simulation entities count. credit_simulation_config_id: "
        << credit_simulation_config_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
        query_builder::where("tenant_id"_c == tid &&
                             "credit_simulation_config_id"_c == credit_simulation_config_id &&
                             "valid_to"_c == max.value());

    return query_builder::execute_count_query<credit_simulation_entity_config_entity>(
        ctx,
        query,
        credit_simulation_entity_config_filter_condition(filter),
        lg(),
        "Counting credit simulation entities by credit_simulation_config_id");
}


std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest_by_transition_matrix_id(
    context ctx,
    const std::string& transition_matrix_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest credit simulation entities. transition_matrix_id: "
        << transition_matrix_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "transition_matrix_id"_c == transition_matrix_id &&
                                            "valid_to"_c == max.value()) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<credit_simulation_entity_config_entity,
                                                     domain::credit_simulation_entity_config>(
        ctx,
        query,
        credit_simulation_entity_config_list_order(order, {"id"}, false),
        credit_simulation_entity_config_filter_condition(filter),
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entities by transition_matrix_id.");
}

std::uint32_t
credit_simulation_entity_config_repository::get_total_entity_count_by_transition_matrix_id(
    context ctx,
    const std::string& transition_matrix_id,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active credit simulation entities count. transition_matrix_id: "
        << transition_matrix_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "transition_matrix_id"_c == transition_matrix_id &&
                                            "valid_to"_c == max.value());

    return query_builder::execute_count_query<credit_simulation_entity_config_entity>(
        ctx,
        query,
        credit_simulation_entity_config_filter_condition(filter),
        lg(),
        "Counting credit simulation entities by transition_matrix_id");
}


credit_simulation_entity_config_repository::remove_status
credit_simulation_entity_config_repository::remove(context ctx,
                                                   const std::string& id,
                                                   std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing credit simulation entity. " << "id: " << id;
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
    const auto query = query_builder::delete_from<credit_simulation_entity_config_entity> |
                       query_builder::where("tenant_id"_c == tid && "id"_c == id &&
                                            "valid_to"_c == max.value() && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing credit simulation entity from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void credit_simulation_entity_config_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest(
    context ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest credit simulation entities with offset: "
                               << offset << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<credit_simulation_entity_config_entity,
                                                     domain::credit_simulation_entity_config>(
        ctx,
        query,
        credit_simulation_entity_config_list_order(order, {"id"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                credit_simulation_entity_config_filter_condition(filter)),
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entities with pagination.");
}

std::uint32_t credit_simulation_entity_config_repository::get_total_entity_count(
    context ctx,
    const std::optional<messaging::credit_simulation_entity_configs_filter>& filter,
    const std::optional<std::string>& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active credit simulation entity count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<credit_simulation_entity_config_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                credit_simulation_entity_config_filter_condition(filter)),
        lg(),
        "Counting credit simulation entities");
}

std::vector<domain::credit_simulation_entity_config>
credit_simulation_entity_config_repository::read_latest(context ctx,
                                                        const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<credit_simulation_entity_config_entity>> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = query_builder::execute_read_query<credit_simulation_entity_config_entity,
                                                    domain::credit_simulation_entity_config>(
        ctx,
        query,
        [](const auto& entities) { return credit_simulation_entity_config_mapper::map(entities); },
        lg(),
        "Reading latest credit simulation entities by ids.");
    return result;
}

void credit_simulation_entity_config_repository::remove(context ctx,
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
    const auto query =
        query_builder::delete_from<credit_simulation_entity_config_entity> |
        query_builder::where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    query_builder::execute_delete_query(
        ctx, query, lg(), "Batch removing credit simulation entities.");
}


}
