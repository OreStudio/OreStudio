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
#include "ores.dq.core/repository/lei_relationship_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.api/domain/lei_relationship_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.api/messaging/lei_relationship_protocol.hpp"
#include "ores.dq.core/repository/lei_relationship_entity.hpp"
#include "ores.dq.core/repository/lei_relationship_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::dq::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lei_relationship_repository::sql() {
    return query_builder::insert_statement_sql<lei_relationship_entity>(lg());
}

bool lei_relationship_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order lei_relationship_list_order(const ores::utility::domain::order& order,
                                                 std::initializer_list<std::string> default_columns,
                                                 bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(default_columns,
                                         default_descending != order.descending,
                                         {"relationship_start_node_node_id"});
    if (!lei_relationship_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of LEI relationships cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order(
        {order.field}, order.descending, {"relationship_start_node_node_id"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> lei_relationship_filter_condition(
    const std::optional<messaging::lei_relationships_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->relationship_start_node_node_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->relationship_start_node_node_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("relationship_start_node_node_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool lei_relationship_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using lei_relationship_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::lei_relationship>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
lei_relationship_rows_by_key
lei_relationship_current_rows(lei_relationship_repository& repo,
                              const ores::database::context& ctx,
                              const std::vector<domain::lei_relationship>& of) {
    std::vector<std::string> relationship_start_node_node_ids;
    relationship_start_node_node_ids.reserve(of.size());
    for (const auto& row : of) {
        relationship_start_node_node_ids.push_back(row.relationship_start_node_node_id);
    }
    lei_relationship_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, relationship_start_node_node_ids))
        current[std::make_tuple(item.relationship_start_node_node_id)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::lei_relationship>&
lei_relationship_rows_of(const lei_relationship_rows_by_key& current,
                         const domain::lei_relationship& item) {
    static const std::vector<domain::lei_relationship> none;
    const auto found = current.find(std::make_tuple(item.relationship_start_node_node_id));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
lei_relationship_replace_claim(const std::vector<domain::lei_relationship>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // when the claim is applied.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::lei_relationship
lei_relationship_apply_claim(const domain::lei_relationship& v,
                             const ores::utility::domain::precondition& claim,
                             const std::vector<domain::lei_relationship>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "lei_relationship_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument(
            "lei_relationship_repository::write: a current row already exists");
    return t;
}

}

void lei_relationship_repository::write(const context& ctx, const domain::lei_relationship& v) {
    write(ctx,
          v,
          lei_relationship_replace_claim(read_latest(ctx, v.relationship_start_node_node_id)));
}

void lei_relationship_repository::write(const context& ctx,
                                        const std::vector<domain::lei_relationship>& v) {
    const auto current = lei_relationship_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(lei_relationship_replace_claim(lei_relationship_rows_of(current, item)));
    write(ctx, v, claims);
}

void lei_relationship_repository::write(const context& ctx,
                                        const domain::lei_relationship& v,
                                        const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << v.relationship_start_node_node_id;
    const auto t =
        lei_relationship_apply_claim(v,
                                     claim,
                                     lei_relationship_claim_reads_current(claim) ?
                                         read_latest(ctx, v.relationship_start_node_node_id) :
                                         std::vector<domain::lei_relationship>{});
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(lei_relationship_mapper::map(t)),
        lg(),
        "Writing LEI relationship to database.");
}

void lei_relationship_repository::write(
    const context& ctx,
    const std::vector<domain::lei_relationship>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationships. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "lei_relationship_repository::write: one claim is needed for each row");
    std::vector<domain::lei_relationship> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (lei_relationship_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = lei_relationship_current_rows(*this, ctx, to_read);
    std::vector<domain::lei_relationship> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            lei_relationship_apply_claim(v[i], claims[i], lei_relationship_rows_of(current, v[i])));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(lei_relationship_mapper::map(batch)),
        lg(),
        "Writing LEI relationships to database.");
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(const context& ctx) {
    const auto query = query_builder::read<std::vector<lei_relationship_entity>> |
                       query_builder::order_by("relationship_start_node_node_id"_c);

    return query_builder::execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships");
}

std::vector<domain::lei_relationship>
lei_relationship_repository::read_latest(const context& ctx,
                                         const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    const auto query = query_builder::read<std::vector<lei_relationship_entity>> |
                       query_builder::where("relationship_start_node_node_id"_c ==
                                            relationship_start_node_node_id);

    return query_builder::execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationship by relationship_start_node_node_id.");
}


std::vector<domain::lei_relationship>
lei_relationship_repository::read_all(const context& ctx,
                                      const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all LEI relationship versions. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    const auto query = query_builder::read<std::vector<lei_relationship_entity>> |
                       query_builder::where("relationship_start_node_node_id"_c ==
                                            relationship_start_node_node_id) |
                       query_builder::order_by("relationship_start_node_node_id"_c);

    return query_builder::execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading all LEI relationship versions by relationship_start_node_node_id.");
}


lei_relationship_repository::remove_status
lei_relationship_repository::remove(const context& ctx,
                                    const std::string& relationship_start_node_node_id,
                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, relationship_start_node_node_id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<lei_relationship_entity> |
        query_builder::where("tenant_id"_c == tid && "relationship_start_node_node_id"_c ==
                                                         relationship_start_node_node_id);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing LEI relationship from database.");
    return remove_status::removed;
}

void lei_relationship_repository::remove(const context& ctx,
                                         const std::string& relationship_start_node_node_id) {
    static_cast<void>(remove(ctx, relationship_start_node_node_id, std::nullopt));
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::lei_relationships_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationships with offset: " << offset
                               << " and limit: " << limit;
    const auto query = query_builder::read<std::vector<lei_relationship_entity>> |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<lei_relationship_entity,
                                                     domain::lei_relationship>(
        ctx,
        query,
        lei_relationship_list_order(order, {"relationship_start_node_node_id"}, false),
        lei_relationship_filter_condition(filter),
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships with pagination.");
}

std::uint32_t lei_relationship_repository::get_total_relationship_count(
    const context& ctx, const std::optional<messaging::lei_relationships_filter>& filter) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active LEI relationship count";

    const auto query = query_builder::read<std::vector<lei_relationship_entity>>;

    return query_builder::execute_count_query<lei_relationship_entity>(
        ctx, query, lei_relationship_filter_condition(filter), lg(), "Counting LEI relationships");
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(
    const context& ctx, const std::vector<std::string>& relationship_start_node_node_ids) {
    if (relationship_start_node_node_ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (relationship_start_node_node_ids.size() > chunk_size) {
        std::vector<domain::lei_relationship> all;
        for (std::size_t begin = 0; begin < relationship_start_node_node_ids.size();
             begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, relationship_start_node_node_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(relationship_start_node_node_ids.begin() + begin,
                                         relationship_start_node_node_ids.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto query = query_builder::read<std::vector<lei_relationship_entity>> |
                       query_builder::where("relationship_start_node_node_id"_c.in(
                           relationship_start_node_node_ids));
    auto result =
        query_builder::execute_read_query<lei_relationship_entity, domain::lei_relationship>(
            ctx,
            query,
            [](const auto& entities) { return lei_relationship_mapper::map(entities); },
            lg(),
            "Reading latest LEI relationships by ids.");
    return result;
}

void lei_relationship_repository::remove(
    const context& ctx, const std::vector<std::string>& relationship_start_node_node_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (relationship_start_node_node_ids.empty())
        return;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<lei_relationship_entity> |
        query_builder::where("tenant_id"_c == tid && "relationship_start_node_node_id"_c.in(
                                                         relationship_start_node_node_ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing LEI relationships.");
}


}
