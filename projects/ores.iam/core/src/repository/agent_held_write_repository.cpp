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
#include "ores.iam.core/repository/agent_held_write_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/agent_held_write.hpp"
#include "ores.iam.api/domain/agent_held_write_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/agent_held_write_entity.hpp"
#include "ores.iam.core/repository/agent_held_write_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
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
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string agent_held_write_repository::sql() {
    return query_builder::insert_statement_sql<agent_held_write_entity>(lg());
}

bool agent_held_write_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order agent_held_write_list_order(const ores::utility::domain::order& order,
                                                 std::initializer_list<std::string> default_columns,
                                                 bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"request_id"});
    if (!agent_held_write_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of agent held writes cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"request_id"});
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool agent_held_write_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using agent_held_write_rows_by_key =
    std::map<std::tuple<std::string>, std::vector<domain::agent_held_write>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
agent_held_write_rows_by_key
agent_held_write_current_rows(agent_held_write_repository& repo,
                              const ores::database::context& ctx,
                              const std::vector<domain::agent_held_write>& of) {
    std::vector<std::string> request_ids;
    request_ids.reserve(of.size());
    for (const auto& row : of) {
        request_ids.push_back(row.request_id);
    }
    agent_held_write_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, request_ids))
        current[std::make_tuple(item.request_id)].push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::agent_held_write>&
agent_held_write_rows_of(const agent_held_write_rows_by_key& current,
                         const domain::agent_held_write& item) {
    static const std::vector<domain::agent_held_write> none;
    const auto found = current.find(std::make_tuple(item.request_id));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
agent_held_write_replace_claim(const std::vector<domain::agent_held_write>& current) {
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
domain::agent_held_write
agent_held_write_apply_claim(const domain::agent_held_write& v,
                             const ores::utility::domain::precondition& claim,
                             const std::vector<domain::agent_held_write>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "agent_held_write_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument(
            "agent_held_write_repository::write: a current row already exists");
    return t;
}

}

void agent_held_write_repository::write(const context& ctx, const domain::agent_held_write& v) {
    write(ctx, v, agent_held_write_replace_claim(read_latest(ctx, v.request_id)));
}

void agent_held_write_repository::write(const context& ctx,
                                        const std::vector<domain::agent_held_write>& v) {
    const auto current = agent_held_write_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(agent_held_write_replace_claim(agent_held_write_rows_of(current, item)));
    write(ctx, v, claims);
}

void agent_held_write_repository::write(const context& ctx,
                                        const domain::agent_held_write& v,
                                        const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing write. " << "request_id: " << v.request_id;
    const auto t = agent_held_write_apply_claim(v,
                                                claim,
                                                agent_held_write_claim_reads_current(claim) ?
                                                    read_latest(ctx, v.request_id) :
                                                    std::vector<domain::agent_held_write>{});
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(agent_held_write_mapper::map(t)),
        lg(),
        "Writing write to database.");
}

void agent_held_write_repository::write(
    const context& ctx,
    const std::vector<domain::agent_held_write>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing agent held writes. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "agent_held_write_repository::write: one claim is needed for each row");
    std::vector<domain::agent_held_write> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (agent_held_write_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = agent_held_write_current_rows(*this, ctx, to_read);
    std::vector<domain::agent_held_write> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(
            agent_held_write_apply_claim(v[i], claims[i], agent_held_write_rows_of(current, v[i])));
    query_builder::execute_write_op(
        ctx,
        query_builder::insert_or_replace(agent_held_write_mapper::map(batch)),
        lg(),
        "Writing agent held writes to database.");
}

std::vector<domain::agent_held_write> agent_held_write_repository::read_latest(const context& ctx) {
    const auto query = query_builder::read<std::vector<agent_held_write_entity>> |
                       query_builder::order_by("request_id"_c);

    return query_builder::execute_read_query<agent_held_write_entity, domain::agent_held_write>(
        ctx,
        query,
        [](const auto& entities) { return agent_held_write_mapper::map(entities); },
        lg(),
        "Reading latest agent held writes");
}

std::vector<domain::agent_held_write>
agent_held_write_repository::read_latest(const context& ctx, const std::string& request_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest write. " << "request_id: " << request_id;
    const auto query = query_builder::read<std::vector<agent_held_write_entity>> |
                       query_builder::where("request_id"_c == request_id);

    return query_builder::execute_read_query<agent_held_write_entity, domain::agent_held_write>(
        ctx,
        query,
        [](const auto& entities) { return agent_held_write_mapper::map(entities); },
        lg(),
        "Reading latest write by request_id.");
}


std::vector<domain::agent_held_write>
agent_held_write_repository::read_all(const context& ctx, const std::string& request_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading all write versions. " << "request_id: " << request_id;
    const auto query = query_builder::read<std::vector<agent_held_write_entity>> |
                       query_builder::where("request_id"_c == request_id) |
                       query_builder::order_by("request_id"_c);

    return query_builder::execute_read_query<agent_held_write_entity, domain::agent_held_write>(
        ctx,
        query,
        [](const auto& entities) { return agent_held_write_mapper::map(entities); },
        lg(),
        "Reading all write versions by request_id.");
}


agent_held_write_repository::remove_status agent_held_write_repository::remove(
    const context& ctx, const std::string& request_id, std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing write. " << "request_id: " << request_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, request_id);
    if (current.empty())
        return remove_status::missing;
    const auto query = query_builder::delete_from<agent_held_write_entity> |
                       query_builder::where("request_id"_c == request_id);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing write from database.");
    return remove_status::removed;
}

void agent_held_write_repository::remove(const context& ctx, const std::string& request_id) {
    static_cast<void>(remove(ctx, request_id, std::nullopt));
}

std::vector<domain::agent_held_write>
agent_held_write_repository::read_latest(const context& ctx,
                                         std::uint32_t offset,
                                         std::uint32_t limit,
                                         const ores::utility::domain::order& order) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest agent held writes with offset: " << offset
                              << " and limit: " << limit;
    const auto query = query_builder::read<std::vector<agent_held_write_entity>> |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<agent_held_write_entity,
                                                     domain::agent_held_write>(
        ctx,
        query,
        agent_held_write_list_order(order, {"request_id"}, false),
        std::nullopt,
        [](const auto& entities) { return agent_held_write_mapper::map(entities); },
        lg(),
        "Reading latest agent held writes with pagination.");
}

std::uint32_t agent_held_write_repository::get_total__count(const context& ctx) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active write count";

    const auto query = query_builder::read<std::vector<agent_held_write_entity>>;

    return query_builder::execute_count_query<agent_held_write_entity>(
        ctx, query, std::nullopt, lg(), "Counting agent held writes");
}

std::vector<domain::agent_held_write>
agent_held_write_repository::read_latest(const context& ctx,
                                         const std::vector<std::string>& request_ids) {
    if (request_ids.empty())
        return {};
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (request_ids.size() > chunk_size) {
        std::vector<domain::agent_held_write> all;
        for (std::size_t begin = 0; begin < request_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, request_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(request_ids.begin() + begin, request_ids.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto query = query_builder::read<std::vector<agent_held_write_entity>> |
                       query_builder::where("request_id"_c.in(request_ids));
    auto result =
        query_builder::execute_read_query<agent_held_write_entity, domain::agent_held_write>(
            ctx,
            query,
            [](const auto& entities) { return agent_held_write_mapper::map(entities); },
            lg(),
            "Reading latest agent held writes by ids.");
    return result;
}

void agent_held_write_repository::remove(const context& ctx,
                                         const std::vector<std::string>& request_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (request_ids.empty())
        return;
    const auto query = query_builder::delete_from<agent_held_write_entity> |
                       query_builder::where("request_id"_c.in(request_ids));
    query_builder::execute_delete_query(ctx, query, lg(), "Batch removing agent held writes.");
}


}
