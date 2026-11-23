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
#include "ores.iam.core/repository/agent_call_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/agent_call.hpp"
#include "ores.iam.api/domain/agent_call_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/agent_call_entity.hpp"
#include "ores.iam.core/repository/agent_call_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string agent_call_repository::sql() {
    return query_builder::insert_statement_sql<agent_call_entity>(lg());
}

bool agent_call_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order agent_call_list_order(const ores::utility::domain::order& order,
                                           std::initializer_list<std::string> default_columns,
                                           bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"id", "called_at"});
    if (!agent_call_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of agent calls cannot be ordered by " + order.field +
                                    ".");
    return query_builder::make_order({order.field}, order.descending, {"id", "called_at"});
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool agent_call_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // No version to state, so only a create needs to know whether a row exists.
    return claim.kind == precondition_kind::must_not_exist;
}

using agent_call_rows_by_key =
    std::map<std::tuple<std::string, std::string>, std::vector<domain::agent_call>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
agent_call_rows_by_key agent_call_current_rows(agent_call_repository& repo,
                                               const ores::database::context& ctx,
                                               const std::vector<domain::agent_call>& of) {
    std::vector<std::string> ids;
    ids.reserve(of.size());
    std::vector<std::string> called_ats;
    called_ats.reserve(of.size());
    for (const auto& row : of) {
        ids.push_back(row.id);
        called_ats.push_back(ores::platform::time::datetime::to_db_string(row.called_at));
    }
    agent_call_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, ids, called_ats))
        current[std::make_tuple(item.id,
                                ores::platform::time::datetime::to_db_string(item.called_at))]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::agent_call>& agent_call_rows_of(const agent_call_rows_by_key& current,
                                                          const domain::agent_call& item) {
    static const std::vector<domain::agent_call> none;
    const auto found = current.find(
        std::make_tuple(item.id, ores::platform::time::datetime::to_db_string(item.called_at)));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
agent_call_replace_claim(const std::vector<domain::agent_call>& current) {
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
domain::agent_call agent_call_apply_claim(const domain::agent_call& v,
                                          const ores::utility::domain::precondition& claim,
                                          const std::vector<domain::agent_call>& current) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "agent_call_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist && !current.empty())
        throw std::invalid_argument("agent_call_repository::write: a current row already exists");
    return t;
}

}

void agent_call_repository::write(const context& ctx, const domain::agent_call& v) {
    write(ctx,
          v,
          agent_call_replace_claim(
              read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.called_at))));
}

void agent_call_repository::write(const context& ctx, const std::vector<domain::agent_call>& v) {
    const auto current = agent_call_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(agent_call_replace_claim(agent_call_rows_of(current, item)));
    write(ctx, v, claims);
}

void agent_call_repository::write(const context& ctx,
                                  const domain::agent_call& v,
                                  const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing call. " << "id: " << v.id
                              << " called_at: " << v.called_at;
    const auto t = agent_call_apply_claim(
        v,
        claim,
        agent_call_claim_reads_current(claim) ?
            read_latest(ctx, v.id, ores::platform::time::datetime::to_db_string(v.called_at)) :
            std::vector<domain::agent_call>{});
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(agent_call_mapper::map(t)),
                                    lg(),
                                    "Writing call to database.");
}

void agent_call_repository::write(const context& ctx,
                                  const std::vector<domain::agent_call>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing agent calls. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "agent_call_repository::write: one claim is needed for each row");
    std::vector<domain::agent_call> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (agent_call_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = agent_call_current_rows(*this, ctx, to_read);
    std::vector<domain::agent_call> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(agent_call_apply_claim(v[i], claims[i], agent_call_rows_of(current, v[i])));
    query_builder::execute_write_op(ctx,
                                    query_builder::insert_or_replace(agent_call_mapper::map(batch)),
                                    lg(),
                                    "Writing agent calls to database.");
}

std::vector<domain::agent_call> agent_call_repository::read_latest(const context& ctx) {
    const auto query = query_builder::read<std::vector<agent_call_entity>> |
                       query_builder::order_by("id"_c, "called_at"_c);

    return query_builder::execute_read_query<agent_call_entity, domain::agent_call>(
        ctx,
        query,
        [](const auto& entities) { return agent_call_mapper::map(entities); },
        lg(),
        "Reading latest agent calls");
}

std::vector<domain::agent_call> agent_call_repository::read_latest(const context& ctx,
                                                                   const std::string& id,
                                                                   const std::string& called_at) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest call. " << "id: " << id
                              << " called_at: " << called_at;
    const auto query = query_builder::read<std::vector<agent_call_entity>> |
                       query_builder::where("id"_c == id && "called_at"_c == called_at);

    return query_builder::execute_read_query<agent_call_entity, domain::agent_call>(
        ctx,
        query,
        [](const auto& entities) { return agent_call_mapper::map(entities); },
        lg(),
        "Reading latest call by id.");
}


std::vector<domain::agent_call> agent_call_repository::read_all(const context& ctx,
                                                                const std::string& id,
                                                                const std::string& called_at) {
    ORES_LOG_SEV(lg(), debug) << "Reading all call versions. " << "id: " << id
                              << " called_at: " << called_at;
    const auto query = query_builder::read<std::vector<agent_call_entity>> |
                       query_builder::where("id"_c == id && "called_at"_c == called_at) |
                       query_builder::order_by("id"_c, "called_at"_c);

    return query_builder::execute_read_query<agent_call_entity, domain::agent_call>(
        ctx,
        query,
        [](const auto& entities) { return agent_call_mapper::map(entities); },
        lg(),
        "Reading all call versions by id.");
}


agent_call_repository::remove_status
agent_call_repository::remove(const context& ctx,
                              const std::string& id,
                              const std::string& called_at,
                              std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing call. " << "id: " << id << " called_at: " << called_at;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id, called_at);
    if (current.empty())
        return remove_status::missing;
    const auto query = query_builder::delete_from<agent_call_entity> |
                       query_builder::where("id"_c == id && "called_at"_c == called_at);

    query_builder::execute_delete_query(ctx, query, lg(), "Removing call from database.");
    return remove_status::removed;
}

void agent_call_repository::remove(const context& ctx,
                                   const std::string& id,
                                   const std::string& called_at) {
    static_cast<void>(remove(ctx, id, called_at, std::nullopt));
}

std::vector<domain::agent_call>
agent_call_repository::read_latest(const context& ctx,
                                   std::uint32_t offset,
                                   std::uint32_t limit,
                                   const ores::utility::domain::order& order) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest agent calls with offset: " << offset
                              << " and limit: " << limit;
    const auto query = query_builder::read<std::vector<agent_call_entity>> |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<agent_call_entity, domain::agent_call>(
        ctx,
        query,
        agent_call_list_order(order, {"id", "called_at"}, false),
        std::nullopt,
        [](const auto& entities) { return agent_call_mapper::map(entities); },
        lg(),
        "Reading latest agent calls with pagination.");
}

std::uint32_t agent_call_repository::get_total__count(const context& ctx) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active call count";

    const auto query = query_builder::read<std::vector<agent_call_entity>>;

    return query_builder::execute_count_query<agent_call_entity>(
        ctx, query, std::nullopt, lg(), "Counting agent calls");
}

std::vector<domain::agent_call>
agent_call_repository::read_latest(const context& ctx,
                                   const std::vector<std::string>& ids,
                                   const std::vector<std::string>& called_ats) {
    if (ids.empty() || called_ats.empty())
        return {};
    if (called_ats.size() != ids.size())
        throw std::invalid_argument(
            "agent_call_repository::read_latest: key column vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (ids.size() > chunk_size) {
        std::vector<domain::agent_call> all;
        for (std::size_t begin = 0; begin < ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(ids.begin() + begin, ids.begin() + end),
                std::vector<std::string>(called_ats.begin() + begin, called_ats.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    const auto query = query_builder::read<std::vector<agent_call_entity>> |
                       query_builder::where("id"_c.in(ids) && "called_at"_c.in(called_ats));
    auto result = query_builder::execute_read_query<agent_call_entity, domain::agent_call>(
        ctx,
        query,
        [](const auto& entities) { return agent_call_mapper::map(entities); },
        lg(),
        "Reading latest agent calls by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < ids.size(); ++i)
        requested.emplace(ids[i], called_ats[i]);
    std::vector<domain::agent_call> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(
                item.id, ores::platform::time::datetime::to_db_string(item.called_at))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void agent_call_repository::remove(const context& ctx,
                                   const std::vector<std::string>& ids,
                                   const std::vector<std::string>& called_ats) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (called_ats.size() != ids.size())
        throw std::invalid_argument(
            "agent_call_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < ids.size(); ++i)
        remove(ctx, ids[i], called_ats[i]);
}


}
