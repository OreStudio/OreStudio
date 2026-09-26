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
#include "ores.compute.core/repository/node_sample_repository.hpp"
#include "ores.compute.api/domain/node_sample_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/node_sample_entity.hpp"
#include "ores.compute.core/repository/node_sample_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <set>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <tuple>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string node_sample_repository::sql() {
    return generate_create_table_sql<node_sample_entity>(lg());
}

ores::utility::domain::precondition
node_sample_repository::replace_claim(context ctx, const domain::node_sample& v) {
    const auto current = read_latest(ctx,
                                     boost::uuids::to_string(v.id),
                                     ores::platform::time::datetime::to_db_string(v.sampled_at));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::node_sample node_sample_repository::apply_claim(
    context ctx, const domain::node_sample& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "node_sample_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx,
                     boost::uuids::to_string(v.id),
                     ores::platform::time::datetime::to_db_string(v.sampled_at))
             .empty())
        throw std::invalid_argument("node_sample_repository::write: a current row already exists");
    return t;
}

void node_sample_repository::write(context ctx, const domain::node_sample& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void node_sample_repository::write(context ctx, const std::vector<domain::node_sample>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void node_sample_repository::write(context ctx,
                                   const domain::node_sample& v,
                                   const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing sample. " << "id: " << v.id
                               << " sampled_at: " << v.sampled_at;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(node_sample_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void node_sample_repository::write(context ctx,
                                   const std::vector<domain::node_sample>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing node samples. Count: " << v.size();
    std::vector<domain::node_sample> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(node_sample_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::node_sample> node_sample_repository::read_latest(context ctx) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<node_sample_entity>> | where("tenant_id"_c == tid) |
                       order_by("id"_c, "sampled_at"_c);

    return execute_read_query<node_sample_entity, domain::node_sample>(
        ctx,
        query,
        [](const auto& entities) { return node_sample_mapper::map(entities); },
        lg(),
        "Reading latest node samples");
}

std::vector<domain::node_sample> node_sample_repository::read_latest(
    context ctx, const std::string& id, const std::string& sampled_at) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest sample. " << "id: " << id
                               << " sampled_at: " << sampled_at;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<node_sample_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "sampled_at"_c == sampled_at);

    return execute_read_query<node_sample_entity, domain::node_sample>(
        ctx,
        query,
        [](const auto& entities) { return node_sample_mapper::map(entities); },
        lg(),
        "Reading latest sample by id.");
}


std::vector<domain::node_sample> node_sample_repository::read_all(context ctx,
                                                                  const std::string& id,
                                                                  const std::string& sampled_at) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all sample versions. " << "id: " << id
                               << " sampled_at: " << sampled_at;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<node_sample_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "sampled_at"_c == sampled_at) |
                       order_by("id"_c, "sampled_at"_c);

    return execute_read_query<node_sample_entity, domain::node_sample>(
        ctx,
        query,
        [](const auto& entities) { return node_sample_mapper::map(entities); },
        lg(),
        "Reading all sample versions by id.");
}


node_sample_repository::remove_status
node_sample_repository::remove(context ctx,
                               const std::string& id,
                               const std::string& sampled_at,
                               std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing sample. " << "id: " << id
                               << " sampled_at: " << sampled_at;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id, sampled_at);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<node_sample_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "sampled_at"_c == sampled_at);

    execute_delete_query(ctx, query, lg(), "Removing sample from database.");
    return remove_status::removed;
}

void node_sample_repository::remove(context ctx,
                                    const std::string& id,
                                    const std::string& sampled_at) {
    static_cast<void>(remove(ctx, id, sampled_at, std::nullopt));
}

std::vector<domain::node_sample>
node_sample_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest node samples with offset: " << offset
                               << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<node_sample_entity>> | where("tenant_id"_c == tid) |
                       order_by("id"_c, "sampled_at"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<node_sample_entity, domain::node_sample>(
        ctx,
        query,
        [](const auto& entities) { return node_sample_mapper::map(entities); },
        lg(),
        "Reading latest node samples with pagination.");
}

std::uint32_t node_sample_repository::get_total__count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active sample count";

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<node_sample_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active sample count: " << count;
    return count;
}

std::vector<domain::node_sample> node_sample_repository::read_latest(
    context ctx, const std::vector<std::string>& ids, const std::vector<std::string>& sampled_ats) {
    if (ids.empty() || sampled_ats.empty())
        return {};
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<node_sample_entity>> |
        where("tenant_id"_c == tid && "id"_c.in(ids) && "sampled_at"_c.in(sampled_ats));
    auto result = execute_read_query<node_sample_entity, domain::node_sample>(
        ctx,
        query,
        [](const auto& entities) { return node_sample_mapper::map(entities); },
        lg(),
        "Reading latest node samples by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (sampled_ats.size() != ids.size())
        throw std::invalid_argument(
            "node_sample_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < ids.size(); ++i)
        requested.emplace(ids[i], sampled_ats[i]);
    std::vector<domain::node_sample> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.id),
                                ores::platform::time::datetime::to_db_string(item.sampled_at))))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void node_sample_repository::remove(context ctx,
                                    const std::vector<std::string>& ids,
                                    const std::vector<std::string>& sampled_ats) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sampled_ats.size() != ids.size())
        throw std::invalid_argument(
            "node_sample_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < ids.size(); ++i)
        remove(ctx, ids[i], sampled_ats[i]);
}


}
