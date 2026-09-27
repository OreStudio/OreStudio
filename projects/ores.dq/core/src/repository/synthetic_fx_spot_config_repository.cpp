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
#include "ores.dq.core/repository/synthetic_fx_spot_config_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/synthetic_fx_spot_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/synthetic_fx_spot_config_entity.hpp"
#include "ores.dq.core/repository/synthetic_fx_spot_config_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string synthetic_fx_spot_config_repository::sql() {
    return generate_create_table_sql<synthetic_fx_spot_config_entity>(lg());
}

ores::utility::domain::precondition
synthetic_fx_spot_config_repository::replace_claim(context ctx,
                                                   const domain::synthetic_fx_spot_config& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::synthetic_fx_spot_config
synthetic_fx_spot_config_repository::apply_claim(context ctx,
                                                 const domain::synthetic_fx_spot_config& v,
                                                 const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "synthetic_fx_spot_config_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.id)).empty())
        throw std::invalid_argument(
            "synthetic_fx_spot_config_repository::write: a current row already exists");
    return t;
}

void synthetic_fx_spot_config_repository::write(context ctx,
                                                const domain::synthetic_fx_spot_config& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void synthetic_fx_spot_config_repository::write(
    context ctx, const std::vector<domain::synthetic_fx_spot_config>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void synthetic_fx_spot_config_repository::write(context ctx,
                                                const domain::synthetic_fx_spot_config& v,
                                                const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing synthetic FX spot config. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(synthetic_fx_spot_config_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void synthetic_fx_spot_config_repository::write(
    context ctx,
    const std::vector<domain::synthetic_fx_spot_config>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing synthetic FX spot configs. Count: " << v.size();
    std::vector<domain::synthetic_fx_spot_config> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(synthetic_fx_spot_config_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_repository::read_latest(context ctx) {
    const auto query =
        sqlgen::read<std::vector<synthetic_fx_spot_config_entity>> | order_by("id"_c);

    return execute_read_query<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
        ctx,
        query,
        [](const auto& entities) { return synthetic_fx_spot_config_mapper::map(entities); },
        lg(),
        "Reading latest synthetic FX spot configs");
}

std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest synthetic FX spot config. " << "id: " << id;
    const auto query =
        sqlgen::read<std::vector<synthetic_fx_spot_config_entity>> | where("id"_c == id);

    return execute_read_query<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
        ctx,
        query,
        [](const auto& entities) { return synthetic_fx_spot_config_mapper::map(entities); },
        lg(),
        "Reading latest synthetic FX spot config by id.");
}


std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all synthetic FX spot config versions. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<synthetic_fx_spot_config_entity>> |
                       where("id"_c == id) | order_by("id"_c);

    return execute_read_query<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
        ctx,
        query,
        [](const auto& entities) { return synthetic_fx_spot_config_mapper::map(entities); },
        lg(),
        "Reading all synthetic FX spot config versions by id.");
}


synthetic_fx_spot_config_repository::remove_status synthetic_fx_spot_config_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing synthetic FX spot config. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<synthetic_fx_spot_config_entity> |
                       where("tenant_id"_c == tid && "id"_c == id);

    execute_delete_query(ctx, query, lg(), "Removing synthetic FX spot config from database.");
    return remove_status::removed;
}

void synthetic_fx_spot_config_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::synthetic_fx_spot_config> synthetic_fx_spot_config_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest synthetic FX spot configs with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<synthetic_fx_spot_config_entity>> |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
        ctx,
        query,
        [](const auto& entities) { return synthetic_fx_spot_config_mapper::map(entities); },
        lg(),
        "Reading latest synthetic FX spot configs with pagination.");
}

std::uint32_t synthetic_fx_spot_config_repository::get_total_config_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active synthetic FX spot config count";

    struct count_result {
        long long count;
    };

    const auto query =
        sqlgen::select_from<synthetic_fx_spot_config_entity>(sqlgen::count().as<"count">()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active synthetic FX spot config count: " << count;
    return count;
}

std::vector<domain::synthetic_fx_spot_config>
synthetic_fx_spot_config_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    const auto query =
        sqlgen::read<std::vector<synthetic_fx_spot_config_entity>> | where("id"_c.in(ids));
    auto result =
        execute_read_query<synthetic_fx_spot_config_entity, domain::synthetic_fx_spot_config>(
            ctx,
            query,
            [](const auto& entities) { return synthetic_fx_spot_config_mapper::map(entities); },
            lg(),
            "Reading latest synthetic FX spot configs by ids.");
    return result;
}

void synthetic_fx_spot_config_repository::remove(context ctx, const std::vector<std::string>& ids) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<synthetic_fx_spot_config_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids));
    execute_delete_query(ctx, query, lg(), "Batch removing synthetic FX spot configs.");
}


}
