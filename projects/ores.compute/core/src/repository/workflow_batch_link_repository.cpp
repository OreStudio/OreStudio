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
#include "ores.compute.core/repository/workflow_batch_link_repository.hpp"
#include "ores.compute.api/domain/workflow_batch_link_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/workflow_batch_link_entity.hpp"
#include "ores.compute.core/repository/workflow_batch_link_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string workflow_batch_link_repository::sql() {
    return generate_create_table_sql<workflow_batch_link_entity>(lg());
}

ores::utility::domain::precondition
workflow_batch_link_repository::replace_claim(context ctx, const domain::workflow_batch_link& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.batch_id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    // No version column to state, so a replace names no claim at all; the
    // store replaces the row as it stands. A create over a live row is refused
    // by the read in apply_claim.
    return {ores::utility::domain::precondition_kind::any, std::nullopt};
}

domain::workflow_batch_link
workflow_batch_link_repository::apply_claim(context ctx,
                                            const domain::workflow_batch_link& v,
                                            const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    // No version column to state, so the claim is honoured by the read alone.
    if (claim.kind == precondition_kind::must_match_version)
        throw std::invalid_argument(
            "workflow_batch_link_repository::write: this table keeps no version to match");
    if (claim.kind == precondition_kind::must_not_exist &&
        !read_latest(ctx, boost::uuids::to_string(v.batch_id)).empty())
        throw std::invalid_argument(
            "workflow_batch_link_repository::write: a current row already exists");
    return t;
}

void workflow_batch_link_repository::write(context ctx, const domain::workflow_batch_link& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void workflow_batch_link_repository::write(context ctx,
                                           const std::vector<domain::workflow_batch_link>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void workflow_batch_link_repository::write(context ctx,
                                           const domain::workflow_batch_link& v,
                                           const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing link. " << "batch_id: " << v.batch_id;
    const auto t = apply_claim(ctx, v, claim);
    const auto query = sqlgen::insert_or_replace(workflow_batch_link_mapper::map(t));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

void workflow_batch_link_repository::write(
    context ctx,
    const std::vector<domain::workflow_batch_link>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workflow batch links. Count: " << v.size();
    std::vector<domain::workflow_batch_link> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    const auto query = sqlgen::insert_or_replace(workflow_batch_link_mapper::map(batch));
    const auto r = sqlgen::session(ctx.connection_pool())
                       .and_then(sqlgen::begin_transaction)
                       .and_then(query)
                       .and_then(sqlgen::commit);
    ensure_success(r, lg());
}

std::vector<domain::workflow_batch_link> workflow_batch_link_repository::read_latest(context ctx) {
    const auto query =
        sqlgen::read<std::vector<workflow_batch_link_entity>> | order_by("batch_id"_c);

    return execute_read_query<workflow_batch_link_entity, domain::workflow_batch_link>(
        ctx,
        query,
        [](const auto& entities) { return workflow_batch_link_mapper::map(entities); },
        lg(),
        "Reading latest workflow batch links");
}

std::vector<domain::workflow_batch_link>
workflow_batch_link_repository::read_latest(context ctx, const std::string& batch_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest link. " << "batch_id: " << batch_id;
    const auto query =
        sqlgen::read<std::vector<workflow_batch_link_entity>> | where("batch_id"_c == batch_id);

    return execute_read_query<workflow_batch_link_entity, domain::workflow_batch_link>(
        ctx,
        query,
        [](const auto& entities) { return workflow_batch_link_mapper::map(entities); },
        lg(),
        "Reading latest link by batch_id.");
}


std::vector<domain::workflow_batch_link>
workflow_batch_link_repository::read_all(context ctx, const std::string& batch_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all link versions. " << "batch_id: " << batch_id;
    const auto query = sqlgen::read<std::vector<workflow_batch_link_entity>> |
                       where("batch_id"_c == batch_id) | order_by("batch_id"_c);

    return execute_read_query<workflow_batch_link_entity, domain::workflow_batch_link>(
        ctx,
        query,
        [](const auto& entities) { return workflow_batch_link_mapper::map(entities); },
        lg(),
        "Reading all link versions by batch_id.");
}


workflow_batch_link_repository::remove_status workflow_batch_link_repository::remove(
    context ctx, const std::string& batch_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing link. " << "batch_id: " << batch_id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, batch_id);
    if (current.empty())
        return remove_status::missing;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<workflow_batch_link_entity> |
                       where("tenant_id"_c == tid && "batch_id"_c == batch_id);

    execute_delete_query(ctx, query, lg(), "Removing link from database.");
    return remove_status::removed;
}

void workflow_batch_link_repository::remove(context ctx, const std::string& batch_id) {
    static_cast<void>(remove(ctx, batch_id, std::nullopt));
}

std::vector<domain::workflow_batch_link> workflow_batch_link_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest workflow batch links with offset: " << offset
                               << " and limit: " << limit;
    const auto query = sqlgen::read<std::vector<workflow_batch_link_entity>> |
                       order_by("batch_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<workflow_batch_link_entity, domain::workflow_batch_link>(
        ctx,
        query,
        [](const auto& entities) { return workflow_batch_link_mapper::map(entities); },
        lg(),
        "Reading latest workflow batch links with pagination.");
}

std::uint32_t workflow_batch_link_repository::get_total__count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active link count";

    struct count_result {
        long long count;
    };

    const auto query =
        sqlgen::select_from<workflow_batch_link_entity>(sqlgen::count().as<"count">()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active link count: " << count;
    return count;
}

std::vector<domain::workflow_batch_link>
workflow_batch_link_repository::read_latest(context ctx,
                                            const std::vector<std::string>& batch_ids) {
    if (batch_ids.empty())
        return {};
    const auto query =
        sqlgen::read<std::vector<workflow_batch_link_entity>> | where("batch_id"_c.in(batch_ids));
    auto result = execute_read_query<workflow_batch_link_entity, domain::workflow_batch_link>(
        ctx,
        query,
        [](const auto& entities) { return workflow_batch_link_mapper::map(entities); },
        lg(),
        "Reading latest workflow batch links by ids.");
    return result;
}

void workflow_batch_link_repository::remove(context ctx,
                                            const std::vector<std::string>& batch_ids) {
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<workflow_batch_link_entity> |
                       where("tenant_id"_c == tid && "batch_id"_c.in(batch_ids));
    execute_delete_query(ctx, query, lg(), "Batch removing workflow batch links.");
}


}
