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
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/curve_parametric_smile_parameter_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_entity.hpp"
#include "ores.refdata.core/repository/curve_parametric_smile_parameter_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string curve_parametric_smile_parameter_repository::sql() {
    return generate_create_table_sql<curve_parametric_smile_parameter_entity>(lg());
}

ores::utility::domain::precondition curve_parametric_smile_parameter_repository::replace_claim(
    context ctx, const domain::curve_parametric_smile_parameter& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::curve_parametric_smile_parameter curve_parametric_smile_parameter_repository::apply_claim(
    context ctx,
    const domain::curve_parametric_smile_parameter& v,
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

void curve_parametric_smile_parameter_repository::write(
    context ctx, const domain::curve_parametric_smile_parameter& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void curve_parametric_smile_parameter_repository::write(
    context ctx, const std::vector<domain::curve_parametric_smile_parameter>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void curve_parametric_smile_parameter_repository::write(
    context ctx,
    const domain::curve_parametric_smile_parameter& v,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve parametric smile parameter. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        curve_parametric_smile_parameter_mapper::map(t),
                        lg(),
                        "Writing curve parametric smile parameter to database.");
}

void curve_parametric_smile_parameter_repository::write(
    context ctx,
    const std::vector<domain::curve_parametric_smile_parameter>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve parametric smile parameters. Count: " << v.size();
    std::vector<domain::curve_parametric_smile_parameter> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        curve_parametric_smile_parameter_mapper::map(batch),
                        lg(),
                        "Writing curve parametric smile parameters to database.");
}

std::vector<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<curve_parametric_smile_parameter_entity,
                              domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading latest curve parametric smile parameters");
}

std::vector<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve parametric smile parameter. "
                               << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<curve_parametric_smile_parameter_entity,
                              domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading latest curve parametric smile parameter by id.");
}


std::vector<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all curve parametric smile parameter versions. "
                               << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<curve_parametric_smile_parameter_entity,
                              domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading all curve parametric smile parameter versions by id.");
}

std::optional<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_at_version(context ctx,
                                                             const std::string& id,
                                                             std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading curve parametric smile parameter at version. "
                               << "id: " << id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<curve_parametric_smile_parameter_entity,
                                             domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading curve parametric smile parameter at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


curve_parametric_smile_parameter_repository::remove_status
curve_parametric_smile_parameter_repository::remove(context ctx,
                                                    const std::string& id,
                                                    std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing curve parametric smile parameter. " << "id: " << id;
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
    const auto query = sqlgen::delete_from<curve_parametric_smile_parameter_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(
        ctx, query, lg(), "Removing curve parametric smile parameter from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void curve_parametric_smile_parameter_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_latest(context ctx,
                                                         std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve parametric smile parameters with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<curve_parametric_smile_parameter_entity,
                              domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading latest curve parametric smile parameters with pagination.");
}

std::uint32_t
curve_parametric_smile_parameter_repository::get_total_smile_parameter_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active curve parametric smile parameter count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<curve_parametric_smile_parameter_entity>(
                           sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active curve parametric smile parameter count: " << count;
    return count;
}

std::vector<domain::curve_parametric_smile_parameter>
curve_parametric_smile_parameter_repository::read_latest(context ctx,
                                                         const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_parametric_smile_parameter_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<curve_parametric_smile_parameter_entity,
                                     domain::curve_parametric_smile_parameter>(
        ctx,
        query,
        [](const auto& entities) { return curve_parametric_smile_parameter_mapper::map(entities); },
        lg(),
        "Reading latest curve parametric smile parameters by ids.");
    return result;
}

void curve_parametric_smile_parameter_repository::remove(context ctx,
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
    const auto query = sqlgen::delete_from<curve_parametric_smile_parameter_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing curve parametric smile parameters.");
}


}
