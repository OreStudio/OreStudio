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
#include "ores.refdata.core/repository/tenor_unit_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/tenor_unit_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_unit_entity.hpp"
#include "ores.refdata.core/repository/tenor_unit_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_unit_repository::sql() {
    return generate_create_table_sql<tenor_unit_entity>(lg());
}

ores::utility::domain::precondition
tenor_unit_repository::replace_claim(context ctx, const domain::tenor_unit& v) {
    const auto current = read_latest(ctx, v.code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::tenor_unit tenor_unit_repository::apply_claim(
    context ctx, const domain::tenor_unit& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, v.code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void tenor_unit_repository::write(context ctx, const domain::tenor_unit& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void tenor_unit_repository::write(context ctx, const std::vector<domain::tenor_unit>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void tenor_unit_repository::write(context ctx,
                                  const domain::tenor_unit& v,
                                  const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor unit. " << "code: " << v.code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, tenor_unit_mapper::map(t), lg(), "Writing tenor unit to database.");
}

void tenor_unit_repository::write(context ctx,
                                  const std::vector<domain::tenor_unit>& v,
                                  const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor units. Count: " << v.size();
    std::vector<domain::tenor_unit> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, tenor_unit_mapper::map(batch), lg(), "Writing tenor units to database.");
}

std::vector<domain::tenor_unit> tenor_unit_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor units");
}

std::vector<domain::tenor_unit> tenor_unit_repository::read_latest(context ctx,
                                                                   const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor unit. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenor_unit_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor unit by code.");
}


std::vector<domain::tenor_unit> tenor_unit_repository::read_all(context ctx,
                                                                const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor unit versions. " << "code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading all tenor unit versions by code.");
}

std::optional<domain::tenor_unit> tenor_unit_repository::read_at_version(context ctx,
                                                                         const std::string& code,
                                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenor unit at version. " << "code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading tenor unit at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


tenor_unit_repository::remove_status tenor_unit_repository::remove(
    context ctx, const std::string& code, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor unit. " << "code: " << code;
    const auto current = read_latest(ctx, code);
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
    const auto query = sqlgen::delete_from<tenor_unit_entity> |
                       where("tenant_id"_c == tid && "code"_c == code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing tenor unit from database.");
    return remove_status::removed;
}

void tenor_unit_repository::remove(context ctx, const std::string& code) {
    static_cast<void>(remove(ctx, code, std::nullopt));
}

std::vector<domain::tenor_unit>
tenor_unit_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor units with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor units with pagination.");
}

std::uint32_t tenor_unit_repository::get_total_unit_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor unit count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenor_unit_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor unit count: " << count;
    return count;
}

std::vector<domain::tenor_unit>
tenor_unit_repository::read_latest(context ctx, const std::vector<std::string>& codes) {
    if (codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenor_unit_entity>> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    auto result = execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor units by ids.");
    return result;
}

void tenor_unit_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenor_unit_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenor units.");
}


}
