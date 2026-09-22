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
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_pair_convention_entity.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_pair_convention_repository::sql() {
    return generate_create_table_sql<currency_pair_convention_entity>(lg());
}

ores::utility::domain::precondition
currency_pair_convention_repository::replace_claim(context ctx,
                                                   const domain::currency_pair_convention& v) {
    const auto current = read_latest(ctx, v.pair_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::currency_pair_convention
currency_pair_convention_repository::apply_claim(context ctx,
                                                 const domain::currency_pair_convention& v,
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
            const auto current = read_latest(ctx, v.pair_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void currency_pair_convention_repository::write(context ctx,
                                                const domain::currency_pair_convention& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void currency_pair_convention_repository::write(
    context ctx, const std::vector<domain::currency_pair_convention>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void currency_pair_convention_repository::write(context ctx,
                                                const domain::currency_pair_convention& v,
                                                const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair convention. "
                               << "pair_code: " << v.pair_code;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        currency_pair_convention_mapper::map(t),
                        lg(),
                        "Writing currency pair convention to database.");
}

void currency_pair_convention_repository::write(
    context ctx,
    const std::vector<domain::currency_pair_convention>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair conventions. Count: " << v.size();
    std::vector<domain::currency_pair_convention> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        currency_pair_convention_mapper::map(batch),
                        lg(),
                        "Writing currency pair conventions to database.");
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("pair_code"_c);

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair conventions");
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_latest(context ctx, const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair convention. "
                               << "pair_code: " << pair_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_pair_convention_entity>> |
        where("tenant_id"_c == tid && "pair_code"_c == pair_code && "valid_to"_c == max.value());

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair convention by pair_code.");
}


std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_all(context ctx, const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all currency pair convention versions. "
                               << "pair_code: " << pair_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "pair_code"_c == pair_code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading all currency pair convention versions by pair_code.");
}

std::optional<domain::currency_pair_convention>
currency_pair_convention_repository::read_at_version(context ctx,
                                                     const std::string& pair_code,
                                                     std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading currency pair convention at version. "
                               << "pair_code: " << pair_code << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_pair_convention_entity>> |
        where("tenant_id"_c == tid && "pair_code"_c == pair_code && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities =
        execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
            ctx,
            query,
            [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
            lg(),
            "Reading currency pair convention at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


currency_pair_convention_repository::remove_status currency_pair_convention_repository::remove(
    context ctx, const std::string& pair_code, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair convention. "
                               << "pair_code: " << pair_code;
    const auto current = read_latest(ctx, pair_code);
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
    const auto query = sqlgen::delete_from<currency_pair_convention_entity> |
                       where("tenant_id"_c == tid && "pair_code"_c == pair_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing currency pair convention from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, pair_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void currency_pair_convention_repository::remove(context ctx, const std::string& pair_code) {
    static_cast<void>(remove(ctx, pair_code, std::nullopt));
}

std::vector<domain::currency_pair_convention> currency_pair_convention_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair conventions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("pair_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair conventions with pagination.");
}

std::uint32_t currency_pair_convention_repository::get_total_convention_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency pair convention count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_pair_convention_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency pair convention count: " << count;
    return count;
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_latest(context ctx,
                                                 const std::vector<std::string>& pair_codes) {
    if (pair_codes.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_pair_convention_entity>> |
        where("tenant_id"_c == tid && "pair_code"_c.in(pair_codes) && "valid_to"_c == max.value());
    auto result =
        execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
            ctx,
            query,
            [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
            lg(),
            "Reading latest currency pair conventions by ids.");
    return result;
}

void currency_pair_convention_repository::remove(context ctx,
                                                 const std::vector<std::string>& pair_codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<currency_pair_convention_entity> |
        where("tenant_id"_c == tid && "pair_code"_c.in(pair_codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing currency pair conventions.");
}


}
