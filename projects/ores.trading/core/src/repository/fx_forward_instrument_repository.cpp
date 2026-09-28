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
#include "ores.trading.core/repository/fx_forward_instrument_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/fx_forward_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/fx_forward_instrument_entity.hpp"
#include "ores.trading.core/repository/fx_forward_instrument_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string fx_forward_instrument_repository::sql() {
    return generate_create_table_sql<fx_forward_instrument_entity>(lg());
}

ores::utility::domain::precondition
fx_forward_instrument_repository::replace_claim(context ctx,
                                                const domain::fx_forward_instrument& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.trade_id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().identity.version)};
}

domain::fx_forward_instrument
fx_forward_instrument_repository::apply_claim(context ctx,
                                              const domain::fx_forward_instrument& v,
                                              const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.identity.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.identity.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.identity.trade_id));
            t.identity.version = current.empty() ? 0 : current.front().identity.version;
            break;
        }
    }
    return t;
}

void fx_forward_instrument_repository::write(context ctx, const domain::fx_forward_instrument& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void fx_forward_instrument_repository::write(context ctx,
                                             const std::vector<domain::fx_forward_instrument>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void fx_forward_instrument_repository::write(context ctx,
                                             const domain::fx_forward_instrument& v,
                                             const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX forward instrument. "
                               << "trade_id: " << v.identity.trade_id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx,
                        fx_forward_instrument_mapper::map(t),
                        lg(),
                        "Writing FX forward instrument to database.");
}

void fx_forward_instrument_repository::write(
    context ctx,
    const std::vector<domain::fx_forward_instrument>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FX forward instruments. Count: " << v.size();
    std::vector<domain::fx_forward_instrument> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(ctx,
                        fx_forward_instrument_mapper::map(batch),
                        lg(),
                        "Writing FX forward instruments to database.");
}

std::vector<domain::fx_forward_instrument>
fx_forward_instrument_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<fx_forward_instrument_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("trade_id"_c);
        return execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
            ctx,
            query,
            [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
            lg(),
            "Reading latest FX forward instruments (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<fx_forward_instrument_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("trade_id"_c);

    return execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FX forward instruments");
}

std::vector<domain::fx_forward_instrument>
fx_forward_instrument_repository::read_latest(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX forward instrument. "
                               << "trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<fx_forward_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c == trade_id && "valid_to"_c == max.value());

    return execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FX forward instrument by trade_id.");
}


std::vector<domain::fx_forward_instrument>
fx_forward_instrument_repository::read_all(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all FX forward instrument versions. "
                               << "trade_id: " << trade_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<fx_forward_instrument_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "trade_id"_c == trade_id) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
        lg(),
        "Reading all FX forward instrument versions by trade_id.");
}

std::optional<domain::fx_forward_instrument> fx_forward_instrument_repository::read_at_version(
    context ctx, const std::string& trade_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading FX forward instrument at version. "
                               << "trade_id: " << trade_id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<fx_forward_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c == trade_id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
            ctx,
            query,
            [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
            lg(),
            "Reading FX forward instrument at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


fx_forward_instrument_repository::remove_status fx_forward_instrument_repository::remove(
    context ctx, const std::string& trade_id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FX forward instrument. " << "trade_id: " << trade_id;
    const auto current = read_latest(ctx, trade_id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().identity.version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().identity.version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::delete_from<fx_forward_instrument_entity> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "trade_id"_c == trade_id &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing FX forward instrument from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, trade_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void fx_forward_instrument_repository::remove(context ctx, const std::string& trade_id) {
    static_cast<void>(remove(ctx, trade_id, std::nullopt));
}

std::vector<domain::fx_forward_instrument> fx_forward_instrument_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FX forward instruments with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<fx_forward_instrument_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("trade_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FX forward instruments with pagination.");
}

std::uint32_t fx_forward_instrument_repository::get_total_fx_forward_instrument_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active FX forward instrument count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<fx_forward_instrument_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active FX forward instrument count: " << count;
    return count;
}

std::vector<domain::fx_forward_instrument>
fx_forward_instrument_repository::read_latest(context ctx,
                                              const std::vector<std::string>& trade_ids) {
    if (trade_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<fx_forward_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<fx_forward_instrument_entity, domain::fx_forward_instrument>(
        ctx,
        query,
        [](const auto& entities) { return fx_forward_instrument_mapper::map(entities); },
        lg(),
        "Reading latest FX forward instruments by ids.");
    return result;
}

void fx_forward_instrument_repository::remove(context ctx,
                                              const std::vector<std::string>& trade_ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (trade_ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<fx_forward_instrument_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing FX forward instruments.");
}


}
