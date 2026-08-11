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
#include "ores.trading.core/repository/equity_swap_instrument_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/equity_swap_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/equity_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_swap_instrument_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string equity_swap_instrument_repository::sql() {
    return generate_create_table_sql<equity_swap_instrument_entity>(lg());
}

void equity_swap_instrument_repository::write(context ctx,
                                              const domain::equity_swap_instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity swap instrument. "
                               << "instrument_id: " << v.identity.instrument_id;
    execute_write_query(ctx,
                        equity_swap_instrument_mapper::map(v),
                        lg(),
                        "Writing equity swap instrument to database.");
}

void equity_swap_instrument_repository::write(
    context ctx, const std::vector<domain::equity_swap_instrument>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity swap instruments. Count: " << v.size();
    execute_write_query(ctx,
                        equity_swap_instrument_mapper::map(v),
                        lg(),
                        "Writing equity swap instruments to database.");
}

std::vector<domain::equity_swap_instrument>
equity_swap_instrument_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<equity_swap_instrument_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("instrument_id"_c);
        return execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
            ctx,
            query,
            [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
            lg(),
            "Reading latest equity swap instruments (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<equity_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c);

    return execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
        ctx,
        query,
        [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
        lg(),
        "Reading latest equity swap instruments");
}

std::vector<domain::equity_swap_instrument>
equity_swap_instrument_repository::read_latest(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest equity swap instrument. "
                               << "instrument_id: " << instrument_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<equity_swap_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    return execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
        ctx,
        query,
        [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
        lg(),
        "Reading latest equity swap instrument by instrument_id.");
}


std::vector<domain::equity_swap_instrument>
equity_swap_instrument_repository::read_all(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all equity swap instrument versions. "
                               << "instrument_id: " << instrument_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<equity_swap_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c == instrument_id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
        ctx,
        query,
        [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
        lg(),
        "Reading all equity swap instrument versions by instrument_id.");
}

std::optional<domain::equity_swap_instrument> equity_swap_instrument_repository::read_at_version(
    context ctx, const std::string& instrument_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading equity swap instrument at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<equity_swap_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c == instrument_id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
            ctx,
            query,
            [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
            lg(),
            "Reading equity swap instrument at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void equity_swap_instrument_repository::remove(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing equity swap instrument. "
                               << "instrument_id: " << instrument_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<equity_swap_instrument_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing equity swap instrument from database.");
}

std::vector<domain::equity_swap_instrument> equity_swap_instrument_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest equity swap instruments with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<equity_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
        ctx,
        query,
        [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
        lg(),
        "Reading latest equity swap instruments with pagination.");
}

std::uint32_t
equity_swap_instrument_repository::get_total_equity_swap_instrument_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active equity swap instrument count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<equity_swap_instrument_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active equity swap instrument count: " << count;
    return count;
}

std::vector<domain::equity_swap_instrument>
equity_swap_instrument_repository::read_latest(context ctx,
                                               const std::vector<std::string>& instrument_ids) {
    if (instrument_ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<equity_swap_instrument_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c.in(instrument_ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<equity_swap_instrument_entity, domain::equity_swap_instrument>(
        ctx,
        query,
        [](const auto& entities) { return equity_swap_instrument_mapper::map(entities); },
        lg(),
        "Reading latest equity swap instruments by ids.");
    return result;
}

void equity_swap_instrument_repository::remove(context ctx,
                                               const std::vector<std::string>& instrument_ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<equity_swap_instrument_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "instrument_id"_c.in(instrument_ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing equity swap instruments.");
}


}
