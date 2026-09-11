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
#include "ores.trading.core/repository/trade_envelope_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/trade_envelope_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_envelope_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_envelope_repository::sql() {
    return generate_create_table_sql<trade_envelope_entity>(lg());
}

void trade_envelope_repository::write(context ctx, const domain::trade_envelope& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade envelope. " << "trade_id: " << v.trade_id;
    execute_write_query(
        ctx, trade_envelope_mapper::map(v), lg(), "Writing trade envelope to database.");
}

void trade_envelope_repository::write(context ctx, const std::vector<domain::trade_envelope>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade envelopes. Count: " << v.size();
    execute_write_query(
        ctx, trade_envelope_mapper::map(v), lg(), "Writing trade envelopes to database.");
}

std::vector<domain::trade_envelope> trade_envelope_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c);

    return execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading latest trade envelopes");
}

std::vector<domain::trade_envelope>
trade_envelope_repository::read_latest(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade envelope. " << "trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_envelope_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "valid_to"_c == max.value());

    return execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading latest trade envelope by trade_id.");
}


std::vector<domain::trade_envelope>
trade_envelope_repository::read_all(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade envelope versions. "
                               << "trade_id: " << trade_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading all trade envelope versions by trade_id.");
}

std::optional<domain::trade_envelope> trade_envelope_repository::read_at_version(
    context ctx, const std::string& trade_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade envelope at version. " << "trade_id: " << trade_id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<trade_envelope_entity>> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading trade envelope at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void trade_envelope_repository::remove(context ctx, const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade envelope. " << "trade_id: " << trade_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<trade_envelope_entity> |
        where("tenant_id"_c == tid && "trade_id"_c == trade_id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing trade envelope from database.");
}

std::vector<domain::trade_envelope>
trade_envelope_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade envelopes with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<trade_envelope_entity, domain::trade_envelope>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_mapper::map(entities); },
        lg(),
        "Reading latest trade envelopes with pagination.");
}

std::uint32_t trade_envelope_repository::get_total_trade_envelope_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active trade envelope count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<trade_envelope_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active trade envelope count: " << count;
    return count;
}

void trade_envelope_repository::remove(context ctx, const std::vector<std::string>& trade_ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<trade_envelope_entity> |
        where("tenant_id"_c == tid && "trade_id"_c.in(trade_ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing trade envelopes.");
}


}
