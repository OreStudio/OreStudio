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
#include "ores.trading.core/repository/trade_envelope_portfolio_id_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/trade_envelope_portfolio_id_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/trade_envelope_portfolio_id_entity.hpp"
#include "ores.trading.core/repository/trade_envelope_portfolio_id_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_envelope_portfolio_id_repository::sql() {
    return generate_create_table_sql<trade_envelope_portfolio_id_entity>(lg());
}

void trade_envelope_portfolio_id_repository::write(context ctx,
                                                   const domain::trade_envelope_portfolio_id& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade envelope portfolio identifier. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    execute_write_query(ctx,
                        trade_envelope_portfolio_id_mapper::map(v),
                        lg(),
                        "Writing trade envelope portfolio identifier to database.");
}

void trade_envelope_portfolio_id_repository::write(
    context ctx, const std::vector<domain::trade_envelope_portfolio_id>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade envelope portfolio identifiers. Count: "
                               << v.size();
    execute_write_query(ctx,
                        trade_envelope_portfolio_id_mapper::map(v),
                        lg(),
                        "Writing trade envelope portfolio identifiers to database.");
}

std::vector<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c);

    return execute_read_query<trade_envelope_portfolio_id_entity,
                              domain::trade_envelope_portfolio_id>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
        lg(),
        "Reading latest trade envelope portfolio identifiers");
}

std::vector<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_repository::read_latest(context ctx,
                                                    const std::string& trade_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade envelope portfolio identifier. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<trade_envelope_portfolio_id_entity,
                              domain::trade_envelope_portfolio_id>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
        lg(),
        "Reading latest trade envelope portfolio identifier by trade_id.");
}


std::vector<domain::trade_envelope_portfolio_id> trade_envelope_portfolio_id_repository::read_all(
    context ctx, const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade envelope portfolio identifier versions. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<trade_envelope_portfolio_id_entity,
                              domain::trade_envelope_portfolio_id>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
        lg(),
        "Reading all trade envelope portfolio identifier versions by trade_id.");
}

std::optional<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_repository::read_at_version(context ctx,
                                                        const std::string& trade_id,
                                                        const std::string& sequence_number,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading trade envelope portfolio identifier at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<trade_envelope_portfolio_id_entity, domain::trade_envelope_portfolio_id>(
            ctx,
            query,
            [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
            lg(),
            "Reading trade envelope portfolio identifier at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void trade_envelope_portfolio_id_repository::remove(context ctx,
                                                    const std::string& trade_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade envelope portfolio identifier. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<trade_envelope_portfolio_id_entity> |
                       where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    execute_delete_query(
        ctx, query, lg(), "Removing trade envelope portfolio identifier from database.");
}

std::vector<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_repository::read_latest(context ctx,
                                                    std::uint32_t offset,
                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest trade envelope portfolio identifiers with offset: " << offset
        << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_envelope_portfolio_id_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("trade_id"_c, "sequence_number"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<trade_envelope_portfolio_id_entity,
                              domain::trade_envelope_portfolio_id>(
        ctx,
        query,
        [](const auto& entities) { return trade_envelope_portfolio_id_mapper::map(entities); },
        lg(),
        "Reading latest trade envelope portfolio identifiers with pagination.");
}

std::uint32_t
trade_envelope_portfolio_id_repository::get_total_trade_envelope_portfolio_id_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active trade envelope portfolio identifier count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<trade_envelope_portfolio_id_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active trade envelope portfolio identifier count: "
                               << count;
    return count;
}

void trade_envelope_portfolio_id_repository::remove(
    context ctx,
    const std::vector<std::string>& trade_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != trade_ids.size())
        throw std::invalid_argument("trade_envelope_portfolio_id_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < trade_ids.size(); ++i)
        remove(ctx, trade_ids[i], sequence_numbers[i]);
}


}
