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
#include "ores.trading.core/repository/bond_future_delivery_basket_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/bond_future_delivery_basket_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/bond_future_delivery_basket_entity.hpp"
#include "ores.trading.core/repository/bond_future_delivery_basket_mapper.hpp"
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string bond_future_delivery_basket_repository::sql() {
    return generate_create_table_sql<bond_future_delivery_basket_entity>(lg());
}

void bond_future_delivery_basket_repository::write(context ctx,
                                                   const domain::bond_future_delivery_basket& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond future delivery basket identifier. "
                               << "instrument_id: " << v.instrument_id
                               << " sequence_number: " << v.sequence_number;
    execute_write_query(ctx,
                        bond_future_delivery_basket_mapper::map(v),
                        lg(),
                        "Writing bond future delivery basket identifier to database.");
}

void bond_future_delivery_basket_repository::write(
    context ctx, const std::vector<domain::bond_future_delivery_basket>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond future delivery basket identifiers. Count: "
                               << v.size();
    execute_write_query(ctx,
                        bond_future_delivery_basket_mapper::map(v),
                        lg(),
                        "Writing bond future delivery basket identifiers to database.");
}

std::vector<domain::bond_future_delivery_basket>
bond_future_delivery_basket_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c);

    return execute_read_query<bond_future_delivery_basket_entity,
                              domain::bond_future_delivery_basket>(
        ctx,
        query,
        [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
        lg(),
        "Reading latest bond future delivery basket identifiers");
}

std::vector<domain::bond_future_delivery_basket>
bond_future_delivery_basket_repository::read_latest(context ctx,
                                                    const std::string& instrument_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond future delivery basket identifier. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    return execute_read_query<bond_future_delivery_basket_entity,
                              domain::bond_future_delivery_basket>(
        ctx,
        query,
        [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
        lg(),
        "Reading latest bond future delivery basket identifier by instrument_id.");
}


std::vector<domain::bond_future_delivery_basket> bond_future_delivery_basket_repository::read_all(
    context ctx, const std::string& instrument_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all bond future delivery basket identifier versions. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "sequence_number"_c == sequence_number) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<bond_future_delivery_basket_entity,
                              domain::bond_future_delivery_basket>(
        ctx,
        query,
        [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
        lg(),
        "Reading all bond future delivery basket identifier versions by instrument_id.");
}

std::optional<domain::bond_future_delivery_basket>
bond_future_delivery_basket_repository::read_at_version(context ctx,
                                                        const std::string& instrument_id,
                                                        const std::string& sequence_number,
                                                        std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading bond future delivery basket identifier at version. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "sequence_number"_c == sequence_number && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<bond_future_delivery_basket_entity, domain::bond_future_delivery_basket>(
            ctx,
            query,
            [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
            lg(),
            "Reading bond future delivery basket identifier at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void bond_future_delivery_basket_repository::remove(context ctx,
                                                    const std::string& instrument_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond future delivery basket identifier. "
                               << "instrument_id: " << instrument_id
                               << " sequence_number: " << sequence_number;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<bond_future_delivery_basket_entity> |
                       where("tenant_id"_c == tid && "instrument_id"_c == instrument_id &&
                             "sequence_number"_c == sequence_number && "valid_to"_c == max.value());

    execute_delete_query(
        ctx, query, lg(), "Removing bond future delivery basket identifier from database.");
}

std::vector<domain::bond_future_delivery_basket>
bond_future_delivery_basket_repository::read_latest(context ctx,
                                                    std::uint32_t offset,
                                                    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest bond future delivery basket identifiers with offset: " << offset
        << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_future_delivery_basket_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("instrument_id"_c, "sequence_number"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<bond_future_delivery_basket_entity,
                              domain::bond_future_delivery_basket>(
        ctx,
        query,
        [](const auto& entities) { return bond_future_delivery_basket_mapper::map(entities); },
        lg(),
        "Reading latest bond future delivery basket identifiers with pagination.");
}

std::uint32_t
bond_future_delivery_basket_repository::get_total_delivery_basket_id_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active bond future delivery basket identifier count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<bond_future_delivery_basket_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active bond future delivery basket identifier count: "
                               << count;
    return count;
}

void bond_future_delivery_basket_repository::remove(
    context ctx,
    const std::vector<std::string>& instrument_ids,
    const std::vector<std::string>& sequence_numbers) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (sequence_numbers.size() != instrument_ids.size())
        throw std::invalid_argument("bond_future_delivery_basket_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < instrument_ids.size(); ++i)
        remove(ctx, instrument_ids[i], sequence_numbers[i]);
}


}
