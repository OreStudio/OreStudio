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
#include "ores.trading.core/repository/swap_leg_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/swap_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/swap_leg_entity.hpp"
#include "ores.trading.core/repository/swap_leg_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string swap_leg_repository::sql() {
    return generate_create_table_sql<swap_leg_entity>(lg());
}

void swap_leg_repository::write(context ctx, const domain::swap_leg& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing swap leg: " << v.id;
    execute_write_query(ctx, swap_leg_mapper::map(v),
        lg(), "Writing swap leg to database.");
}

void swap_leg_repository::write(
    context ctx, const std::vector<domain::swap_leg>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing swap legs. Count: " << v.size();
    execute_write_query(ctx, swap_leg_mapper::map(v),
        lg(), "Writing swap legs to database.");
}

std::vector<domain::swap_leg>
swap_leg_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c, "leg_number"_c);

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx, query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(), "Reading latest swap legs");
}

std::vector<domain::swap_leg>
swap_leg_repository::read_by_instrument(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading swap legs by instrument. instrument_id: " << instrument_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id
              && "valid_to"_c == max.value()) |
        order_by("leg_number"_c);

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx, query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(), "Reading swap legs by instrument.");
}

std::vector<domain::swap_leg>
swap_leg_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all swap leg versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<swap_leg_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<swap_leg_entity, domain::swap_leg>(
        ctx, query,
        [](const auto& entities) { return swap_leg_mapper::map(entities); },
        lg(), "Reading all swap leg versions by id.");
}

std::uint32_t swap_leg_repository::count_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Counting latest swap legs";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<swap_leg_entity>(
        sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active swap leg count: " << count;
    return count;
}

void swap_leg_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing swap leg: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<swap_leg_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing swap leg from database.");
}

void swap_leg_repository::remove_by_instrument(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing swap legs for instrument: " << instrument_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<swap_leg_entity> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id
              && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing swap legs for instrument from database.");
}

}
