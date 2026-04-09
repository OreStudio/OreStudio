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
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string balance_guaranteed_swap_instrument_repository::sql() {
    return generate_create_table_sql<balance_guaranteed_swap_instrument_entity>(lg());
}

void balance_guaranteed_swap_instrument_repository::write(context ctx, const domain::balance_guaranteed_swap_instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing balance guaranteed swap instrument: " << v.instrument_id;
    execute_write_query(ctx, balance_guaranteed_swap_instrument_mapper::map(v),
        lg(), "Writing balance guaranteed swap instrument to database.");
}

void balance_guaranteed_swap_instrument_repository::write(
    context ctx, const std::vector<domain::balance_guaranteed_swap_instrument>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing balance guaranteed swap instruments. Count: " << v.size();
    execute_write_query(ctx, balance_guaranteed_swap_instrument_mapper::map(v),
        lg(), "Writing balance guaranteed swap instruments to database.");
}

std::vector<domain::balance_guaranteed_swap_instrument>
balance_guaranteed_swap_instrument_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<balance_guaranteed_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c);

    return execute_read_query<balance_guaranteed_swap_instrument_entity, domain::balance_guaranteed_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return balance_guaranteed_swap_instrument_mapper::map(entities); },
        lg(), "Reading latest balance guaranteed swap instruments");
}

std::vector<domain::balance_guaranteed_swap_instrument>
balance_guaranteed_swap_instrument_repository::read_latest(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest balance guaranteed swap instrument. instrument_id: " << instrument_id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<balance_guaranteed_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    return execute_read_query<balance_guaranteed_swap_instrument_entity, domain::balance_guaranteed_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return balance_guaranteed_swap_instrument_mapper::map(entities); },
        lg(), "Reading latest balance guaranteed swap instrument by instrument_id.");
}

std::vector<domain::balance_guaranteed_swap_instrument>
balance_guaranteed_swap_instrument_repository::read_all(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all balance guaranteed swap instrument versions. instrument_id: " << instrument_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<balance_guaranteed_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id) |
        order_by("version"_c.desc());

    return execute_read_query<balance_guaranteed_swap_instrument_entity, domain::balance_guaranteed_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return balance_guaranteed_swap_instrument_mapper::map(entities); },
        lg(), "Reading all balance guaranteed swap instrument versions by instrument_id.");
}

void balance_guaranteed_swap_instrument_repository::remove(context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing balance guaranteed swap instrument: " << instrument_id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<balance_guaranteed_swap_instrument_entity> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing balance guaranteed swap instrument from database.");
}

}
