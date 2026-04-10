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
#include "ores.trading.core/repository/equity_variance_swap_instrument_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/equity_variance_swap_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/equity_variance_swap_instrument_entity.hpp"
#include "ores.trading.core/repository/equity_variance_swap_instrument_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string equity_variance_swap_instrument_repository::sql() {
    return generate_create_table_sql<equity_variance_swap_instrument_entity>(lg());
}

void equity_variance_swap_instrument_repository::write(
    context ctx, const domain::equity_variance_swap_instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity variance swap instrument: " << v.instrument_id;
    execute_write_query(ctx, equity_variance_swap_instrument_mapper::map(v),
        lg(), "Writing equity variance swap instrument to database.");
}

void equity_variance_swap_instrument_repository::write(
    context ctx, const std::vector<domain::equity_variance_swap_instrument>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing equity variance swap instruments. Count: " << v.size();
    execute_write_query(ctx, equity_variance_swap_instrument_mapper::map(v),
        lg(), "Writing equity variance swap instruments to database.");
}

std::vector<domain::equity_variance_swap_instrument>
equity_variance_swap_instrument_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_variance_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c);

    return execute_read_query<equity_variance_swap_instrument_entity, domain::equity_variance_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return equity_variance_swap_instrument_mapper::map(entities); },
        lg(), "Reading latest equity variance swap instruments");
}

std::vector<domain::equity_variance_swap_instrument>
equity_variance_swap_instrument_repository::read_latest(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest equity variance swap instrument. instrument_id: " << instrument_id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_variance_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    return execute_read_query<equity_variance_swap_instrument_entity, domain::equity_variance_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return equity_variance_swap_instrument_mapper::map(entities); },
        lg(), "Reading latest equity variance swap instrument by instrument_id.");
}

std::vector<domain::equity_variance_swap_instrument>
equity_variance_swap_instrument_repository::read_all(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all equity variance swap instrument versions. instrument_id: " << instrument_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<equity_variance_swap_instrument_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id) |
        order_by("version"_c.desc());

    return execute_read_query<equity_variance_swap_instrument_entity, domain::equity_variance_swap_instrument>(
        ctx, query,
        [](const auto& entities) { return equity_variance_swap_instrument_mapper::map(entities); },
        lg(), "Reading all equity variance swap instrument versions by instrument_id.");
}

void equity_variance_swap_instrument_repository::remove(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing equity variance swap instrument: " << instrument_id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<equity_variance_swap_instrument_entity> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing equity variance swap instrument from database.");
}

}
