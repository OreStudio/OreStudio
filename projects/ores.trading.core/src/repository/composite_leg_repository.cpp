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
#include "ores.trading.core/repository/composite_leg_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/composite_leg_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/composite_leg_entity.hpp"
#include "ores.trading.core/repository/composite_leg_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string composite_leg_repository::sql() {
    return generate_create_table_sql<composite_leg_entity>(lg());
}

void composite_leg_repository::write(context ctx, const domain::composite_leg& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing composite leg: " << v.id;
    execute_write_query(ctx, composite_leg_mapper::map(v),
        lg(), "Writing composite leg to database.");
}

void composite_leg_repository::write(
    context ctx, const std::vector<domain::composite_leg>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing composite legs. Count: " << v.size();
    execute_write_query(ctx, composite_leg_mapper::map(v),
        lg(), "Writing composite legs to database.");
}

std::vector<domain::composite_leg>
composite_leg_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("instrument_id"_c, "leg_sequence"_c);

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx, query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(), "Reading latest composite legs");
}

std::vector<domain::composite_leg>
composite_leg_repository::read_by_instrument(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading composite legs by instrument. instrument_id: " << instrument_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id
              && "valid_to"_c == max.value()) |
        order_by("leg_sequence"_c);

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx, query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(), "Reading composite legs by instrument.");
}

std::vector<domain::composite_leg>
composite_leg_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all composite leg versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<composite_leg_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<composite_leg_entity, domain::composite_leg>(
        ctx, query,
        [](const auto& entities) { return composite_leg_mapper::map(entities); },
        lg(), "Reading all composite leg versions by id.");
}

std::uint32_t composite_leg_repository::count_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Counting latest composite legs";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<composite_leg_entity>(
        sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active composite leg count: " << count;
    return count;
}

void composite_leg_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing composite leg: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<composite_leg_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing composite leg from database.");
}

void composite_leg_repository::remove_by_instrument(
    context ctx, const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Removing composite legs for instrument: " << instrument_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<composite_leg_entity> |
        where("tenant_id"_c == tid && "instrument_id"_c == instrument_id
              && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing composite legs for instrument from database.");
}

}
