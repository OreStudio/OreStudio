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
#include "ores.trading.core/repository/credit_instrument_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/credit_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/credit_instrument_entity.hpp"
#include "ores.trading.core/repository/credit_instrument_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string credit_instrument_repository::sql() {
    return generate_create_table_sql<credit_instrument_entity>(lg());
}

void credit_instrument_repository::write(context ctx,
    const domain::credit_instrument& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing credit_instrument: " << v.id;
    execute_write_query(ctx, credit_instrument_mapper::map(v),
        lg(), "Writing credit_instrument to database.");
}

void credit_instrument_repository::write(
    context ctx, const std::vector<domain::credit_instrument>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing credit_instruments. Count: " << v.size();
    execute_write_query(ctx, credit_instrument_mapper::map(v),
        lg(), "Writing credit_instruments to database.");
}

std::vector<domain::credit_instrument>
credit_instrument_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_instrument_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<credit_instrument_entity, domain::credit_instrument>(
        ctx, query,
        [](const auto& entities) { return credit_instrument_mapper::map(entities); },
        lg(), "Reading latest credit_instruments");
}

std::vector<domain::credit_instrument>
credit_instrument_repository::read_latest(context ctx, std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest credit_instruments with offset: "
        << offset << " and limit: " << limit;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_instrument_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<credit_instrument_entity, domain::credit_instrument>(
        ctx, query,
        [](const auto& entities) { return credit_instrument_mapper::map(entities); },
        lg(), "Reading latest credit_instruments with pagination");
}

std::uint32_t credit_instrument_repository::count_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Counting latest credit_instruments";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<credit_instrument_entity>(
        sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active credit_instrument count: " << count;
    return count;
}

std::vector<domain::credit_instrument>
credit_instrument_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest credit_instrument. id: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_instrument_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<credit_instrument_entity, domain::credit_instrument>(
        ctx, query,
        [](const auto& entities) { return credit_instrument_mapper::map(entities); },
        lg(), "Reading latest credit_instrument by id.");
}

std::vector<domain::credit_instrument>
credit_instrument_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading all credit_instrument versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<credit_instrument_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<credit_instrument_entity, domain::credit_instrument>(
        ctx, query,
        [](const auto& entities) { return credit_instrument_mapper::map(entities); },
        lg(), "Reading all credit_instrument versions by id.");
}

void credit_instrument_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing credit_instrument: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<credit_instrument_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing credit_instrument from database.");
}

}
