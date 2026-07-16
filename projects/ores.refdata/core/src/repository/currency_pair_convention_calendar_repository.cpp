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
#include "ores.refdata.core/repository/currency_pair_convention_calendar_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_calendar_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_pair_convention_calendar_entity.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_calendar_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_pair_convention_calendar_repository::sql() {
    return generate_create_table_sql<currency_pair_convention_calendar_entity>(lg());
}

currency_pair_convention_calendar_repository::currency_pair_convention_calendar_repository(
    context ctx)
    : ctx_(std::move(ctx)) {}

void currency_pair_convention_calendar_repository::write(
    const domain::currency_pair_convention_calendar& pair_convention_calendar) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair convention calendar to database: "
                               << pair_convention_calendar.pair_code << "/"
                               << pair_convention_calendar.calendar_code;
    execute_write_query(ctx_,
                        currency_pair_convention_calendar_mapper::map(pair_convention_calendar),
                        lg(),
                        "writing currency pair convention calendar to database");
}

void currency_pair_convention_calendar_repository::write(
    const std::vector<domain::currency_pair_convention_calendar>& pair_convention_calendars) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair convention calendars to database. Count: "
                               << pair_convention_calendars.size();
    execute_write_query(ctx_,
                        currency_pair_convention_calendar_mapper::map(pair_convention_calendars),
                        lg(),
                        "writing currency pair convention calendars to database");
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_calendar_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("pair_code"_c, "calendar_code"_c);

    return execute_read_query<currency_pair_convention_calendar_entity,
                              domain::currency_pair_convention_calendar>(
        ctx_,
        query,
        [](const auto& entities) {
            return currency_pair_convention_calendar_mapper::map(entities);
        },
        lg(),
        "Reading latest currency pair convention calendars");
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_repository::read_latest_by_pair(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair convention calendars. Pair: "
                               << pair_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_pair_convention_calendar_entity>> |
        where("tenant_id"_c == tid && "pair_code"_c == pair_code && "valid_to"_c == max.value()) |
        order_by("calendar_code"_c);

    return execute_read_query<currency_pair_convention_calendar_entity,
                              domain::currency_pair_convention_calendar>(
        ctx_,
        query,
        [](const auto& entities) {
            return currency_pair_convention_calendar_mapper::map(entities);
        },
        lg(),
        "Reading latest currency pair convention calendars by pair.");
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_repository::read_latest_by_calendar(
    const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair convention calendars. Calendar: "
                               << calendar_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_calendar_entity>> |
                       where("tenant_id"_c == tid && "calendar_code"_c == calendar_code &&
                             "valid_to"_c == max.value()) |
                       order_by("pair_code"_c);

    return execute_read_query<currency_pair_convention_calendar_entity,
                              domain::currency_pair_convention_calendar>(
        ctx_,
        query,
        [](const auto& entities) {
            return currency_pair_convention_calendar_mapper::map(entities);
        },
        lg(),
        "Reading latest currency pair convention calendars by calendar.");
}

void currency_pair_convention_calendar_repository::remove(const std::string& pair_code,
                                                          const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair convention calendar from database: "
                               << pair_code << "/" << calendar_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_pair_convention_calendar_entity> |
                       where("tenant_id"_c == tid && "pair_code"_c == pair_code &&
                             "calendar_code"_c == calendar_code);

    execute_delete_query(
        ctx_, query, lg(), "removing currency pair convention calendar from database");
}

void currency_pair_convention_calendar_repository::remove_by_pair(const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all currency pair convention calendars from database: "
                               << pair_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<currency_pair_convention_calendar_entity> |
                       where("tenant_id"_c == tid && "pair_code"_c == pair_code);

    execute_delete_query(
        ctx_, query, lg(), "removing all currency pair convention calendars from database");
}

}
