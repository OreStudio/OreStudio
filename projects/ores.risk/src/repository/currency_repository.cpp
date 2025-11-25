/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.risk/repository/currency_repository.hpp"

#include <format>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.risk/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.risk/repository/currency_mapper.hpp"
#include "ores.risk/repository/currency_entity.hpp"

namespace ores::risk::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
using namespace ores::utility::repository;

std::string currency_repository::sql() {
    return generate_create_table_sql<currency_entity>(
        "ores.risk.repository.currency_repository");
}

void currency_repository::
write(context ctx, const domain::currency& currency) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency to database: "
                               << currency;

    execute_write_query(ctx,
        currency_mapper::map(currency),
        "ores.risk.repository.currency_repository",
        "writing currency to database");
}

void currency_repository::
write(context ctx, const std::vector<domain::currency>& currencies) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currencies to database. Count: "
                             << currencies.size();

    execute_write_query(ctx,
        currency_mapper::map(currencies),
        "ores.risk.repository.currency_repository",
        "writing currencies to database");
}


std::vector<domain::currency> currency_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto sql = postgres::to_sql(query);
    BOOST_LOG_SEV(lg(), debug) << "Query: " << sql;

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading latest currencies");
}

std::vector<domain::currency>
currency_repository::read_latest(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currencies. ISO code: "
                             << iso_code;

    static auto max(make_timestamp(MAX_TIMESTAMP));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading latest currencies by ISO code");
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg(), debug) << "Reading currencies at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of);
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading currencies at timepoint");
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of,
    const std::string& iso_code) {

    const auto ts = make_timestamp(as_of);
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading currencies at timepoint by ISO code");
}

std::vector<domain::currency> currency_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading all currencies");
}

std::vector<domain::currency>
currency_repository::read_all(context ctx, const std::string& iso_code) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code) |
        order_by("valid_from"_c.desc());

    return execute_read_query<currency_entity, domain::currency>(ctx, query,
        [](const auto& entities) { return currency_mapper::map(entities); },
        "ores.risk.repository.currency_repository",
        "Reading all currencies by ISO code");
}

void currency_repository::remove(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency from database: " << iso_code;

    // Delete the currency - the database trigger will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto query = sqlgen::delete_from<currency_entity> |
        where("iso_code"_c == iso_code);

    execute_delete_query(ctx, query,
        "ores.risk.repository.currency_repository",
        "removing currency from database");
}

}
