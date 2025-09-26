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
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.core/risk/db/currency_table.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.core.risk.db.currency_table"));

}

namespace ores::core::risk::db {

using types::currency;
using namespace sqlgen;
using namespace sqlgen::literals;


void currency_table::
write(sqlgen_connection c, const std::vector<currency>& currencies) {
    BOOST_LOG_SEV(lg, debug) << "HERE";
    const auto create_query = create_table<currency> |
        if_not_exists;
    const auto sql = postgres::to_sql(create_query);
    BOOST_LOG_SEV(lg, debug) << sql;

    const auto result = begin_transaction(c)
                        .and_then(create_table<currency> | if_not_exists)
                        .and_then(insert(std::ref(currencies)))
                        .and_then(commit);
    if (!result)
        BOOST_LOG_SEV(lg, debug) << "Error: " << result.error().what();

}

std::vector<currency>
currency_table::read_latest(sqlgen_connection c,
    const std::string& iso_code) {
    const auto query = sqlgen::read<std::vector<currency>> |
        where("iso_code"_c == iso_code /*&& "valid_to"_c == max("valid_to"_c)*/) |
        order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (result)
        BOOST_LOG_SEV(lg, debug) << rfl::json::write(*result);
    else
        BOOST_LOG_SEV(lg, debug) << "Error: " << result.error().what();

    return *result;
}

std::vector<currency>
currency_table::read_at_timepoint(sqlgen_connection c,
    const std::string& as_of,
    const std::string& iso_code) {

    const auto ts = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S%z">::from_string(as_of);
    const auto query = sqlgen::read<std::vector<currency>> |
        where("iso_code"_c == iso_code/* &&
                                        "valid_from"_c <= ts && "valid_to"_c >= ts*/);

    const auto result = query(c);
    if (result)
        BOOST_LOG_SEV(lg, debug) << rfl::json::write(*result);
    else
        BOOST_LOG_SEV(lg, debug) << "Error: " << result.error().what();

    return *result;
}

std::vector<currency> currency_table::
read_all(sqlgen_connection c) {
    const auto query = sqlgen::read<std::vector<currency>> |
        order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (result)
        BOOST_LOG_SEV(lg, debug) << rfl::json::write(*result);
    else
        BOOST_LOG_SEV(lg, debug) << "Error: " << result.error().what();

    return *result;
}

std::vector<currency> currency_table::
read_all(sqlgen_connection c, const std::string& iso_code) {
    const auto query = sqlgen::read<std::vector<currency>> |
                where("iso_code"_c == iso_code) |
                order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (result)
        BOOST_LOG_SEV(lg, debug) << rfl::json::write(*result);
    else
        BOOST_LOG_SEV(lg, debug) << "Error: " << result.error().what();

    return *result;
}

}
