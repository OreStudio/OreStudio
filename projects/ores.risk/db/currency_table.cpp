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
#include "ores.risk/db/currency_table.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.risk.db.currency_table"));

}

namespace ores::risk::db {

using domain::currency;
using namespace sqlgen;
using namespace sqlgen::literals;

std::string currency_table::table_sql() {
    const auto query = create_table<currency> |
        if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg, debug) << sql;
    return sql;
}

void currency_table::
write(sqlgen_connection c, const std::vector<currency>& currencies) {
    const auto result = begin_transaction(c)
        .and_then(insert(std::ref(currencies)))
        .and_then(commit);

    if (!result)
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();
}

std::vector<currency>
currency_table::read_latest(sqlgen_connection c, const std::string& iso_code) {

    const auto max = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string("9999-12-31 23:59:59");
    if (!max) {
        BOOST_LOG_SEV(lg, debug) << "Error: " << max.error().what();
    }

    if (iso_code.empty()) {
        const auto query = sqlgen::read<std::vector<currency>> |
            where("valid_to"_c == max.value()) |
            order_by("valid_from"_c.desc());

        const auto result = query(c);
        if (!result)
            BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

        return *result;
    }

    const auto query = sqlgen::read<std::vector<currency>> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (!result)
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

    return *result;
}

std::vector<currency>
currency_table::read_at_timepoint(sqlgen_connection c,
    const std::string& as_of, const std::string& iso_code) {

    const auto ts = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(as_of);
    if (!ts)
        BOOST_LOG_SEV(lg, severity_level::error) << ts.error().what();

    if (iso_code.empty()) {
        const auto query = sqlgen::read<std::vector<currency>> |
            where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

        const auto result = query(c);
        if (!result)
            BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

        return *result;
    }

    const auto query = sqlgen::read<std::vector<currency>> |
        where("iso_code"_c == iso_code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    const auto result = query(c);
    if (!result)
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

    return *result;
}

std::vector<currency> currency_table::read_all(sqlgen_connection c) {
    const auto query = sqlgen::read<std::vector<currency>> |
        order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (!result)
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

    return *result;
}

std::vector<currency>
currency_table::read_all(sqlgen_connection c, const std::string& iso_code) {
    if (iso_code.empty()) {
        const auto query = sqlgen::read<std::vector<currency>> |
            order_by("valid_from"_c.desc());

        const auto result = query(c);
        if (!result)
            BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

        return *result;
    }

    const auto query = sqlgen::read<std::vector<currency>> |
        where("iso_code"_c == iso_code) |
        order_by("valid_from"_c.desc());

    const auto result = query(c);
    if (!result)
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();

    return *result;
}

}
