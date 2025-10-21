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
#include <format>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.risk/repository/currency_mapper.hpp"
#include "ores.risk/repository/currency_entity.hpp"
#include "ores.risk/repository/currency_repository.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.risk.repository.currency_repository"));
const std::string max_timestamp("9999-12-31 23:59:59");
using ores::utility::repository::repository_exception;

void ensure_success(const auto result) {
    if (!result) {
        BOOST_LOG_SEV(lg, severity_level::error) << result.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(std::format("Repository error: {}",
                    result.error().what())));
    }
}

auto make_timestamp(const std::string& s) {
    const auto r = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">::from_string(s);
    if (!r) {
        BOOST_LOG_SEV(lg, error) << "Error converting timestamp: '" << s
                                 << "'. Error: " << r.error().what();
        BOOST_THROW_EXCEPTION(
            repository_exception(
                std::format("Timestamp conversion error: {}", s)));
    }
    return r;
}

}

namespace ores::risk::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

std::string currency_repository::sql() {
    const auto query = create_table<currency_entity> | if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg, debug) << sql;
    return sql;
}

void currency_repository::
write(context ctx, const std::vector<domain::currency>& currencies) {
    BOOST_LOG_SEV(lg, debug) << "Writing currencies to database. Count: "
                             << currencies.size();

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(currency_mapper::map(currencies)))
        .and_then(commit);
    ensure_success(r);

    BOOST_LOG_SEV(lg, debug) << "Finished writing currencies to database.";
}

std::vector<domain::currency> currency_repository::read_latest(context ctx) {
    BOOST_LOG_SEV(lg, debug) << "Reading latest currencies.";

    static auto max(make_timestamp(max_timestamp));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

std::vector<domain::currency>
currency_repository::read_latest(context ctx, const std::string& iso_code) {
    BOOST_LOG_SEV(lg, debug) << "Reading latest currencies. ISO code: "
                             << iso_code;

    static auto max(make_timestamp(max_timestamp));
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of) {
    BOOST_LOG_SEV(lg, debug) << "Reading currencies at timepoint: " << as_of;

    const auto ts = make_timestamp(as_of);
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

std::vector<domain::currency>
currency_repository::read_at_timepoint(context ctx, const std::string& as_of,
    const std::string& iso_code) {

    const auto ts = make_timestamp(as_of);
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code &&
            "valid_from"_c <= ts.value() && "valid_to"_c >= ts.value());

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

std::vector<domain::currency> currency_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

std::vector<domain::currency>
currency_repository::read_all(context ctx, const std::string& iso_code) {
    const auto query = sqlgen::read<std::vector<currency_entity>> |
        where("iso_code"_c == iso_code) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest currencies. Total: " << r->size();
    return currency_mapper::map(*r);
}

}
