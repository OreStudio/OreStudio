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
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.utility/repository/repository_exception.hpp"
#include "ores.accounts/repository/account_mapper.hpp"
#include "ores.accounts/repository/account_entity.hpp"
#include "ores.accounts/repository/account_repository.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.repository.account_repository"));
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

namespace ores::accounts::repository {

using namespace sqlgen;
using namespace sqlgen::literals;

std::string account_repository::sql() {
    const auto query = create_table<account_entity> | if_not_exists;
    const auto sql = postgres::to_sql(query);

    BOOST_LOG_SEV(lg, debug) << sql;
    return sql;
}

void account_repository::
write(context ctx, const std::vector<domain::account>& accounts) {
    BOOST_LOG_SEV(lg, debug) << "Writing accounts to database. Count: "
                             << accounts.size();

    const auto r = session(ctx.connection_pool())
        .and_then(begin_transaction)
        .and_then(insert(account_mapper::map(accounts)))
        .and_then(commit);
    ensure_success(r);

    BOOST_LOG_SEV(lg, debug) << "Finished writing accounts to database.";
}

std::vector<domain::account> account_repository::read_latest(context ctx) {
    BOOST_LOG_SEV(lg, debug) << "Reading latest accounts.";

    static auto max(make_timestamp(max_timestamp));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest accounts. Total: " << r->size();
    return account_mapper::map(*r);
}

std::vector<domain::account>
account_repository::read_latest(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg, debug) << "Reading latest accounts. ID: " << id;

    static auto max(make_timestamp(max_timestamp));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest accounts. Total: " << r->size();
    return account_mapper::map(*r);
}

std::vector<domain::account> account_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<account_entity>> |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool())
        .and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read all accounts. Total: " << r->size();
    return account_mapper::map(*r);
}

std::vector<domain::account>
account_repository::read_all(context ctx, const boost::uuids::uuid& id) {
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("id"_c == id_str) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read all accounts. Total: " << r->size();
    return account_mapper::map(*r);
}

std::vector<domain::account>
account_repository::read_latest_by_username(context ctx, const std::string& username) {
    BOOST_LOG_SEV(lg, debug) << "Reading latest account by username: " << username;

    static auto max(make_timestamp(max_timestamp));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("username"_c == username && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto r = session(ctx.connection_pool()).and_then(query);
    ensure_success(r);
    BOOST_LOG_SEV(lg, debug) << "Read latest account by username. Total: " << r->size();
    return account_mapper::map(*r);
}

}
