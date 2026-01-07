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
#include "ores.iam/repository/account_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/account_entity.hpp"
#include "ores.iam/repository/account_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string account_repository::sql() {
    return generate_create_table_sql<account_entity>(lg());
}

account_repository::account_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void account_repository::
write(const domain::account& account) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account to database: " << account;

    execute_write_query(ctx_, account_mapper::map(account),
        lg(), "writing account to database");
}

void account_repository::
write(const std::vector<domain::account>& accounts) {
    BOOST_LOG_SEV(lg(), debug) << "Writing accounts to database. Count: "
                               << accounts.size();

    execute_write_query(ctx_, account_mapper::map(accounts),
        lg(), "writing accounts to database");
}

std::vector<domain::account> account_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading latest accounts");
}

std::vector<domain::account>
account_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest accounts. ID: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading latest accounts by ID.");
}

std::vector<domain::account>
account_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest accounts with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc()) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading latest accounts with pagination.");
}

std::uint32_t account_repository::get_total_account_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account count.";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<account_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account count: " << count;
    return count;
}

std::vector<domain::account> account_repository::read_all() {
    const auto query = sqlgen::read<std::vector<account_entity>> |
        order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading all accounts.");
}

std::vector<domain::account>
account_repository::read_all(const boost::uuids::uuid& id) {
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("id"_c == id_str) |
        order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading all accounts by ID.");
}

std::vector<domain::account>
account_repository::read_latest_by_username(const std::string& username) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account by username: " << username;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("username"_c == username && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    const auto sql = postgres::to_sql(query);
    BOOST_LOG_SEV(lg(), debug) << "Query: " << sql;

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading latest account by username");
}

std::vector<domain::account>
account_repository::read_latest_by_email(const std::string& email) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account by email: " << email;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
        where("email"_c == email && "valid_to"_c == max.value()) |
        order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(ctx_, query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(), "Reading latest account by email");
}

void account_repository::remove(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account from database: " << account_id;

    // Delete the account - the database trigger will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::delete_from<account_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing account from database");
}

}
