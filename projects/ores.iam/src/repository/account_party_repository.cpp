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
#include "ores.iam/repository/account_party_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/account_party_entity.hpp"
#include "ores.iam/repository/account_party_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_party_repository::sql() {
    return generate_create_table_sql<account_party_entity>(lg());
}

account_party_repository::account_party_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void account_party_repository::write(
    const domain::account_party& account_party) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account party to database: "
                               << account_party.account_id << "/" << account_party.party_id;
    execute_write_query(ctx_, account_party_mapper::map(account_party),
        lg(), "writing account party to database");
}

void account_party_repository::write(
    const std::vector<domain::account_party>& account_parties) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account parties to database. Count: "
                               << account_parties.size();
    execute_write_query(ctx_, account_party_mapper::map(account_parties),
        lg(), "writing account parties to database");
}

std::vector<domain::account_party>
account_party_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("account_id"_c, "party_id"_c);

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_, query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(), "Reading latest account parties");
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties. Account: "
                               << account_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
        where("account_id"_c == account_id_str && "valid_to"_c == max.value()) |
        order_by("party_id"_c);

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_, query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(), "Reading latest account parties by account.");
}

std::vector<domain::account_party>
account_party_repository::read_latest_by_party(const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account parties. Party: "
                               << party_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::read<std::vector<account_party_entity>> |
        where("party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("account_id"_c);

    return execute_read_query<account_party_entity, domain::account_party>(
        ctx_, query,
        [](const auto& entities) { return account_party_mapper::map(entities); },
        lg(), "Reading latest account parties by party.");
}

void account_party_repository::remove(
    const boost::uuids::uuid& account_id, const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account party from database: "
                               << account_id << "/" << party_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<account_party_entity> |
        where("account_id"_c == account_id_str && "party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing account party from database");
}

void account_party_repository::remove_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all account parties from database: "
                               << account_id;

    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto query = sqlgen::delete_from<account_party_entity> |
        where("account_id"_c == account_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all account parties from database");
}

}
