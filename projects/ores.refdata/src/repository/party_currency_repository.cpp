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
#include "ores.refdata/repository/party_currency_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_currency_entity.hpp"
#include "ores.refdata/repository/party_currency_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_currency_repository::sql() {
    return generate_create_table_sql<party_currency_entity>(lg());
}

party_currency_repository::party_currency_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void party_currency_repository::write(
    const domain::party_currency& party_currency) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party currency to database: "
                               << party_currency.party_id << "/"
                               << party_currency.currency_iso_code;
    execute_write_query(ctx_, party_currency_mapper::map(party_currency),
        lg(), "writing party currency to database");
}

void party_currency_repository::write(
    const std::vector<domain::party_currency>& party_currencies) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party currencies to database. Count: "
                               << party_currencies.size();
    execute_write_query(ctx_, party_currency_mapper::map(party_currencies),
        lg(), "writing party currencies to database");
}

std::vector<domain::party_currency>
party_currency_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_currency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("party_id"_c, "currency_iso_code"_c);

    return execute_read_query<party_currency_entity, domain::party_currency>(
        ctx_, query,
        [](const auto& entities) { return party_currency_mapper::map(entities); },
        lg(), "Reading latest party currencies");
}

std::vector<domain::party_currency>
party_currency_repository::read_latest_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party currencies. Party: "
                               << party_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::read<std::vector<party_currency_entity>> |
        where("party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("currency_iso_code"_c);

    return execute_read_query<party_currency_entity, domain::party_currency>(
        ctx_, query,
        [](const auto& entities) { return party_currency_mapper::map(entities); },
        lg(), "Reading latest party currencies by party.");
}

std::vector<domain::party_currency>
party_currency_repository::read_latest_by_currency(
    const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party currencies. Currency: "
                               << iso_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_currency_entity>> |
        where("currency_iso_code"_c == iso_code && "valid_to"_c == max.value()) |
        order_by("party_id"_c);

    return execute_read_query<party_currency_entity, domain::party_currency>(
        ctx_, query,
        [](const auto& entities) { return party_currency_mapper::map(entities); },
        lg(), "Reading latest party currencies by currency.");
}

void party_currency_repository::remove(
    const boost::uuids::uuid& party_id,
    const std::string& iso_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party currency from database: "
                               << party_id << "/" << iso_code;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<party_currency_entity> |
        where("party_id"_c == party_id_str &&
              "currency_iso_code"_c == iso_code);

    execute_delete_query(ctx_, query, lg(),
        "removing party currency from database");
}

void party_currency_repository::remove_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all party currencies from database: "
                               << party_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<party_currency_entity> |
        where("party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all party currencies from database");
}

}
