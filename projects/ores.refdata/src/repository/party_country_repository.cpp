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
#include "ores.refdata/repository/party_country_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_country_entity.hpp"
#include "ores.refdata/repository/party_country_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_country_repository::sql() {
    return generate_create_table_sql<party_country_entity>(lg());
}

party_country_repository::party_country_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void party_country_repository::write(
    const domain::party_country& party_country) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party country to database: "
                               << party_country.party_id << "/"
                               << party_country.country_alpha2_code;
    execute_write_query(ctx_, party_country_mapper::map(party_country),
        lg(), "writing party country to database");
}

void party_country_repository::write(
    const std::vector<domain::party_country>& party_countries) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party countries to database. Count: "
                               << party_countries.size();
    execute_write_query(ctx_, party_country_mapper::map(party_countries),
        lg(), "writing party countries to database");
}

std::vector<domain::party_country>
party_country_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_country_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("party_id"_c, "country_alpha2_code"_c);

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_, query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(), "Reading latest party countries");
}

std::vector<domain::party_country>
party_country_repository::read_latest_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries. Party: "
                               << party_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::read<std::vector<party_country_entity>> |
        where("party_id"_c == party_id_str && "valid_to"_c == max.value()) |
        order_by("country_alpha2_code"_c);

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_, query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(), "Reading latest party countries by party.");
}

std::vector<domain::party_country>
party_country_repository::read_latest_by_country(
    const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party countries. Country: "
                               << alpha2_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_country_entity>> |
        where("country_alpha2_code"_c == alpha2_code && "valid_to"_c == max.value()) |
        order_by("party_id"_c);

    return execute_read_query<party_country_entity, domain::party_country>(
        ctx_, query,
        [](const auto& entities) { return party_country_mapper::map(entities); },
        lg(), "Reading latest party countries by country.");
}

void party_country_repository::remove(
    const boost::uuids::uuid& party_id,
    const std::string& alpha2_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party country from database: "
                               << party_id << "/" << alpha2_code;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<party_country_entity> |
        where("party_id"_c == party_id_str &&
              "country_alpha2_code"_c == alpha2_code);

    execute_delete_query(ctx_, query, lg(),
        "removing party country from database");
}

void party_country_repository::remove_by_party(
    const boost::uuids::uuid& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all party countries from database: "
                               << party_id;

    const auto party_id_str = boost::uuids::to_string(party_id);
    const auto query = sqlgen::delete_from<party_country_entity> |
        where("party_id"_c == party_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all party countries from database");
}

}
