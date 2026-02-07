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
#include "ores.refdata/repository/party_identifier_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_identifier_entity.hpp"
#include "ores.refdata/repository/party_identifier_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_identifier_repository::sql() {
    return generate_create_table_sql<party_identifier_entity>(lg());
}

party_identifier_repository::party_identifier_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void party_identifier_repository::write(const domain::party_identifier& party_identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party identifier to database: "
                               << party_identifier.id;
    execute_write_query(ctx_, party_identifier_mapper::map(party_identifier),
        lg(), "writing party identifier to database");
}

void party_identifier_repository::write(
    const std::vector<domain::party_identifier>& party_identifiers) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party identifiers to database. Count: "
                               << party_identifiers.size();
    execute_write_query(ctx_, party_identifier_mapper::map(party_identifiers),
        lg(), "writing party identifiers to database");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("id_scheme"_c);

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx_, query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(), "Reading latest party identifiers");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party identifier. Id: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx_, query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(), "Reading latest party identifier by id.");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party identifier. Code: " << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
        where("id_scheme"_c == code && "valid_to"_c == max.value());

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx_, query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(), "Reading latest party identifier by code.");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all party identifier versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx_, query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(), "Reading all party identifier versions by id.");
}

void party_identifier_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party identifier from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<party_identifier_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing party identifier from database");
}

}
