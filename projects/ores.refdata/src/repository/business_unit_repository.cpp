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
#include "ores.refdata/repository/business_unit_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/business_unit_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/business_unit_entity.hpp"
#include "ores.refdata/repository/business_unit_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string business_unit_repository::sql() {
    return generate_create_table_sql<business_unit_entity>(lg());
}

business_unit_repository::business_unit_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void business_unit_repository::write(const domain::business_unit& business_unit) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business unit to database: "
                               << business_unit.id;
    execute_write_query(ctx_, business_unit_mapper::map(business_unit),
        lg(), "writing business unit to database");
}

void business_unit_repository::write(
    const std::vector<domain::business_unit>& business_units) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business units to database. Count: "
                               << business_units.size();
    execute_write_query(ctx_, business_unit_mapper::map(business_units),
        lg(), "writing business units to database");
}

std::vector<domain::business_unit>
business_unit_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("unit_name"_c);

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx_, query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(), "Reading latest business units");
}

std::vector<domain::business_unit>
business_unit_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx_, query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(), "Reading latest business unit by id.");
}

std::vector<domain::business_unit>
business_unit_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
        where("unit_code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx_, query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(), "Reading latest business unit by code.");
}

std::vector<domain::business_unit>
business_unit_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all business unit versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<business_unit_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<business_unit_entity, domain::business_unit>(
        ctx_, query,
        [](const auto& entities) { return business_unit_mapper::map(entities); },
        lg(), "Reading all business unit versions by id.");
}

void business_unit_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business unit from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<business_unit_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing business unit from database");
}

}
