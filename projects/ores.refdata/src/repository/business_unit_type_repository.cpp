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
#include "ores.refdata/repository/business_unit_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata/domain/business_unit_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/business_unit_type_entity.hpp"
#include "ores.refdata/repository/business_unit_type_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string business_unit_type_repository::sql() {
    return generate_create_table_sql<business_unit_type_entity>(lg());
}

business_unit_type_repository::business_unit_type_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void business_unit_type_repository::write(
    const domain::business_unit_type& business_unit_type) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business unit type to database: "
                               << business_unit_type.id;
    execute_write_query(ctx_, business_unit_type_mapper::map(business_unit_type),
        lg(), "writing business unit type to database");
}

void business_unit_type_repository::write(
    const std::vector<domain::business_unit_type>& business_unit_types) {
    BOOST_LOG_SEV(lg(), debug) << "Writing business unit types to database. Count: "
                               << business_unit_types.size();
    execute_write_query(ctx_, business_unit_type_mapper::map(business_unit_types),
        lg(), "writing business unit types to database");
}

std::vector<domain::business_unit_type>
business_unit_type_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_unit_type_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<business_unit_type_entity, domain::business_unit_type>(
        ctx_, query,
        [](const auto& entities) { return business_unit_type_mapper::map(entities); },
        lg(), "Reading latest business unit types");
}

std::vector<domain::business_unit_type>
business_unit_type_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit type. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<business_unit_type_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<business_unit_type_entity, domain::business_unit_type>(
        ctx_, query,
        [](const auto& entities) { return business_unit_type_mapper::map(entities); },
        lg(), "Reading latest business unit type by id.");
}

std::vector<domain::business_unit_type>
business_unit_type_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest business unit type. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<business_unit_type_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<business_unit_type_entity, domain::business_unit_type>(
        ctx_, query,
        [](const auto& entities) { return business_unit_type_mapper::map(entities); },
        lg(), "Reading latest business unit type by code.");
}

std::vector<domain::business_unit_type>
business_unit_type_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all business unit type versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<business_unit_type_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<business_unit_type_entity, domain::business_unit_type>(
        ctx_, query,
        [](const auto& entities) { return business_unit_type_mapper::map(entities); },
        lg(), "Reading all business unit type versions by id.");
}

void business_unit_type_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing business unit type from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<business_unit_type_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing business unit type from database");
}

}
