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
#include "ores.dq/repository/dataset_bundle_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/dataset_bundle_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/dataset_bundle_entity.hpp"
#include "ores.dq/repository/dataset_bundle_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string dataset_bundle_repository::sql() {
    return generate_create_table_sql<dataset_bundle_entity>(lg());
}

dataset_bundle_repository::dataset_bundle_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void dataset_bundle_repository::write(const domain::dataset_bundle& bundle) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundle to database: "
                               << bundle.id;
    execute_write_query(ctx_, dataset_bundle_mapper::map(bundle),
        lg(), "writing dataset bundle to database");
}

void dataset_bundle_repository::write(
    const std::vector<domain::dataset_bundle>& bundles) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundles to database. Count: "
                               << bundles.size();
    execute_write_query(ctx_, dataset_bundle_mapper::map(bundles),
        lg(), "writing dataset bundles to database");
}

std::vector<domain::dataset_bundle>
dataset_bundle_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_bundle_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<dataset_bundle_entity, domain::dataset_bundle>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_mapper::map(entities); },
        lg(), "Reading latest dataset bundles");
}

std::vector<domain::dataset_bundle>
dataset_bundle_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<dataset_bundle_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<dataset_bundle_entity, domain::dataset_bundle>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_mapper::map(entities); },
        lg(), "Reading latest dataset bundle by id.");
}

std::vector<domain::dataset_bundle>
dataset_bundle_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_bundle_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<dataset_bundle_entity, domain::dataset_bundle>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_mapper::map(entities); },
        lg(), "Reading latest dataset bundle by code.");
}

std::vector<domain::dataset_bundle>
dataset_bundle_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all dataset bundle versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<dataset_bundle_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<dataset_bundle_entity, domain::dataset_bundle>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_mapper::map(entities); },
        lg(), "Reading all dataset bundle versions by id.");
}

void dataset_bundle_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<dataset_bundle_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing dataset bundle from database");
}

}
