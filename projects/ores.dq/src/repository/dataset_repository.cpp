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
#include "ores.dq/repository/dataset_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/dataset_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/dataset_entity.hpp"
#include "ores.dq/repository/dataset_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string dataset_repository::sql() {
    return generate_create_table_sql<dataset_entity>(lg());
}

dataset_repository::dataset_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void dataset_repository::write(const domain::dataset& dataset) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset to database: "
                               << dataset.id;
    execute_write_query(ctx_, dataset_mapper::map(dataset),
        lg(), "writing dataset to database");
}

void dataset_repository::write(
    const std::vector<domain::dataset>& datasets) {
    BOOST_LOG_SEV(lg(), debug) << "Writing datasets to database. Count: "
                               << datasets.size();
    execute_write_query(ctx_, dataset_mapper::map(datasets),
        lg(), "writing datasets to database");
}

std::vector<domain::dataset>
dataset_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest datasets");
}

std::vector<domain::dataset>
dataset_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset. Id: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest dataset by id.");
}

std::vector<domain::dataset>
dataset_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest datasets with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest datasets with pagination.");
}

std::vector<domain::dataset>
dataset_repository::read_latest_by_catalog(const std::string& catalog_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest datasets by catalog: "
                               << catalog_name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("catalog_name"_c == catalog_name && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest datasets by catalog.");
}

std::vector<domain::dataset>
dataset_repository::read_latest_by_subject_area(
    const std::string& subject_area_name,
    const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest datasets by subject_area: "
                               << subject_area_name << "/" << domain_name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("subject_area_name"_c == subject_area_name &&
              "domain_name"_c == domain_name &&
              "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest datasets by subject_area.");
}

std::vector<domain::dataset>
dataset_repository::read_latest_by_origin(const std::string& origin_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest datasets by origin: "
                               << origin_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("origin_code"_c == origin_code && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading latest datasets by origin.");
}

std::uint32_t dataset_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active dataset count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<dataset_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active dataset count: " << count;
    return count;
}

std::vector<domain::dataset>
dataset_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all dataset versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<dataset_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<dataset_entity, domain::dataset>(
        ctx_, query,
        [](const auto& entities) { return dataset_mapper::map(entities); },
        lg(), "Reading all dataset versions by id.");
}

void dataset_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<dataset_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing dataset from database");
}

}
