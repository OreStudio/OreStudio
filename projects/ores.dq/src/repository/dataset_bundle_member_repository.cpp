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
#include "ores.dq/repository/dataset_bundle_member_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/dataset_bundle_member_entity.hpp"
#include "ores.dq/repository/dataset_bundle_member_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string dataset_bundle_member_repository::sql() {
    return generate_create_table_sql<dataset_bundle_member_entity>(lg());
}

dataset_bundle_member_repository::dataset_bundle_member_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void dataset_bundle_member_repository::write(
    const domain::dataset_bundle_member& member) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundle member to database: "
                               << member.bundle_code << "/" << member.dataset_code;
    execute_write_query(ctx_, dataset_bundle_member_mapper::map(member),
        lg(), "writing dataset bundle member to database");
}

void dataset_bundle_member_repository::write(
    const std::vector<domain::dataset_bundle_member>& members) {
    BOOST_LOG_SEV(lg(), debug) << "Writing dataset bundle members to database. Count: "
                               << members.size();
    execute_write_query(ctx_, dataset_bundle_member_mapper::map(members),
        lg(), "writing dataset bundle members to database");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("bundle_code"_c, "display_order"_c);

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(), "Reading latest dataset bundle members");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Bundle: "
                               << bundle_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
        where("bundle_code"_c == bundle_code && "valid_to"_c == max.value()) |
        order_by("display_order"_c);

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(), "Reading latest dataset bundle members by bundle.");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_dataset(const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Dataset: "
                               << dataset_code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_bundle_member_entity>> |
        where("dataset_code"_c == dataset_code && "valid_to"_c == max.value()) |
        order_by("bundle_code"_c);

    return execute_read_query<dataset_bundle_member_entity, domain::dataset_bundle_member>(
        ctx_, query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(), "Reading latest dataset bundle members by dataset.");
}

void dataset_bundle_member_repository::remove(
    const std::string& bundle_code, const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing dataset bundle member from database: "
                               << bundle_code << "/" << dataset_code;

    const auto query = sqlgen::delete_from<dataset_bundle_member_entity> |
        where("bundle_code"_c == bundle_code && "dataset_code"_c == dataset_code);

    execute_delete_query(ctx_, query, lg(),
        "removing dataset bundle member from database");
}

void dataset_bundle_member_repository::remove_by_bundle(const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all dataset bundle members from database: "
                               << bundle_code;

    const auto query = sqlgen::delete_from<dataset_bundle_member_entity> |
        where("bundle_code"_c == bundle_code);

    execute_delete_query(ctx_, query, lg(),
        "removing all dataset bundle members from database");
}

}
