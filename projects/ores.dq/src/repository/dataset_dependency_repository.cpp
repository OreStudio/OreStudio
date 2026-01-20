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
#include "ores.dq/repository/dataset_dependency_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/dataset_dependency_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/dataset_dependency_entity.hpp"
#include "ores.dq/repository/dataset_dependency_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

dataset_dependency_repository::dataset_dependency_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::dataset_dependency>
dataset_dependency_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_dependency_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("dataset_code"_c, "dependency_code"_c);

    return execute_read_query<dataset_dependency_entity, domain::dataset_dependency>(
        ctx_, query,
        [](const auto& entities) { return dataset_dependency_mapper::map(entities); },
        lg(), "Reading latest dataset dependencies");
}

std::vector<domain::dataset_dependency>
dataset_dependency_repository::read_latest_by_dataset(const std::string& dataset_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest dependencies for dataset: "
                               << dataset_code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<dataset_dependency_entity>> |
        where("dataset_code"_c == dataset_code && "valid_to"_c == max.value()) |
        order_by("dependency_code"_c);

    return execute_read_query<dataset_dependency_entity, domain::dataset_dependency>(
        ctx_, query,
        [](const auto& entities) { return dataset_dependency_mapper::map(entities); },
        lg(), "Reading latest dataset dependencies by dataset");
}

}
