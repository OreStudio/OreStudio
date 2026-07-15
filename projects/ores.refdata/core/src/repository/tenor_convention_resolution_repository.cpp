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
#include "ores.refdata.core/repository/tenor_convention_resolution_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_convention_resolution_entity.hpp"
#include "ores.refdata.core/repository/tenor_convention_resolution_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_convention_resolution_repository::sql() {
    return generate_create_table_sql<tenor_convention_resolution_entity>(lg());
}

tenor_convention_resolution_repository::tenor_convention_resolution_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("valid_to"_c == max.value()) |
                       order_by("convention_code"_c, "tenor_code"_c);

    return execute_read_query<tenor_convention_resolution_entity,
                              domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions");
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_convention(
    const std::string& convention_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Convention: "
                               << convention_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query =
        sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
        where("convention_code"_c == convention_code && "valid_to"_c == max.value()) |
        order_by("tenor_code"_c);

    return execute_read_query<tenor_convention_resolution_entity,
                              domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions by convention.");
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_latest_by_tenor(const std::string& tenor_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor convention resolutions. Tenor: "
                               << tenor_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenor_code"_c == tenor_code && "valid_to"_c == max.value()) |
                       order_by("convention_code"_c);

    return execute_read_query<tenor_convention_resolution_entity,
                              domain::tenor_convention_resolution>(
        ctx_,
        query,
        [](const auto& entities) { return tenor_convention_resolution_mapper::map(entities); },
        lg(),
        "Reading latest tenor convention resolutions by tenor.");
}

}
