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
#include "ores.dq/repository/artefact_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/artefact_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/artefact_type_entity.hpp"
#include "ores.dq/repository/artefact_type_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string artefact_type_repository::sql() {
    return generate_create_table_sql<artefact_type_entity>(lg());
}

artefact_type_repository::artefact_type_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::artefact_type>
artefact_type_repository::read_all() {
    BOOST_LOG_SEV(lg(), debug) << "Reading all artefact_types";

    const auto query = sqlgen::read<std::vector<artefact_type_entity>> |
        order_by("display_order"_c);

    return execute_read_query<artefact_type_entity, domain::artefact_type>(
        ctx_, query,
        [](const auto& entities) { return artefact_type_mapper::map(entities); },
        lg(), "Reading all artefact_types");
}

std::optional<domain::artefact_type>
artefact_type_repository::read_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading artefact_type by code: " << code;

    const auto query = sqlgen::read<std::vector<artefact_type_entity>> |
        where("code"_c == code);

    auto results = execute_read_query<artefact_type_entity, domain::artefact_type>(
        ctx_, query,
        [](const auto& entities) { return artefact_type_mapper::map(entities); },
        lg(), "Reading artefact_type by code");

    if (results.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No artefact_type found for code: " << code;
        return std::nullopt;
    }

    return results.front();
}

}
