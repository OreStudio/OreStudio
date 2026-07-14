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
#include "ores.refdata.core/repository/tenor_convention_resolution_entity.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

namespace {

std::vector<domain::tenor_convention_resolution>
map(const std::vector<tenor_convention_resolution_entity>& entities) {
    std::vector<domain::tenor_convention_resolution> r;
    r.reserve(entities.size());
    for (const auto& e : entities) {
        r.push_back(domain::tenor_convention_resolution{
            .convention_code = e.convention_code,
            .tenor_code = e.tenor_code,
            .anchor_override = e.anchor_override,
            .offset_unit = e.offset_unit,
            .offset_multiplier = e.offset_multiplier,
        });
    }
    return r;
}

}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_repository::read_all(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor convention resolutions.";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_convention_resolution_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("convention_code"_c, "tenor_code"_c);

    return execute_read_query<tenor_convention_resolution_entity,
                              domain::tenor_convention_resolution>(
        ctx,
        query,
        [](const auto& entities) { return map(entities); },
        lg(),
        "Reading all tenor convention resolutions.");
}

}
