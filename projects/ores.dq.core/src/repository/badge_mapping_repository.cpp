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
#include "ores.dq.core/repository/badge_mapping_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq.core/repository/badge_mapping_entity.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

badge_mapping_repository::badge_mapping_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<messaging::badge_mapping> badge_mapping_repository::read_all() {
    BOOST_LOG_SEV(lg(), debug) << "Reading all active badge mappings";
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_mapping_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code_domain_code"_c, "entity_code"_c);

    return execute_read_query<badge_mapping_entity, messaging::badge_mapping>(
        ctx_, query,
        [](const auto& entities) {
            std::vector<messaging::badge_mapping> result;
            result.reserve(entities.size());
            for (const auto& e : entities) {
                messaging::badge_mapping m;
                m.code_domain_code = e.code_domain_code;
                m.entity_code = e.entity_code;
                m.badge_code = e.badge_code;
                result.push_back(std::move(m));
            }
            return result;
        },
        lg(), "Reading active badge mappings");
}

}
