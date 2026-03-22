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
#include "ores.compute/repository/platform_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.compute/repository/platform_entity.hpp"

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<domain::compute_platform>
platform_repository::read_active(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading active compute platforms";
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto query = sqlgen::read<std::vector<platform_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("display_name"_c);

    return execute_read_query<platform_entity, domain::compute_platform>(
        ctx, query,
        [](const std::vector<platform_entity>& entities) {
            std::vector<domain::compute_platform> result;
            result.reserve(entities.size());
            for (const auto& e : entities) {
                domain::compute_platform p;
                try {
                    p.id = boost::lexical_cast<boost::uuids::uuid>(e.id.value());
                } catch (const boost::bad_lexical_cast& ex) {
                    BOOST_LOG_SEV(lg(), error)
                        << "Failed to parse platform UUID '"
                        << e.id.value() << "': " << ex.what();
                    continue;
                }
                p.code = e.code;
                p.display_name = e.display_name;
                p.description = e.description;
                p.os_family = e.os_family;
                p.cpu_arch = e.cpu_arch;
                p.abi = e.abi;
                p.is_active = e.is_active;
                result.push_back(std::move(p));
            }
            return result;
        },
        lg(), "Reading active platforms");
}

}
