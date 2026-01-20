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
#include "ores.dq/repository/catalog_dependency_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq/domain/catalog_dependency_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::catalog_dependency
catalog_dependency_mapper::map(const catalog_dependency_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::catalog_dependency r;
    r.catalog_name = v.catalog_name.value();
    r.dependency_name = v.dependency_name;
    r.recorded_by = v.recorded_by;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

std::vector<domain::catalog_dependency>
catalog_dependency_mapper::map(const std::vector<catalog_dependency_entity>& v) {
    return map_vector<catalog_dependency_entity, domain::catalog_dependency>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

}
