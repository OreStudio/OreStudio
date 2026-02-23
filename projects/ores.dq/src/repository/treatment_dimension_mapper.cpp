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
#include "ores.dq/repository/treatment_dimension_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq/domain/treatment_dimension_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::treatment_dimension
treatment_dimension_mapper::map(const treatment_dimension_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::treatment_dimension r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code.value();
    r.name = v.name;
    r.description = v.description;
r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

treatment_dimension_entity
treatment_dimension_mapper::map(const domain::treatment_dimension& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    treatment_dimension_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::treatment_dimension>
treatment_dimension_mapper::map(const std::vector<treatment_dimension_entity>& v) {
    return map_vector<treatment_dimension_entity, domain::treatment_dimension>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<treatment_dimension_entity>
treatment_dimension_mapper::map(const std::vector<domain::treatment_dimension>& v) {
    return map_vector<domain::treatment_dimension, treatment_dimension_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
