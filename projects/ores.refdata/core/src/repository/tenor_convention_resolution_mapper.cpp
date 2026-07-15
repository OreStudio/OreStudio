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
#include "ores.refdata.core/repository/tenor_convention_resolution_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::tenor_convention_resolution
tenor_convention_resolution_mapper::map(const tenor_convention_resolution_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::tenor_convention_resolution r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.convention_code = v.convention_code.value();
    r.tenor_code = v.tenor_code;
    r.anchor_override = v.anchor_override;
    r.offset_unit = v.offset_unit;
    r.offset_multiplier = v.offset_multiplier;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

tenor_convention_resolution_entity
tenor_convention_resolution_mapper::map(const domain::tenor_convention_resolution& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    tenor_convention_resolution_entity r;
    r.convention_code = v.convention_code;
    r.tenant_id = v.tenant_id;
    r.tenor_code = v.tenor_code;
    r.version = v.version;
    r.anchor_override = v.anchor_override;
    r.offset_unit = v.offset_unit;
    r.offset_multiplier = v.offset_multiplier;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::tenor_convention_resolution>
tenor_convention_resolution_mapper::map(const std::vector<tenor_convention_resolution_entity>& v) {
    return map_vector<tenor_convention_resolution_entity, domain::tenor_convention_resolution>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<tenor_convention_resolution_entity>
tenor_convention_resolution_mapper::map(const std::vector<domain::tenor_convention_resolution>& v) {
    return map_vector<domain::tenor_convention_resolution, tenor_convention_resolution_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
