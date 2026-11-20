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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.core/repository/series_identity_shape_entry_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.marketdata.api/domain/series_identity_shape_entry.hpp"
#include "ores.marketdata.api/domain/series_identity_shape_entry_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/series_identity_shape_entry_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::series_identity_shape_entry
series_identity_shape_entry_mapper::map(const series_identity_shape_entry_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::series_identity_shape_entry r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.shape_code = v.shape_code;
    r.field = v.field;
    r.requirement = v.requirement;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

series_identity_shape_entry_entity
series_identity_shape_entry_mapper::map(const domain::series_identity_shape_entry& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    series_identity_shape_entry_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.shape_code = v.shape_code;
    r.field = v.field;
    r.requirement = v.requirement;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::series_identity_shape_entry>
series_identity_shape_entry_mapper::map(const std::vector<series_identity_shape_entry_entity>& v) {
    return map_vector<series_identity_shape_entry_entity, domain::series_identity_shape_entry>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<series_identity_shape_entry_entity>
series_identity_shape_entry_mapper::map(const std::vector<domain::series_identity_shape_entry>& v) {
    return map_vector<domain::series_identity_shape_entry, series_identity_shape_entry_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
