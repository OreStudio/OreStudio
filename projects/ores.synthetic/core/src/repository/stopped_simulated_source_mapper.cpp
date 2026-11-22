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
#include "ores.synthetic.core/repository/stopped_simulated_source_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.synthetic.api/domain/stopped_simulated_source.hpp"
#include "ores.synthetic.api/domain/stopped_simulated_source_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/stopped_simulated_source_entity.hpp"
#include <vector>

namespace ores::synthetic::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::stopped_simulated_source
stopped_simulated_source_mapper::map(const stopped_simulated_source_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::stopped_simulated_source r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.source_name = v.source_name;
    r.provider_code = v.provider_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

stopped_simulated_source_entity
stopped_simulated_source_mapper::map(const domain::stopped_simulated_source& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    stopped_simulated_source_entity r;
    r.source_name = v.source_name;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.provider_code = v.provider_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::stopped_simulated_source>
stopped_simulated_source_mapper::map(const std::vector<stopped_simulated_source_entity>& v) {
    return map_vector<stopped_simulated_source_entity, domain::stopped_simulated_source>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<stopped_simulated_source_entity>
stopped_simulated_source_mapper::map(const std::vector<domain::stopped_simulated_source>& v) {
    return map_vector<domain::stopped_simulated_source, stopped_simulated_source_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
