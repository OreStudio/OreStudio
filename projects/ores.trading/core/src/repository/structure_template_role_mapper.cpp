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
#include "ores.trading.core/repository/structure_template_role_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/structure_template_role.hpp"
#include "ores.trading.api/domain/structure_template_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/structure_template_role_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::structure_template_role
structure_template_role_mapper::map(const structure_template_role_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::structure_template_role r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.template_code = v.template_code.value();
    r.role = v.role.value();
    r.min_legs = v.min_legs;
    r.max_legs = v.max_legs;
    r.description = v.description.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

structure_template_role_entity
structure_template_role_mapper::map(const domain::structure_template_role& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    structure_template_role_entity r;
    r.template_code = v.template_code;
    r.role = v.role;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.min_legs = v.min_legs;
    r.max_legs = v.max_legs;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::structure_template_role>
structure_template_role_mapper::map(const std::vector<structure_template_role_entity>& v) {
    return map_vector<structure_template_role_entity, domain::structure_template_role>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<structure_template_role_entity>
structure_template_role_mapper::map(const std::vector<domain::structure_template_role>& v) {
    return map_vector<domain::structure_template_role, structure_template_role_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
