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
#include "ores.analytics.core/repository/sensitivity_asset_class_mapper.hpp"
#include "ores.analytics.api/domain/sensitivity_asset_class.hpp"
#include "ores.analytics.api/domain/sensitivity_asset_class_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.core/repository/sensitivity_asset_class_entity.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::sensitivity_asset_class
sensitivity_asset_class_mapper::map(const sensitivity_asset_class_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::sensitivity_asset_class r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.asset_class_code = v.asset_class_code.value();
    r.template_prefix = v.template_prefix;
    r.name = v.name;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

sensitivity_asset_class_entity
sensitivity_asset_class_mapper::map(const domain::sensitivity_asset_class& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    sensitivity_asset_class_entity r;
    r.asset_class_code = v.asset_class_code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.template_prefix = v.template_prefix;
    r.name = v.name;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::sensitivity_asset_class>
sensitivity_asset_class_mapper::map(const std::vector<sensitivity_asset_class_entity>& v) {
    return map_vector<sensitivity_asset_class_entity, domain::sensitivity_asset_class>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<sensitivity_asset_class_entity>
sensitivity_asset_class_mapper::map(const std::vector<domain::sensitivity_asset_class>& v) {
    return map_vector<domain::sensitivity_asset_class, sensitivity_asset_class_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
