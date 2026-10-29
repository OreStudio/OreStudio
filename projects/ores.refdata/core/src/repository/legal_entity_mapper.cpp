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
#include "ores.refdata.core/repository/legal_entity_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/legal_entity.hpp"
#include "ores.refdata.api/domain/legal_entity_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/legal_entity_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::legal_entity legal_entity_mapper::map(const legal_entity_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::legal_entity r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.lei = v.lei;

    r.legal_name = v.legal_name;

    r.jurisdiction = v.jurisdiction.value_or("");
    r.entity_status = v.entity_status.value_or("");
    r.entity_category = v.entity_category.value_or("");
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

legal_entity_entity legal_entity_mapper::map(const domain::legal_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    legal_entity_entity r;
    r.lei = v.lei;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.legal_name = v.legal_name;

    r.jurisdiction = v.jurisdiction.empty() ? std::nullopt : std::optional(v.jurisdiction);
    r.entity_status = v.entity_status.empty() ? std::nullopt : std::optional(v.entity_status);
    r.entity_category = v.entity_category.empty() ? std::nullopt : std::optional(v.entity_category);
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::legal_entity>
legal_entity_mapper::map(const std::vector<legal_entity_entity>& v) {
    return map_vector<legal_entity_entity, domain::legal_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<legal_entity_entity>
legal_entity_mapper::map(const std::vector<domain::legal_entity>& v) {
    return map_vector<domain::legal_entity, legal_entity_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
