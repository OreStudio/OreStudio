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
#include "ores.analytics.core/repository/pricing_pair_setting_mapper.hpp"
#include "ores.analytics.api/domain/pricing_pair_setting.hpp"
#include "ores.analytics.api/domain/pricing_pair_setting_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.core/repository/pricing_pair_setting_entity.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::analytics::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::pricing_pair_setting
pricing_pair_setting_mapper::map(const pricing_pair_setting_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::pricing_pair_setting r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code;
    r.model = v.model;
    r.engine = v.engine;
    r.pricing_engine_type_code = v.pricing_engine_type_code;
    r.scope = v.scope;
    r.name = v.name;
    r.usual_value = v.usual_value.value_or("");
    r.is_required = v.is_required;
    r.note = v.note.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

pricing_pair_setting_entity
pricing_pair_setting_mapper::map(const domain::pricing_pair_setting& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    pricing_pair_setting_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.model = v.model;
    r.engine = v.engine;
    r.pricing_engine_type_code = v.pricing_engine_type_code;
    r.scope = v.scope;
    r.name = v.name;
    r.usual_value = v.usual_value.empty() ? std::nullopt : std::optional(v.usual_value);
    r.is_required = v.is_required;
    r.note = v.note.empty() ? std::nullopt : std::optional(v.note);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::pricing_pair_setting>
pricing_pair_setting_mapper::map(const std::vector<pricing_pair_setting_entity>& v) {
    return map_vector<pricing_pair_setting_entity, domain::pricing_pair_setting>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<pricing_pair_setting_entity>
pricing_pair_setting_mapper::map(const std::vector<domain::pricing_pair_setting>& v) {
    return map_vector<domain::pricing_pair_setting, pricing_pair_setting_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
