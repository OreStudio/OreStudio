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
#include "ores.marketdata.core/repository/series_classification_rule_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.marketdata.api/domain/series_classification_rule_json_io.hpp" // IWYU pragma: keep.

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::series_classification_rule
series_classification_rule_mapper::map(const series_classification_rule_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::series_classification_rule r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.series_type = v.series_type.value();
    r.metric = v.metric.value();
    r.asset_class_source = v.asset_class_source;
    r.asset_class_code = v.asset_class_code;
    r.series_subclass_code = v.series_subclass_code;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

series_classification_rule_entity
series_classification_rule_mapper::map(const domain::series_classification_rule& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    series_classification_rule_entity r;
    r.series_type = v.series_type;
    r.metric = v.metric;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.asset_class_source = v.asset_class_source;
    r.asset_class_code = v.asset_class_code;
    r.series_subclass_code = v.series_subclass_code;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::series_classification_rule>
series_classification_rule_mapper::map(const std::vector<series_classification_rule_entity>& v) {
    return map_vector<series_classification_rule_entity, domain::series_classification_rule>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<series_classification_rule_entity>
series_classification_rule_mapper::map(const std::vector<domain::series_classification_rule>& v) {
    return map_vector<domain::series_classification_rule, series_classification_rule_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
