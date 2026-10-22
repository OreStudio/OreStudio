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
#include "ores.reporting.core/repository/ore_parameter_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/ore_parameter.hpp"
#include "ores.reporting.api/domain/ore_parameter_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/ore_parameter_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::ore_parameter ore_parameter_mapper::map(const ore_parameter_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::ore_parameter r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.scope = v.scope;
    r.subtype = v.subtype;
    r.name = v.name;
    r.schema_name = v.schema_name;
    r.xpath = v.xpath;
    r.position = v.position;
    r.ore_parameter_value_domain_code = v.ore_parameter_value_domain_code;
    r.is_required = v.is_required;
    r.label = v.label.value_or("");
    r.help = v.help.value_or("");
    r.description = v.description.value_or("");
    r.default_text = v.default_text.value_or("");
    r.format = v.format.value_or("");
    r.introduced_in = v.introduced_in.value_or("");
    r.removed_in = v.removed_in.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

ore_parameter_entity ore_parameter_mapper::map(const domain::ore_parameter& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    ore_parameter_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.scope = v.scope;
    r.subtype = v.subtype;
    r.name = v.name;
    r.schema_name = v.schema_name;
    r.xpath = v.xpath;
    r.position = v.position;
    r.ore_parameter_value_domain_code = v.ore_parameter_value_domain_code;
    r.is_required = v.is_required;
    r.label = v.label.empty() ? std::nullopt : std::optional(v.label);
    r.help = v.help.empty() ? std::nullopt : std::optional(v.help);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.default_text = v.default_text.empty() ? std::nullopt : std::optional(v.default_text);
    r.format = v.format.empty() ? std::nullopt : std::optional(v.format);
    r.introduced_in = v.introduced_in.empty() ? std::nullopt : std::optional(v.introduced_in);
    r.removed_in = v.removed_in.empty() ? std::nullopt : std::optional(v.removed_in);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::ore_parameter>
ore_parameter_mapper::map(const std::vector<ore_parameter_entity>& v) {
    return map_vector<ore_parameter_entity, domain::ore_parameter>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<ore_parameter_entity>
ore_parameter_mapper::map(const std::vector<domain::ore_parameter>& v) {
    return map_vector<domain::ore_parameter, ore_parameter_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
