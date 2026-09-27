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
#include "ores.reporting.core/repository/parameter_definition_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.reporting.api/domain/parameter_definition_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::parameter_definition
parameter_definition_mapper::map(const parameter_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::parameter_definition r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.scope = v.scope;
    r.subtype = v.subtype;
    r.name = v.name;
    r.position = v.position;
    r.parameter_value_domain_code = v.parameter_value_domain_code;
    r.is_required = v.is_required;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

parameter_definition_entity
parameter_definition_mapper::map(const domain::parameter_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    parameter_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.scope = v.scope;
    r.subtype = v.subtype;
    r.name = v.name;
    r.position = v.position;
    r.parameter_value_domain_code = v.parameter_value_domain_code;
    r.is_required = v.is_required;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::parameter_definition>
parameter_definition_mapper::map(const std::vector<parameter_definition_entity>& v) {
    return map_vector<parameter_definition_entity, domain::parameter_definition>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<parameter_definition_entity>
parameter_definition_mapper::map(const std::vector<domain::parameter_definition>& v) {
    return map_vector<domain::parameter_definition, parameter_definition_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
