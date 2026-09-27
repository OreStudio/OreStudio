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
#include "ores.reporting.core/repository/report_configuration_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.reporting.api/domain/report_configuration_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::report_configuration
report_configuration_mapper::map(const report_configuration_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::report_configuration r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.report_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.report_definition_id);
    r.configuration_type_code = v.configuration_type_code;
    r.configuration_id = boost::lexical_cast<boost::uuids::uuid>(v.configuration_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

report_configuration_entity
report_configuration_mapper::map(const domain::report_configuration& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    report_configuration_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.report_definition_id = boost::uuids::to_string(v.report_definition_id);
    r.configuration_type_code = v.configuration_type_code;
    r.configuration_id = boost::uuids::to_string(v.configuration_id);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::report_configuration>
report_configuration_mapper::map(const std::vector<report_configuration_entity>& v) {
    return map_vector<report_configuration_entity, domain::report_configuration>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<report_configuration_entity>
report_configuration_mapper::map(const std::vector<domain::report_configuration>& v) {
    return map_vector<domain::report_configuration, report_configuration_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
