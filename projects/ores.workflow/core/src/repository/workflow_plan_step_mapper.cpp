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
#include "ores.workflow.core/repository/workflow_plan_step_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.workflow.api/domain/workflow_plan_step.hpp"
#include "ores.workflow.api/domain/workflow_plan_step_json_io.hpp" // IWYU pragma: keep.
#include "ores.workflow.core/repository/workflow_plan_step_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workflow_plan_step workflow_plan_step_mapper::map(const workflow_plan_step_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workflow_plan_step r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.workflow_id = boost::lexical_cast<boost::uuids::uuid>(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.label = v.label;
    r.description = v.description;
    r.command_subject = v.command_subject;
    r.compensation_subject = v.compensation_subject;
    r.timeout_seconds = v.timeout_seconds;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workflow_plan_step_entity workflow_plan_step_mapper::map(const domain::workflow_plan_step& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workflow_plan_step_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.workflow_id = boost::uuids::to_string(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.label = v.label;
    r.description = v.description;
    r.command_subject = v.command_subject;
    r.compensation_subject = v.compensation_subject;
    r.timeout_seconds = v.timeout_seconds;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workflow_plan_step>
workflow_plan_step_mapper::map(const std::vector<workflow_plan_step_entity>& v) {
    return map_vector<workflow_plan_step_entity, domain::workflow_plan_step>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<workflow_plan_step_entity>
workflow_plan_step_mapper::map(const std::vector<domain::workflow_plan_step>& v) {
    return map_vector<domain::workflow_plan_step, workflow_plan_step_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
