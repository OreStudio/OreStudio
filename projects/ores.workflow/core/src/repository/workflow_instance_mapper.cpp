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
#include "ores.workflow.core/repository/workflow_instance_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.workflow.api/domain/workflow_instance_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workflow_instance workflow_instance_mapper::map(const workflow_instance_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workflow_instance r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.type = v.type;
    r.state_id = boost::lexical_cast<boost::uuids::uuid>(v.state_id);
    r.request_json = v.request_json;
    r.result_json = v.result_json.value_or("");
    r.error = v.error.value_or("");
    r.correlation_id = v.correlation_id.value_or("");
    r.created_by = v.created_by;
    r.current_step_index = v.current_step_index;
    r.step_count = v.step_count;
    r.materialised_steps_json = v.materialised_steps_json;
    r.completed_at = v.completed_at.has_value() ?
                         std::optional(timestamp_to_timepoint(*v.completed_at)) :
                         std::nullopt;
    r.last_event_at = v.last_event_at.has_value() ?
                          std::optional(timestamp_to_timepoint(*v.last_event_at)) :
                          std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workflow_instance_entity workflow_instance_mapper::map(const domain::workflow_instance& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workflow_instance_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.type = v.type;
    r.state_id = boost::uuids::to_string(v.state_id);
    r.request_json = v.request_json;
    r.result_json = v.result_json.empty() ? std::nullopt : std::optional(v.result_json);
    r.error = v.error.empty() ? std::nullopt : std::optional(v.error);
    r.correlation_id = v.correlation_id.empty() ? std::nullopt : std::optional(v.correlation_id);
    r.created_by = v.created_by;
    r.current_step_index = v.current_step_index;
    r.step_count = v.step_count;
    r.materialised_steps_json = v.materialised_steps_json;
    r.completed_at =
        v.completed_at.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.completed_at)) :
            std::nullopt;
    r.last_event_at =
        v.last_event_at.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.last_event_at)) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workflow_instance>
workflow_instance_mapper::map(const std::vector<workflow_instance_entity>& v) {
    return map_vector<workflow_instance_entity, domain::workflow_instance>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<workflow_instance_entity>
workflow_instance_mapper::map(const std::vector<domain::workflow_instance>& v) {
    return map_vector<domain::workflow_instance, workflow_instance_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
