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
#include "ores.workflow.core/repository/workflow_step_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.workflow.api/domain/workflow_step_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workflow_step workflow_step_mapper::map(const workflow_step_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workflow_step r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.workflow_id = boost::lexical_cast<boost::uuids::uuid>(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.state_id = boost::lexical_cast<boost::uuids::uuid>(v.state_id);
    r.request_json = v.request_json;
    r.response_json = v.response_json.value_or("");
    r.error = v.error.value_or("");
    r.step_log_json = v.step_log_json.value_or("");
    r.command_subject = v.command_subject;
    r.command_json = v.command_json;
    r.command_published_at = v.command_published_at.has_value() ?
                                 std::optional(timestamp_to_timepoint(*v.command_published_at)) :
                                 std::nullopt;
    r.idempotency_key = v.idempotency_key;
    r.compensation_subject = v.compensation_subject;
    r.compensation_json = v.compensation_json;
    r.started_at = v.started_at.has_value() ? std::optional(timestamp_to_timepoint(*v.started_at)) :
                                              std::nullopt;
    r.completed_at = v.completed_at.has_value() ?
                         std::optional(timestamp_to_timepoint(*v.completed_at)) :
                         std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workflow_step_entity workflow_step_mapper::map(const domain::workflow_step& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workflow_step_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.workflow_id = boost::uuids::to_string(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.state_id = boost::uuids::to_string(v.state_id);
    r.request_json = v.request_json;
    r.response_json = v.response_json.empty() ? std::nullopt : std::optional(v.response_json);
    r.error = v.error.empty() ? std::nullopt : std::optional(v.error);
    r.step_log_json = v.step_log_json.empty() ? std::nullopt : std::optional(v.step_log_json);
    r.command_subject = v.command_subject;
    r.command_json = v.command_json;
    r.command_published_at =
        v.command_published_at.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.command_published_at)) :
            std::nullopt;
    r.idempotency_key = v.idempotency_key;
    r.compensation_subject = v.compensation_subject;
    r.compensation_json = v.compensation_json;
    r.started_at = v.started_at.has_value() ?
                       std::optional(ores::platform::time::datetime::to_db_string(*v.started_at)) :
                       std::nullopt;
    r.completed_at =
        v.completed_at.has_value() ?
            std::optional(ores::platform::time::datetime::to_db_string(*v.completed_at)) :
            std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workflow_step>
workflow_step_mapper::map(const std::vector<workflow_step_entity>& v) {
    return map_vector<workflow_step_entity, domain::workflow_step>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<workflow_step_entity>
workflow_step_mapper::map(const std::vector<domain::workflow_step>& v) {
    return map_vector<domain::workflow_step, workflow_step_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
