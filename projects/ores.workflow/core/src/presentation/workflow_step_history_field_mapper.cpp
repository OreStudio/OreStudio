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
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.workflow.core/presentation/workflow_step_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::workflow::presentation {

std::vector<ores::diff::domain::field_value>
render_workflow_step_fields(const domain::workflow_step& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Workflow ID", .value = boost::uuids::to_string(v.workflow_id)});
    fields.push_back({.name = "Step Index", .value = std::to_string(v.step_index)});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "State ID", .value = boost::uuids::to_string(v.state_id)});
    fields.push_back({.name = "Request Json", .value = v.request_json});
    fields.push_back({.name = "Response Json", .value = v.response_json});
    fields.push_back({.name = "Error", .value = v.error});
    fields.push_back({.name = "Command Subject", .value = v.command_subject});
    fields.push_back({.name = "Command Json", .value = v.command_json});
    fields.push_back(
        {.name = "Command Published At",
         .value = v.command_published_at ?
                      ores::platform::time::datetime::to_iso8601_utc(*v.command_published_at) :
                      std::string{}});
    fields.push_back({.name = "Idempotency Key", .value = v.idempotency_key});
    fields.push_back({.name = "Compensation Subject", .value = v.compensation_subject});
    fields.push_back({.name = "Compensation Json", .value = v.compensation_json});
    fields.push_back({.name = "Started At",
                      .value = v.started_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.started_at) :
                                   std::string{}});
    fields.push_back({.name = "Completed At",
                      .value = v.completed_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.completed_at) :
                                   std::string{}});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
