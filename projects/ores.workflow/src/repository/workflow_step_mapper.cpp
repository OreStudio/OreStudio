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
#include "ores.workflow/repository/workflow_step_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.workflow/domain/workflow_step_json_io.hpp" // IWYU pragma: keep.

namespace ores::workflow::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workflow_step
workflow_step_mapper::map(const workflow_step_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workflow_step r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.workflow_id = boost::lexical_cast<boost::uuids::uuid>(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.state_id = boost::lexical_cast<boost::uuids::uuid>(v.state_id);
    r.request_json = v.request_json;
    r.response_json = v.response_json.value_or("");
    r.error = v.error.value_or("");
    r.command_subject = v.command_subject;
    r.command_json = v.command_json;
    if (v.command_published_at)
        r.command_published_at = timestamp_to_timepoint(*v.command_published_at);
    r.idempotency_key = v.idempotency_key;
    r.compensation_subject = v.compensation_subject;
    r.compensation_json = v.compensation_json;
    if (v.started_at)
        r.started_at = timestamp_to_timepoint(*v.started_at);
    if (v.completed_at)
        r.completed_at = timestamp_to_timepoint(*v.completed_at);
    if (!v.created_at)
        throw std::logic_error("Cannot map entity with null created_at to domain object.");
    r.created_at = timestamp_to_timepoint(*v.created_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

std::vector<domain::workflow_step>
workflow_step_mapper::map(const std::vector<workflow_step_entity>& v) {
    return map_vector<workflow_step_entity, domain::workflow_step>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

workflow_step_entity
workflow_step_mapper::to_entity(const domain::workflow_step& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain object to entity.";

    workflow_step_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.workflow_id = boost::uuids::to_string(v.workflow_id);
    r.step_index = v.step_index;
    r.name = v.name;
    r.state_id = boost::uuids::to_string(v.state_id);
    r.request_json = v.request_json;
    r.response_json = v.response_json.empty()
        ? std::nullopt : std::optional<std::string>(v.response_json);
    r.error = v.error.empty()
        ? std::nullopt : std::optional<std::string>(v.error);
    r.command_subject = v.command_subject;
    r.command_json = v.command_json;
    if (v.command_published_at)
        r.command_published_at = timepoint_to_timestamp(*v.command_published_at, lg());
    r.idempotency_key = v.idempotency_key;
    r.compensation_subject = v.compensation_subject;
    r.compensation_json = v.compensation_json;
    if (v.started_at)
        r.started_at = timepoint_to_timestamp(*v.started_at, lg());
    if (v.completed_at)
        r.completed_at = timepoint_to_timestamp(*v.completed_at, lg());
    r.created_at = timepoint_to_timestamp(v.created_at, lg());

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain object to entity.";
    return r;
}

}
