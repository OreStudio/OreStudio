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
#include "ores.reporting/repository/report_definition_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.reporting/domain/report_definition_json_io.hpp" // IWYU pragma: keep.

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::report_definition
report_definition_mapper::map(const report_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::report_definition r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.name = v.name;
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.description = v.description;
    r.report_type = v.report_type;
    r.fsm_state_id = v.fsm_state_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.fsm_state_id)) : std::nullopt;
    r.schedule_expression = v.schedule_expression;
    r.concurrency_policy = v.concurrency_policy;
    r.scheduler_job_id = v.scheduler_job_id.has_value() ? std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.scheduler_job_id)) : std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error("Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

report_definition_entity
report_definition_mapper::map(const domain::report_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    report_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.name = v.name;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.description = v.description;
    r.report_type = v.report_type;
    r.fsm_state_id = v.fsm_state_id.has_value() ? std::optional(boost::uuids::to_string(*v.fsm_state_id)) : std::nullopt;
    r.schedule_expression = v.schedule_expression;
    r.concurrency_policy = v.concurrency_policy;
    r.scheduler_job_id = v.scheduler_job_id.has_value() ? std::optional(boost::uuids::to_string(*v.scheduler_job_id)) : std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::report_definition>
report_definition_mapper::map(const std::vector<report_definition_entity>& v) {
    return map_vector<report_definition_entity, domain::report_definition>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<report_definition_entity>
report_definition_mapper::map(const std::vector<domain::report_definition>& v) {
    return map_vector<domain::report_definition, report_definition_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
