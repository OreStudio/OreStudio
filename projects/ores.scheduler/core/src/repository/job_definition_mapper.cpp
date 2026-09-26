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
#include "ores.scheduler.core/repository/job_definition_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.scheduler.api/domain/job_definition_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::job_definition job_definition_mapper::map(const job_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::job_definition r;
    r.version = v.version;
    if (v.tenant_id)
        r.tenant_id = utility::uuid::tenant_id::from_string(*v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.job_name = v.job_name;

    r.party_id = v.party_id.has_value() ?
                     std::optional(boost::lexical_cast<boost::uuids::uuid>(*v.party_id)) :
                     std::nullopt;
    r.description = v.description;
    r.command = v.command;
    r.schedule_expression = domain::cron_expression::from_string(v.schedule_expression).value();
    r.action_type = v.action_type;
    r.action_payload = v.action_payload;
    r.is_active = v.is_active;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

job_definition_entity job_definition_mapper::map(const domain::job_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    job_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    if (v.tenant_id)
        r.tenant_id = v.tenant_id->to_string();
    r.version = v.version;

    r.job_name = v.job_name;

    r.party_id =
        v.party_id.has_value() ? std::optional(boost::uuids::to_string(*v.party_id)) : std::nullopt;
    r.description = v.description;
    r.command = v.command;
    r.schedule_expression = v.schedule_expression.to_string();
    r.action_type = v.action_type;
    r.action_payload = v.action_payload;
    r.is_active = v.is_active;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::job_definition>
job_definition_mapper::map(const std::vector<job_definition_entity>& v) {
    return map_vector<job_definition_entity, domain::job_definition>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<job_definition_entity>
job_definition_mapper::map(const std::vector<domain::job_definition>& v) {
    return map_vector<domain::job_definition, job_definition_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
