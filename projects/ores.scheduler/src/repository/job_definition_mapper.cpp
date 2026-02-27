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
#include "ores.scheduler/repository/job_definition_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.scheduler/domain/job_definition_json_io.hpp" // IWYU pragma: keep.

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::job_definition
job_definition_mapper::map(const job_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::job_definition r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.cron_job_id = v.cron_job_id;
    r.job_name = v.job_name;
    r.description = v.description.value_or("");
    r.command = v.command;
    auto expr = domain::cron_expression::from_string(v.schedule_expression);
    if (!expr)
        throw std::logic_error("Invalid cron expression in database: " + expr.error());
    r.schedule_expression = std::move(*expr);
    r.database_name = v.database_name;
    r.is_active = (v.is_active != 0);
    r.version = v.version;
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

job_definition_entity
job_definition_mapper::map(const domain::job_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    job_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.cron_job_id = v.cron_job_id;
    r.job_name = v.job_name;
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.command = v.command;
    r.schedule_expression = v.schedule_expression.to_string();
    r.database_name = v.database_name;
    r.is_active = v.is_active ? 1 : 0;
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
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<job_definition_entity>
job_definition_mapper::map(const std::vector<domain::job_definition>& v) {
    return map_vector<domain::job_definition, job_definition_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
