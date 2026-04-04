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
#include "ores.controller.core/repository/service_definition_mapper.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::controller::repository {

using namespace ores::logging;
using namespace ores::database::repository;

api::domain::service_definition
service_definition_mapper::map(const service_definition_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity.";

    api::domain::service_definition r;
    r.version = v.version;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.service_name = v.service_name;
    r.binary_name = v.binary_name;
    r.desired_replicas = v.desired_replicas;
    r.restart_policy = v.restart_policy;
    r.max_restart_count = v.max_restart_count;
    r.enabled = v.enabled;
    r.args_template = v.args_template;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    if (!v.valid_from)
        throw std::logic_error(
            "Cannot map entity with null valid_from to domain object.");
    r.recorded_at = timestamp_to_timepoint(*v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

service_definition_entity
service_definition_mapper::map(const api::domain::service_definition& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    service_definition_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.version = v.version;
    r.service_name = v.service_name;
    r.binary_name = v.binary_name;
    r.desired_replicas = v.desired_replicas;
    r.restart_policy = v.restart_policy;
    r.max_restart_count = v.max_restart_count;
    r.enabled = v.enabled;
    r.args_template = v.args_template;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity.";
    return r;
}

std::vector<api::domain::service_definition>
service_definition_mapper::map(const std::vector<service_definition_entity>& v) {
    return map_vector<service_definition_entity, api::domain::service_definition>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

}
