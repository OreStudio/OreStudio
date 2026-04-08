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
#include "ores.controller.core/repository/service_instance_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::controller::repository {

using namespace ores::logging;
using namespace ores::database::repository;

api::domain::service_instance
service_instance_mapper::map(const service_instance_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity.";

    api::domain::service_instance r;
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.service_name = v.service_name;
    r.replica_index = v.replica_index;
    r.pid = v.pid;
    r.phase = v.phase;
    if (v.started_at)
        r.started_at = timestamp_to_timepoint(*v.started_at);
    if (v.stopped_at)
        r.stopped_at = timestamp_to_timepoint(*v.stopped_at);
    r.restart_count = v.restart_count;
    r.last_error = v.last_error;
    r.created_at = timestamp_to_timepoint(v.created_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

service_instance_entity
service_instance_mapper::map(const api::domain::service_instance& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    service_instance_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.service_name = v.service_name;
    r.replica_index = v.replica_index;
    r.pid = v.pid;
    r.phase = v.phase;
    if (v.started_at)
        r.started_at = timepoint_to_timestamp(*v.started_at, lg());
    if (v.stopped_at)
        r.stopped_at = timepoint_to_timestamp(*v.stopped_at, lg());
    r.restart_count = v.restart_count;
    r.last_error = v.last_error;
    r.created_at = timepoint_to_timestamp(v.created_at, lg());

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity.";
    return r;
}

std::vector<api::domain::service_instance>
service_instance_mapper::map(const std::vector<service_instance_entity>& v) {
    return map_vector<service_instance_entity, api::domain::service_instance>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

}
