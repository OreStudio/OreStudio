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
#include "ores.controller.core/repository/service_event_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"

namespace ores::controller::repository {

using namespace ores::logging;
using namespace ores::database::repository;

api::domain::service_event
service_event_mapper::map(const service_event_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity.";

    api::domain::service_event r;
    r.occurred_at = timestamp_to_timepoint(v.occurred_at.value());
    r.event_id = boost::lexical_cast<boost::uuids::uuid>(v.event_id.value());
    r.service_name = v.service_name;
    if (v.instance_id)
        r.instance_id = boost::lexical_cast<boost::uuids::uuid>(*v.instance_id);
    r.replica_index = v.replica_index;
    r.event_type = v.event_type;
    r.message = v.message;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity.";
    return r;
}

service_event_entity
service_event_mapper::map(const api::domain::service_event& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity.";

    service_event_entity r;
    r.occurred_at = timepoint_to_timestamp(v.occurred_at, lg());
    r.event_id = boost::uuids::to_string(v.event_id);
    r.service_name = v.service_name;
    if (v.instance_id)
        r.instance_id = boost::uuids::to_string(*v.instance_id);
    r.replica_index = v.replica_index;
    r.event_type = v.event_type;
    r.message = v.message;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity.";
    return r;
}

std::vector<api::domain::service_event>
service_event_mapper::map(const std::vector<service_event_entity>& v) {
    return map_vector<service_event_entity, api::domain::service_event>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

}
