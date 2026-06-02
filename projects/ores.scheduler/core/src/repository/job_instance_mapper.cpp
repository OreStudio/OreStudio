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
#include "ores.scheduler.core/repository/job_instance_mapper.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.scheduler.api/domain/job_status.hpp"

namespace ores::scheduler::repository {

namespace {

domain::job_status status_from_string(const std::string& s) {
    if (s == "succeeded") return domain::job_status::succeeded;
    if (s == "failed")    return domain::job_status::failed;
    return domain::job_status::starting;
}

} // anonymous namespace

domain::job_instance job_instance_mapper::map(const job_instance_entity& e) {
    domain::job_instance inst;

    if (e.id)
        inst.id = std::stoll(*e.id);
    if (e.tenant_id)
        inst.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*e.tenant_id);
    if (e.party_id)
        inst.party_id = boost::lexical_cast<boost::uuids::uuid>(*e.party_id);
    if (e.job_definition_id)
        inst.job_definition_id =
            boost::lexical_cast<boost::uuids::uuid>(*e.job_definition_id);
    if (e.action_type)
        inst.action_type = *e.action_type;
    if (e.status)
        inst.status = status_from_string(*e.status);
    if (e.triggered_at) {
        try {
            inst.triggered_at =
                ores::platform::time::datetime::from_iso8601_utc(*e.triggered_at);
        } catch (...) {}
    }
    if (e.started_at) {
        try {
            inst.started_at =
                ores::platform::time::datetime::from_iso8601_utc(*e.started_at);
        } catch (...) {}
    }
    if (e.completed_at) {
        try {
            inst.completed_at =
                ores::platform::time::datetime::from_iso8601_utc(*e.completed_at);
        } catch (...) {}
    }
    if (e.duration_ms)
        inst.duration_ms = std::stoll(*e.duration_ms);
    if (e.error_message)
        inst.error_message = *e.error_message;

    return inst;
}

std::vector<domain::job_instance>
job_instance_mapper::map(const std::vector<job_instance_entity>& v) {
    std::vector<domain::job_instance> result;
    result.reserve(v.size());
    for (const auto& e : v)
        result.push_back(map(e));
    return result;
}

} // namespace ores::scheduler::repository
