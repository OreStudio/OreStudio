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
#include "ores.compute.core/repository/workflow_batch_link_mapper.hpp"
#include "ores.compute.api/domain/workflow_batch_link_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::workflow_batch_link workflow_batch_link_mapper::map(const workflow_batch_link_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::workflow_batch_link r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.batch_id = boost::lexical_cast<boost::uuids::uuid>(v.batch_id.value());
    r.workflow_step_id = v.workflow_step_id;
    r.workflow_instance_id = v.workflow_instance_id;
    r.created_at = timestamp_to_timepoint(std::string_view{v.created_at});

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

workflow_batch_link_entity workflow_batch_link_mapper::map(const domain::workflow_batch_link& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    workflow_batch_link_entity r;
    r.batch_id = boost::uuids::to_string(v.batch_id);
    r.tenant_id = v.tenant_id.to_string();
    r.workflow_step_id = v.workflow_step_id;
    r.workflow_instance_id = v.workflow_instance_id;
    r.created_at = ores::platform::time::datetime::to_iso8601_utc(v.created_at);

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::workflow_batch_link>
workflow_batch_link_mapper::map(const std::vector<workflow_batch_link_entity>& v) {
    return map_vector<workflow_batch_link_entity, domain::workflow_batch_link>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<workflow_batch_link_entity>
workflow_batch_link_mapper::map(const std::vector<domain::workflow_batch_link>& v) {
    return map_vector<domain::workflow_batch_link, workflow_batch_link_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
