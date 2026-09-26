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
#include "ores.compute.core/repository/node_sample_mapper.hpp"
#include "ores.compute.api/domain/node_sample_json_io.hpp" // IWYU pragma: keep.
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

domain::node_sample node_sample_mapper::map(const node_sample_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::node_sample r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.sampled_at = timestamp_to_timepoint(std::string_view{v.sampled_at.value()});
    r.host_id = boost::lexical_cast<boost::uuids::uuid>(v.host_id);
    r.tasks_completed = v.tasks_completed;
    r.tasks_failed = v.tasks_failed;
    r.tasks_since_last = v.tasks_since_last;
    r.avg_task_duration_ms = v.avg_task_duration_ms;
    r.max_task_duration_ms = v.max_task_duration_ms;
    r.input_bytes_fetched = v.input_bytes_fetched;
    r.output_bytes_uploaded = v.output_bytes_uploaded;
    r.seconds_since_hb = v.seconds_since_hb;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

node_sample_entity node_sample_mapper::map(const domain::node_sample& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    node_sample_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.sampled_at = ores::platform::time::datetime::to_db_string(v.sampled_at);
    r.tenant_id = v.tenant_id.to_string();
    r.host_id = boost::uuids::to_string(v.host_id);
    r.tasks_completed = v.tasks_completed;
    r.tasks_failed = v.tasks_failed;
    r.tasks_since_last = v.tasks_since_last;
    r.avg_task_duration_ms = v.avg_task_duration_ms;
    r.max_task_duration_ms = v.max_task_duration_ms;
    r.input_bytes_fetched = v.input_bytes_fetched;
    r.output_bytes_uploaded = v.output_bytes_uploaded;
    r.seconds_since_hb = v.seconds_since_hb;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::node_sample> node_sample_mapper::map(const std::vector<node_sample_entity>& v) {
    return map_vector<node_sample_entity, domain::node_sample>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<node_sample_entity> node_sample_mapper::map(const std::vector<domain::node_sample>& v) {
    return map_vector<domain::node_sample, node_sample_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
