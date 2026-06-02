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
#include "ores.compute.core/repository/result_mapper.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.compute.api/domain/result_json_io.hpp" // IWYU pragma: keep.

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::result
result_mapper::map(const result_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::result r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.workunit_id = boost::lexical_cast<boost::uuids::uuid>(v.workunit_id);
    r.host_id = v.host_id.has_value() ? boost::lexical_cast<boost::uuids::uuid>(*v.host_id) : boost::uuids::uuid{};
    r.pgmq_msg_id = v.pgmq_msg_id.value_or(0LL);
    r.server_state = v.server_state;
    r.outcome = v.outcome.value_or(0);
    r.output_uri = v.output_uri.value_or("");
    r.error_message = v.error_message.value_or("");
    r.received_at = v.received_at ? timestamp_to_timepoint(*v.received_at) : std::chrono::system_clock::time_point{};
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

result_entity
result_mapper::map(const domain::result& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    result_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.workunit_id = boost::uuids::to_string(v.workunit_id);
    r.host_id = (v.host_id == boost::uuids::uuid{}) ? std::nullopt : std::optional(boost::uuids::to_string(v.host_id));
    r.pgmq_msg_id = v.pgmq_msg_id == 0 ? std::nullopt : std::optional(v.pgmq_msg_id);
    r.server_state = v.server_state;
    r.outcome = v.outcome == 0 ? std::nullopt : std::optional(v.outcome);
    r.output_uri = v.output_uri.empty() ? std::nullopt : std::optional(v.output_uri);
    r.error_message = v.error_message.empty() ? std::nullopt : std::optional(v.error_message);
    r.received_at = (v.received_at == std::chrono::system_clock::time_point{}) ? std::nullopt : std::optional(timepoint_to_timestamp(v.received_at, lg()));
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::result>
result_mapper::map(const std::vector<result_entity>& v) {
    return map_vector<result_entity, domain::result>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<result_entity>
result_mapper::map(const std::vector<domain::result>& v) {
    return map_vector<domain::result, result_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
