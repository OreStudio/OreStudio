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
#include "ores.dq.core/repository/lei_relationship_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/lei_relationship_json_io.hpp" // IWYU pragma: keep.

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::lei_relationship lei_relationship_mapper::map(const lei_relationship_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::lei_relationship r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.relationship_start_node_node_id = v.relationship_start_node_node_id.value();
    r.relationship_start_node_node_id_type = v.relationship_start_node_node_id_type;
    r.relationship_end_node_node_id = v.relationship_end_node_node_id;
    r.relationship_end_node_node_id_type = v.relationship_end_node_node_id_type;
    r.relationship_relationship_type = v.relationship_relationship_type;
    r.relationship_relationship_status = v.relationship_relationship_status;
    r.relationship_period_1_start_date =
        v.relationship_period_1_start_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.relationship_period_1_start_date)) :
            std::nullopt;
    r.relationship_period_1_end_date =
        v.relationship_period_1_end_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.relationship_period_1_end_date)) :
            std::nullopt;
    r.registration_initial_registration_date =
        v.registration_initial_registration_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.registration_initial_registration_date)) :
            std::nullopt;
    r.registration_last_update_date =
        v.registration_last_update_date.has_value() ?
            std::optional(timestamp_to_timepoint(*v.registration_last_update_date)) :
            std::nullopt;
    r.registration_registration_status = v.registration_registration_status;
    r.registration_validation_sources = v.registration_validation_sources;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

lei_relationship_entity lei_relationship_mapper::map(const domain::lei_relationship& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    lei_relationship_entity r;
    r.relationship_start_node_node_id = v.relationship_start_node_node_id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.relationship_start_node_node_id_type = v.relationship_start_node_node_id_type;
    r.relationship_end_node_node_id = v.relationship_end_node_node_id;
    r.relationship_end_node_node_id_type = v.relationship_end_node_node_id_type;
    r.relationship_relationship_type = v.relationship_relationship_type;
    r.relationship_relationship_status = v.relationship_relationship_status;
    r.relationship_period_1_start_date =
        v.relationship_period_1_start_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.relationship_period_1_start_date, lg())) :
            std::nullopt;
    r.relationship_period_1_end_date =
        v.relationship_period_1_end_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.relationship_period_1_end_date, lg())) :
            std::nullopt;
    r.registration_initial_registration_date =
        v.registration_initial_registration_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.registration_initial_registration_date, lg())) :
            std::nullopt;
    r.registration_last_update_date =
        v.registration_last_update_date.has_value() ?
            std::optional(timepoint_to_timestamp(*v.registration_last_update_date, lg())) :
            std::nullopt;
    r.registration_registration_status = v.registration_registration_status;
    r.registration_validation_sources = v.registration_validation_sources;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::lei_relationship>
lei_relationship_mapper::map(const std::vector<lei_relationship_entity>& v) {
    return map_vector<lei_relationship_entity, domain::lei_relationship>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<lei_relationship_entity>
lei_relationship_mapper::map(const std::vector<domain::lei_relationship>& v) {
    return map_vector<domain::lei_relationship, lei_relationship_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
