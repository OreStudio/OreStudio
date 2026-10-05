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
#include "ores.iam.core/repository/role_grant_request_role_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/role_grant_request_role_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::role_grant_request_role
role_grant_request_role_mapper::map(const role_grant_request_role_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::role_grant_request_role r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.request_id = boost::lexical_cast<boost::uuids::uuid>(v.request_id.value());
    r.role_id = boost::lexical_cast<boost::uuids::uuid>(v.role_id);
    r.applied_at = v.applied_at.has_value() ? std::optional(timestamp_to_timepoint(*v.applied_at)) :
                                              std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

role_grant_request_role_entity
role_grant_request_role_mapper::map(const domain::role_grant_request_role& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    role_grant_request_role_entity r;
    r.request_id = boost::uuids::to_string(v.request_id);
    r.tenant_id = v.tenant_id;
    r.role_id = boost::uuids::to_string(v.role_id);
    r.version = v.version;
    r.applied_at = v.applied_at.has_value() ?
                       std::optional(ores::platform::time::datetime::to_db_string(*v.applied_at)) :
                       std::nullopt;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::role_grant_request_role>
role_grant_request_role_mapper::map(const std::vector<role_grant_request_role_entity>& v) {
    return map_vector<role_grant_request_role_entity, domain::role_grant_request_role>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<role_grant_request_role_entity>
role_grant_request_role_mapper::map(const std::vector<domain::role_grant_request_role>& v) {
    return map_vector<domain::role_grant_request_role, role_grant_request_role_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
