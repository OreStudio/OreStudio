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
#include "ores.iam.core/repository/seed_profile_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/seed_profile_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::seed_profile seed_profile_mapper::map(const seed_profile_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::seed_profile r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.code = v.code;

    r.name = v.name;
    r.description = v.description;
    r.audience = v.audience;
    r.tenant_name = v.tenant_name;
    r.tenant_code = v.tenant_code;
    r.tenant_hostname = v.tenant_hostname.value_or("");
    r.admin_username = v.admin_username;
    r.admin_email = v.admin_email;
    r.inherits_admin_password = v.inherits_admin_password;
    r.force_password_change = v.force_password_change;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

seed_profile_entity seed_profile_mapper::map(const domain::seed_profile& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    seed_profile_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.code = v.code;

    r.name = v.name;
    r.description = v.description;
    r.audience = v.audience;
    r.tenant_name = v.tenant_name;
    r.tenant_code = v.tenant_code;
    r.tenant_hostname = v.tenant_hostname.empty() ? std::nullopt : std::optional(v.tenant_hostname);
    r.admin_username = v.admin_username;
    r.admin_email = v.admin_email;
    r.inherits_admin_password = v.inherits_admin_password;
    r.force_password_change = v.force_password_change;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::seed_profile>
seed_profile_mapper::map(const std::vector<seed_profile_entity>& v) {
    return map_vector<seed_profile_entity, domain::seed_profile>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<seed_profile_entity>
seed_profile_mapper::map(const std::vector<domain::seed_profile>& v) {
    return map_vector<domain::seed_profile, seed_profile_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
