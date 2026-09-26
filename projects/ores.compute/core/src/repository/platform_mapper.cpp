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
#include "ores.compute.core/repository/platform_mapper.hpp"
#include "ores.compute.api/domain/platform_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::platform platform_mapper::map(const platform_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::platform r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.code = v.code;

    r.display_name = v.display_name;
    r.description = v.description;
    r.os_family = v.os_family;
    r.cpu_arch = v.cpu_arch;
    r.abi = v.abi.value_or("");
    r.is_active = v.is_active;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

platform_entity platform_mapper::map(const domain::platform& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    platform_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.code = v.code;

    r.display_name = v.display_name;
    r.description = v.description;
    r.os_family = v.os_family;
    r.cpu_arch = v.cpu_arch;
    r.abi = v.abi.empty() ? std::nullopt : std::optional(v.abi);
    r.is_active = v.is_active;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::platform> platform_mapper::map(const std::vector<platform_entity>& v) {
    return map_vector<platform_entity, domain::platform>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<platform_entity> platform_mapper::map(const std::vector<domain::platform>& v) {
    return map_vector<domain::platform, platform_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
