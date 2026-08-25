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
#include "ores.compute.core/repository/app_version_platform_mapper.hpp"
#include "ores.compute.api/domain/app_version_platform_json_io.hpp" // IWYU pragma: keep.
#include "ores.database/repository/mapper_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::compute::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::app_version_platform
app_version_platform_mapper::map(const app_version_platform_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::app_version_platform r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.app_version_id = boost::lexical_cast<boost::uuids::uuid>(v.app_version_id.value());
    r.platform_id = boost::lexical_cast<boost::uuids::uuid>(v.platform_id);
    r.package_uri = v.package_uri;
    r.sha256 = v.sha256;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

app_version_platform_entity
app_version_platform_mapper::map(const domain::app_version_platform& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    app_version_platform_entity r;
    r.app_version_id = boost::uuids::to_string(v.app_version_id);
    r.tenant_id = v.tenant_id;
    r.platform_id = boost::uuids::to_string(v.platform_id);
    r.version = v.version;
    r.package_uri = v.package_uri;
    r.sha256 = v.sha256;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::app_version_platform>
app_version_platform_mapper::map(const std::vector<app_version_platform_entity>& v) {
    return map_vector<app_version_platform_entity, domain::app_version_platform>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<app_version_platform_entity>
app_version_platform_mapper::map(const std::vector<domain::app_version_platform>& v) {
    return map_vector<domain::app_version_platform, app_version_platform_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
