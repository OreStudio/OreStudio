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
#include "ores.iam/repository/tenant_status_mapper.hpp"

#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam/domain/tenant_status_json_io.hpp" // IWYU pragma: keep.

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::tenant_status
tenant_status_mapper::map(const tenant_status_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::tenant_status r;
    r.version = v.version;
    r.status = v.status.value();
    r.name = v.name;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from.value());

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

tenant_status_entity
tenant_status_mapper::map(const domain::tenant_status& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    tenant_status_entity r;
    r.status = v.status;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::tenant_status>
tenant_status_mapper::map(const std::vector<tenant_status_entity>& v) {
    return map_vector<tenant_status_entity, domain::tenant_status>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<tenant_status_entity>
tenant_status_mapper::map(const std::vector<domain::tenant_status>& v) {
    return map_vector<domain::tenant_status, tenant_status_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
