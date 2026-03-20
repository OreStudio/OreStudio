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
#include "ores.variability/repository/system_setting_mapper.hpp"

#include <algorithm>
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.variability/domain/system_setting_json_io.hpp" // IWYU pragma: keep.

namespace ores::variability::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::system_setting system_setting_mapper::map(const system_setting_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::system_setting r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.name = v.name.value();
    r.value = v.value;
    r.data_type = v.data_type;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

system_setting_entity system_setting_mapper::map(const domain::system_setting& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    system_setting_entity r;
    r.name = v.name;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.value = v.value;
    r.data_type = v.data_type;
    r.description = v.description;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    // Note: recorded_at is read-only; valid_from/valid_to are managed by triggers

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::system_setting>
system_setting_mapper::map(const std::vector<system_setting_entity>& v) {
    return map_vector<system_setting_entity, domain::system_setting>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<system_setting_entity>
system_setting_mapper::map(const std::vector<domain::system_setting>& v) {
    return map_vector<domain::system_setting, system_setting_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

}
