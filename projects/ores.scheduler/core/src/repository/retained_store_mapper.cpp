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
#include "ores.scheduler.core/repository/retained_store_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.scheduler.api/domain/retained_store.hpp"
#include "ores.scheduler.api/domain/retained_store_json_io.hpp" // IWYU pragma: keep.
#include "ores.scheduler.core/repository/retained_store_entity.hpp"
#include <vector>

namespace ores::scheduler::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::retained_store retained_store_mapper::map(const retained_store_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::retained_store r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.display_name = v.display_name;
    r.description = v.description;
    r.owner_component = v.owner_component;
    r.scope = v.scope;
    r.target_kind = v.target_kind;
    r.target = v.target;
    r.time_column = v.time_column;
    r.measure_kind = v.measure_kind;
    r.bound_kind = v.bound_kind;
    r.default_action = v.default_action;
    r.default_retention_days = v.default_retention_days;
    r.default_cadence_hours = v.default_cadence_hours;
    r.default_reason = v.default_reason;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

retained_store_entity retained_store_mapper::map(const domain::retained_store& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    retained_store_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.display_name = v.display_name;
    r.description = v.description;
    r.owner_component = v.owner_component;
    r.scope = v.scope;
    r.target_kind = v.target_kind;
    r.target = v.target;
    r.time_column = v.time_column;
    r.measure_kind = v.measure_kind;
    r.bound_kind = v.bound_kind;
    r.default_action = v.default_action;
    r.default_retention_days = v.default_retention_days;
    r.default_cadence_hours = v.default_cadence_hours;
    r.default_reason = v.default_reason;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::retained_store>
retained_store_mapper::map(const std::vector<retained_store_entity>& v) {
    return map_vector<retained_store_entity, domain::retained_store>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<retained_store_entity>
retained_store_mapper::map(const std::vector<domain::retained_store>& v) {
    return map_vector<domain::retained_store, retained_store_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
