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
#include "ores.refdata.core/repository/cut_code_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/cut_code.hpp"
#include "ores.refdata.api/domain/cut_code_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/cut_code_entity.hpp"
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cut_code cut_code_mapper::map(const cut_code_entity& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cut_code r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.name = v.name;
    r.description = v.description;
    r.cut_time = ores::platform::time::datetime::from_iso8601_time(v.cut_time);
    r.cut_time_zone = v.cut_time_zone;
    r.fixing_source_code = v.fixing_source_code;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    ORES_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cut_code_entity cut_code_mapper::map(const domain::cut_code& v) {
    ORES_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cut_code_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.description = v.description;
    r.cut_time = ores::platform::time::datetime::to_iso8601_time(v.cut_time);
    r.cut_time_zone = v.cut_time_zone;
    r.fixing_source_code = v.fixing_source_code;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    ORES_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cut_code> cut_code_mapper::map(const std::vector<cut_code_entity>& v) {
    return map_vector<cut_code_entity, domain::cut_code>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cut_code_entity> cut_code_mapper::map(const std::vector<domain::cut_code>& v) {
    return map_vector<domain::cut_code, cut_code_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
