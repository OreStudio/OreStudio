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
#include "ores.refdata.core/repository/local_jurisdiction_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/local_jurisdiction.hpp"
#include "ores.refdata.api/domain/local_jurisdiction_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/local_jurisdiction_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <optional>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::local_jurisdiction local_jurisdiction_mapper::map(const local_jurisdiction_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::local_jurisdiction r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code;
    r.coding_scheme_code = v.coding_scheme_code;
    r.source = v.source.value_or("");
    r.description = v.description.value_or("");
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

local_jurisdiction_entity local_jurisdiction_mapper::map(const domain::local_jurisdiction& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    local_jurisdiction_entity r;
    r.code = v.code;
    r.coding_scheme_code = v.coding_scheme_code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.source = v.source.empty() ? std::nullopt : std::optional(v.source);
    r.description = v.description.empty() ? std::nullopt : std::optional(v.description);
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::local_jurisdiction>
local_jurisdiction_mapper::map(const std::vector<local_jurisdiction_entity>& v) {
    return map_vector<local_jurisdiction_entity, domain::local_jurisdiction>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<local_jurisdiction_entity>
local_jurisdiction_mapper::map(const std::vector<domain::local_jurisdiction>& v) {
    return map_vector<domain::local_jurisdiction, local_jurisdiction_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
