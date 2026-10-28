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
#include "ores.refdata.core/repository/convention_kind_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/convention_kind.hpp"
#include "ores.refdata.api/domain/convention_kind_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/convention_kind_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::convention_kind convention_kind_mapper::map(const convention_kind_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::convention_kind r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code;

    r.name = v.name;

    r.description = v.description;
    r.asset_class = v.asset_class;
    r.ore_element = v.ore_element;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

convention_kind_entity convention_kind_mapper::map(const domain::convention_kind& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    convention_kind_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.name = v.name;

    r.description = v.description;
    r.asset_class = v.asset_class;
    r.ore_element = v.ore_element;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::convention_kind>
convention_kind_mapper::map(const std::vector<convention_kind_entity>& v) {
    return map_vector<convention_kind_entity, domain::convention_kind>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<convention_kind_entity>
convention_kind_mapper::map(const std::vector<domain::convention_kind>& v) {
    return map_vector<domain::convention_kind, convention_kind_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
