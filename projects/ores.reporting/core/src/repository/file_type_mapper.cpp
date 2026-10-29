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
#include "ores.reporting.core/repository/file_type_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/file_type.hpp"
#include "ores.reporting.api/domain/file_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/file_type_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::file_type file_type_mapper::map(const file_type_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::file_type r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.analytic_type_code = v.analytic_type_code;
    r.ore_parameter = v.ore_parameter;
    r.direction = v.direction;
    r.format = v.format;
    r.xsd_file = v.xsd_file;
    r.kind = v.kind;
    r.configuration_type_code = v.configuration_type_code;
    r.name_pattern = v.name_pattern;
    r.is_default = v.is_default;
    r.produced_by_file_type_code = v.produced_by_file_type_code;
    r.sample_count = v.sample_count;
    r.sample_total = v.sample_total;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

file_type_entity file_type_mapper::map(const domain::file_type& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    file_type_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.analytic_type_code = v.analytic_type_code;
    r.ore_parameter = v.ore_parameter;
    r.direction = v.direction;
    r.format = v.format;
    r.xsd_file = v.xsd_file;
    r.kind = v.kind;
    r.configuration_type_code = v.configuration_type_code;
    r.name_pattern = v.name_pattern;
    r.is_default = v.is_default;
    r.produced_by_file_type_code = v.produced_by_file_type_code;
    r.sample_count = v.sample_count;
    r.sample_total = v.sample_total;
    r.description = v.description;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::file_type> file_type_mapper::map(const std::vector<file_type_entity>& v) {
    return map_vector<file_type_entity, domain::file_type>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<file_type_entity> file_type_mapper::map(const std::vector<domain::file_type>& v) {
    return map_vector<domain::file_type, file_type_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
