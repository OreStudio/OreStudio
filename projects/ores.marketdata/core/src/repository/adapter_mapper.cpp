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
#include "ores.marketdata.core/repository/adapter_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.marketdata.api/domain/adapter.hpp"
#include "ores.marketdata.api/domain/adapter_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/adapter_entity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::marketdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::adapter adapter_mapper::map(const adapter_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::adapter r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.code = v.code;
    r.name = v.name;
    r.module_version = v.module_version;
    r.description = v.description;
    r.process = v.process;
    r.asset_classes = v.asset_classes;
    r.services = v.services;
    r.configuration_schema = v.configuration_schema;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

adapter_entity adapter_mapper::map(const domain::adapter& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    adapter_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.name = v.name;
    r.module_version = v.module_version;
    r.description = v.description;
    r.process = v.process;
    r.asset_classes = v.asset_classes;
    r.services = v.services;
    r.configuration_schema = v.configuration_schema;
    r.display_order = v.display_order;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::adapter> adapter_mapper::map(const std::vector<adapter_entity>& v) {
    return map_vector<adapter_entity, domain::adapter>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<adapter_entity> adapter_mapper::map(const std::vector<domain::adapter>& v) {
    return map_vector<domain::adapter, adapter_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
