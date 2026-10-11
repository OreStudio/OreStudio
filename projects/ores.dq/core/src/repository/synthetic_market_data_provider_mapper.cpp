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
#include "ores.dq.core/repository/synthetic_market_data_provider_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/synthetic_market_data_provider.hpp"
#include "ores.dq.api/domain/synthetic_market_data_provider_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/synthetic_market_data_provider_entity.hpp"
#include "ores.logging/boost_severity.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <vector>

namespace ores::dq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::synthetic_market_data_provider
synthetic_market_data_provider_mapper::map(const synthetic_market_data_provider_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::synthetic_market_data_provider r;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.code = v.code.value();
    r.name = v.name;
    r.kind = v.kind;
    r.description = v.description;
    r.display_order = v.display_order;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

synthetic_market_data_provider_entity
synthetic_market_data_provider_mapper::map(const domain::synthetic_market_data_provider& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    synthetic_market_data_provider_entity r;
    r.code = v.code;
    r.tenant_id = v.tenant_id.to_string();
    r.name = v.name;
    r.kind = v.kind;
    r.description = v.description;
    r.display_order = v.display_order;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::synthetic_market_data_provider> synthetic_market_data_provider_mapper::map(
    const std::vector<synthetic_market_data_provider_entity>& v) {
    return map_vector<synthetic_market_data_provider_entity,
                      domain::synthetic_market_data_provider>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<synthetic_market_data_provider_entity> synthetic_market_data_provider_mapper::map(
    const std::vector<domain::synthetic_market_data_provider>& v) {
    return map_vector<domain::synthetic_market_data_provider,
                      synthetic_market_data_provider_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
