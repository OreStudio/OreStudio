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
#include "ores.refdata.core/repository/swaption_proxy_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/swaption_proxy_config.hpp"
#include "ores.refdata.api/domain/swaption_proxy_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/swaption_proxy_config_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swaption_proxy_config
swaption_proxy_config_mapper::map(const swaption_proxy_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swaption_proxy_config r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.swaption_volatility_config_id = v.swaption_volatility_config_id;
    r.party_id = v.party_id;
    r.source_curve_id = v.source_curve_id;
    r.source_short_swap_index_base = v.source_short_swap_index_base;
    r.source_swap_index_base = v.source_swap_index_base;
    r.target_short_swap_index_base = v.target_short_swap_index_base;
    r.target_swap_index_base = v.target_swap_index_base;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swaption_proxy_config_entity
swaption_proxy_config_mapper::map(const domain::swaption_proxy_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swaption_proxy_config_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.swaption_volatility_config_id = v.swaption_volatility_config_id;
    r.party_id = v.party_id;
    r.source_curve_id = v.source_curve_id;
    r.source_short_swap_index_base = v.source_short_swap_index_base;
    r.source_swap_index_base = v.source_swap_index_base;
    r.target_short_swap_index_base = v.target_short_swap_index_base;
    r.target_swap_index_base = v.target_swap_index_base;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::swaption_proxy_config>
swaption_proxy_config_mapper::map(const std::vector<swaption_proxy_config_entity>& v) {
    return map_vector<swaption_proxy_config_entity, domain::swaption_proxy_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<swaption_proxy_config_entity>
swaption_proxy_config_mapper::map(const std::vector<domain::swaption_proxy_config>& v) {
    return map_vector<domain::swaption_proxy_config, swaption_proxy_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
