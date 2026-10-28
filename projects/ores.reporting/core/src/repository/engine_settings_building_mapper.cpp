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
#include "ores.reporting.core/repository/engine_settings_building_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.reporting.api/domain/engine_settings_building.hpp"
#include "ores.reporting.api/domain/engine_settings_building_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.core/repository/engine_settings_building_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::reporting::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::engine_settings_building
engine_settings_building_mapper::map(const engine_settings_building_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::engine_settings_building r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id);
    r.engine_settings_id = boost::lexical_cast<boost::uuids::uuid>(v.engine_settings_id);
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.observation_model = v.observation_model;
    r.lazy_market_building = v.lazy_market_building;
    r.build_failed_trades = v.build_failed_trades;
    r.continue_on_error = v.continue_on_error;
    r.use_at_par_coupons_curves = v.use_at_par_coupons_curves;
    r.use_at_par_coupons_trades = v.use_at_par_coupons_trades;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

engine_settings_building_entity
engine_settings_building_mapper::map(const domain::engine_settings_building& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    engine_settings_building_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.engine_settings_id = boost::uuids::to_string(v.engine_settings_id);
    r.party_id = boost::uuids::to_string(v.party_id);
    r.observation_model = v.observation_model;
    r.lazy_market_building = v.lazy_market_building;
    r.build_failed_trades = v.build_failed_trades;
    r.continue_on_error = v.continue_on_error;
    r.use_at_par_coupons_curves = v.use_at_par_coupons_curves;
    r.use_at_par_coupons_trades = v.use_at_par_coupons_trades;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::engine_settings_building>
engine_settings_building_mapper::map(const std::vector<engine_settings_building_entity>& v) {
    return map_vector<engine_settings_building_entity, domain::engine_settings_building>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<engine_settings_building_entity>
engine_settings_building_mapper::map(const std::vector<domain::engine_settings_building>& v) {
    return map_vector<domain::engine_settings_building, engine_settings_building_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
