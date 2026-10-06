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
#include "ores.refdata.core/repository/fx_volatility_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/fx_volatility_config.hpp"
#include "ores.refdata.api/domain/fx_volatility_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/fx_volatility_config_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::fx_volatility_config
fx_volatility_config_mapper::map(const fx_volatility_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::fx_volatility_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.dimension = v.dimension;
    r.smile_type = v.smile_type;
    r.smile_interpolation = v.smile_interpolation;
    r.deltas = v.deltas;
    r.smile_delta = v.smile_delta;
    r.conventions = v.conventions;
    r.expiries = v.expiries;
    r.fx_spot_id = v.fx_spot_id;
    r.fx_foreign_curve_id = v.fx_foreign_curve_id;
    r.fx_domestic_curve_id = v.fx_domestic_curve_id;
    r.calendar = v.calendar;
    r.day_counter = v.day_counter;
    r.fx_index_tag = v.fx_index_tag;
    r.base_volatility_1 = v.base_volatility_1;
    r.base_volatility_2 = v.base_volatility_2;
    r.smile_extrapolation = v.smile_extrapolation;
    r.time_interpolation = v.time_interpolation;
    r.time_weighting = v.time_weighting;
    r.butterfly_error_tolerance = v.butterfly_error_tolerance;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

fx_volatility_config_entity
fx_volatility_config_mapper::map(const domain::fx_volatility_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    fx_volatility_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.dimension = v.dimension;
    r.smile_type = v.smile_type;
    r.smile_interpolation = v.smile_interpolation;
    r.deltas = v.deltas;
    r.smile_delta = v.smile_delta;
    r.conventions = v.conventions;
    r.expiries = v.expiries;
    r.fx_spot_id = v.fx_spot_id;
    r.fx_foreign_curve_id = v.fx_foreign_curve_id;
    r.fx_domestic_curve_id = v.fx_domestic_curve_id;
    r.calendar = v.calendar;
    r.day_counter = v.day_counter;
    r.fx_index_tag = v.fx_index_tag;
    r.base_volatility_1 = v.base_volatility_1;
    r.base_volatility_2 = v.base_volatility_2;
    r.smile_extrapolation = v.smile_extrapolation;
    r.time_interpolation = v.time_interpolation;
    r.time_weighting = v.time_weighting;
    r.butterfly_error_tolerance = v.butterfly_error_tolerance;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::fx_volatility_config>
fx_volatility_config_mapper::map(const std::vector<fx_volatility_config_entity>& v) {
    return map_vector<fx_volatility_config_entity, domain::fx_volatility_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<fx_volatility_config_entity>
fx_volatility_config_mapper::map(const std::vector<domain::fx_volatility_config>& v) {
    return map_vector<domain::fx_volatility_config, fx_volatility_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
