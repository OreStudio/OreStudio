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
#include "ores.refdata.core/repository/cap_floor_volatility_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/cap_floor_volatility_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::cap_floor_volatility_config
cap_floor_volatility_config_mapper::map(const cap_floor_volatility_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::cap_floor_volatility_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.volatility_type = v.volatility_type;
    r.output_volatility_type = v.output_volatility_type;
    r.model_shift = v.model_shift;
    r.output_shift = v.output_shift;
    r.extrapolation = v.extrapolation;
    r.interpolation_method = v.interpolation_method;
    r.include_atm = v.include_atm;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.tenors = v.tenors;
    r.strikes = v.strikes;
    r.optional_quotes = v.optional_quotes;
    r.ibor_index = v.ibor_index;
    r.index = v.index;
    r.rate_computation_period = v.rate_computation_period;
    r.on_cap_settlement_days = v.on_cap_settlement_days;
    r.discount_curve = v.discount_curve;
    r.atm_tenors = v.atm_tenors;
    r.settlement_days = v.settlement_days;
    r.interpolate_on = v.interpolate_on;
    r.time_interpolation = v.time_interpolation;
    r.strike_interpolation = v.strike_interpolation;
    r.input_type = v.input_type;
    r.quote_includes_index_name = v.quote_includes_index_name;
    r.flat_first_period = v.flat_first_period;
    r.use_effecive_volatility = v.use_effecive_volatility;
    r.use_effective_volatility = v.use_effective_volatility;
    r.has_proxy_config = v.has_proxy_config;
    r.proxy_source_curve_id = v.proxy_source_curve_id;
    r.proxy_source_index = v.proxy_source_index;
    r.proxy_source_rate_computation_period = v.proxy_source_rate_computation_period;
    r.proxy_target_index = v.proxy_target_index;
    r.proxy_target_rate_computation_period = v.proxy_target_rate_computation_period;
    r.proxy_target_on_cap_settlement_days = v.proxy_target_on_cap_settlement_days;
    r.proxy_scaling_factor = v.proxy_scaling_factor;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

cap_floor_volatility_config_entity
cap_floor_volatility_config_mapper::map(const domain::cap_floor_volatility_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    cap_floor_volatility_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.volatility_type = v.volatility_type;
    r.output_volatility_type = v.output_volatility_type;
    r.model_shift = v.model_shift;
    r.output_shift = v.output_shift;
    r.extrapolation = v.extrapolation;
    r.interpolation_method = v.interpolation_method;
    r.include_atm = v.include_atm;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.tenors = v.tenors;
    r.strikes = v.strikes;
    r.optional_quotes = v.optional_quotes;
    r.ibor_index = v.ibor_index;
    r.index = v.index;
    r.rate_computation_period = v.rate_computation_period;
    r.on_cap_settlement_days = v.on_cap_settlement_days;
    r.discount_curve = v.discount_curve;
    r.atm_tenors = v.atm_tenors;
    r.settlement_days = v.settlement_days;
    r.interpolate_on = v.interpolate_on;
    r.time_interpolation = v.time_interpolation;
    r.strike_interpolation = v.strike_interpolation;
    r.input_type = v.input_type;
    r.quote_includes_index_name = v.quote_includes_index_name;
    r.flat_first_period = v.flat_first_period;
    r.use_effecive_volatility = v.use_effecive_volatility;
    r.use_effective_volatility = v.use_effective_volatility;
    r.has_proxy_config = v.has_proxy_config;
    r.proxy_source_curve_id = v.proxy_source_curve_id;
    r.proxy_source_index = v.proxy_source_index;
    r.proxy_source_rate_computation_period = v.proxy_source_rate_computation_period;
    r.proxy_target_index = v.proxy_target_index;
    r.proxy_target_rate_computation_period = v.proxy_target_rate_computation_period;
    r.proxy_target_on_cap_settlement_days = v.proxy_target_on_cap_settlement_days;
    r.proxy_scaling_factor = v.proxy_scaling_factor;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::cap_floor_volatility_config>
cap_floor_volatility_config_mapper::map(const std::vector<cap_floor_volatility_config_entity>& v) {
    return map_vector<cap_floor_volatility_config_entity, domain::cap_floor_volatility_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<cap_floor_volatility_config_entity>
cap_floor_volatility_config_mapper::map(const std::vector<domain::cap_floor_volatility_config>& v) {
    return map_vector<domain::cap_floor_volatility_config, cap_floor_volatility_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
