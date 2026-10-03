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
#include "ores.refdata.core/repository/swaption_volatility_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/swaption_volatility_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::swaption_volatility_config
swaption_volatility_config_mapper::map(const swaption_volatility_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::swaption_volatility_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.dimension = v.dimension;
    r.volatility_type = v.volatility_type;
    r.interpolation = v.interpolation;
    r.extrapolation = v.extrapolation;
    r.output_volatility_type = v.output_volatility_type;
    r.model_shift = v.model_shift;
    r.output_shift = v.output_shift;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.option_tenors = v.option_tenors;
    r.swap_tenors = v.swap_tenors;
    r.short_swap_index_base = v.short_swap_index_base;
    r.swap_index_base = v.swap_index_base;
    r.smile_option_tenors = v.smile_option_tenors;
    r.smile_swap_tenors = v.smile_swap_tenors;
    r.smile_spreads = v.smile_spreads;
    r.quote_tag = v.quote_tag;
    r.has_proxy_config = v.has_proxy_config;
    r.proxy_source_curve_id = v.proxy_source_curve_id;
    r.proxy_source_short_swap_index_base = v.proxy_source_short_swap_index_base;
    r.proxy_source_swap_index_base = v.proxy_source_swap_index_base;
    r.proxy_target_short_swap_index_base = v.proxy_target_short_swap_index_base;
    r.proxy_target_swap_index_base = v.proxy_target_swap_index_base;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

swaption_volatility_config_entity
swaption_volatility_config_mapper::map(const domain::swaption_volatility_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    swaption_volatility_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.dimension = v.dimension;
    r.volatility_type = v.volatility_type;
    r.interpolation = v.interpolation;
    r.extrapolation = v.extrapolation;
    r.output_volatility_type = v.output_volatility_type;
    r.model_shift = v.model_shift;
    r.output_shift = v.output_shift;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.option_tenors = v.option_tenors;
    r.swap_tenors = v.swap_tenors;
    r.short_swap_index_base = v.short_swap_index_base;
    r.swap_index_base = v.swap_index_base;
    r.smile_option_tenors = v.smile_option_tenors;
    r.smile_swap_tenors = v.smile_swap_tenors;
    r.smile_spreads = v.smile_spreads;
    r.quote_tag = v.quote_tag;
    r.has_proxy_config = v.has_proxy_config;
    r.proxy_source_curve_id = v.proxy_source_curve_id;
    r.proxy_source_short_swap_index_base = v.proxy_source_short_swap_index_base;
    r.proxy_source_swap_index_base = v.proxy_source_swap_index_base;
    r.proxy_target_short_swap_index_base = v.proxy_target_short_swap_index_base;
    r.proxy_target_swap_index_base = v.proxy_target_swap_index_base;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::swaption_volatility_config>
swaption_volatility_config_mapper::map(const std::vector<swaption_volatility_config_entity>& v) {
    return map_vector<swaption_volatility_config_entity, domain::swaption_volatility_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<swaption_volatility_config_entity>
swaption_volatility_config_mapper::map(const std::vector<domain::swaption_volatility_config>& v) {
    return map_vector<domain::swaption_volatility_config, swaption_volatility_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
