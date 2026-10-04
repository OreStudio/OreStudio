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
#include "ores.refdata.core/repository/inflation_curve_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/inflation_curve_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::inflation_curve_config
inflation_curve_config_mapper::map(const inflation_curve_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::inflation_curve_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.nominal_term_structure = v.nominal_term_structure;
    r.inflation_type = v.inflation_type;
    r.conventions = v.conventions;
    r.has_quotes = v.has_quotes;
    r.extrapolation = v.extrapolation;
    r.calendar = v.calendar;
    r.day_counter = v.day_counter;
    r.lag = v.lag;
    r.frequency = v.frequency;
    r.base_rate = v.base_rate;
    r.tolerance = v.tolerance;
    r.has_seasonality = v.has_seasonality;
    r.seasonality_base_date = v.seasonality_base_date;
    r.seasonality_frequency = v.seasonality_frequency;
    r.use_last_fixing_date = v.use_last_fixing_date;
    r.interpolation_variable = v.interpolation_variable;
    r.interpolation_method = v.interpolation_method;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

inflation_curve_config_entity
inflation_curve_config_mapper::map(const domain::inflation_curve_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    inflation_curve_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.nominal_term_structure = v.nominal_term_structure;
    r.inflation_type = v.inflation_type;
    r.conventions = v.conventions;
    r.has_quotes = v.has_quotes;
    r.extrapolation = v.extrapolation;
    r.calendar = v.calendar;
    r.day_counter = v.day_counter;
    r.lag = v.lag;
    r.frequency = v.frequency;
    r.base_rate = v.base_rate;
    r.tolerance = v.tolerance;
    r.has_seasonality = v.has_seasonality;
    r.seasonality_base_date = v.seasonality_base_date;
    r.seasonality_frequency = v.seasonality_frequency;
    r.use_last_fixing_date = v.use_last_fixing_date;
    r.interpolation_variable = v.interpolation_variable;
    r.interpolation_method = v.interpolation_method;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::inflation_curve_config>
inflation_curve_config_mapper::map(const std::vector<inflation_curve_config_entity>& v) {
    return map_vector<inflation_curve_config_entity, domain::inflation_curve_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<inflation_curve_config_entity>
inflation_curve_config_mapper::map(const std::vector<domain::inflation_curve_config>& v) {
    return map_vector<domain::inflation_curve_config, inflation_curve_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
