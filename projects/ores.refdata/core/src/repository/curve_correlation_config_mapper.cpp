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
#include "ores.refdata.core/repository/curve_correlation_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/curve_correlation_config.hpp"
#include "ores.refdata.api/domain/curve_correlation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/curve_correlation_config_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::curve_correlation_config
curve_correlation_config_mapper::map(const curve_correlation_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::curve_correlation_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.correlation_type = v.correlation_type;
    r.index_1 = v.index_1;
    r.index_2 = v.index_2;
    r.conventions = v.conventions;
    r.swaption_volatility = v.swaption_volatility;
    r.discount_curve = v.discount_curve;
    r.currency = v.currency;
    r.dimension = v.dimension;
    r.quote_type = v.quote_type;
    r.extrapolation = v.extrapolation;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.option_tenors = v.option_tenors;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

curve_correlation_config_entity
curve_correlation_config_mapper::map(const domain::curve_correlation_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    curve_correlation_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.correlation_type = v.correlation_type;
    r.index_1 = v.index_1;
    r.index_2 = v.index_2;
    r.conventions = v.conventions;
    r.swaption_volatility = v.swaption_volatility;
    r.discount_curve = v.discount_curve;
    r.currency = v.currency;
    r.dimension = v.dimension;
    r.quote_type = v.quote_type;
    r.extrapolation = v.extrapolation;
    r.day_counter = v.day_counter;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.option_tenors = v.option_tenors;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::curve_correlation_config>
curve_correlation_config_mapper::map(const std::vector<curve_correlation_config_entity>& v) {
    return map_vector<curve_correlation_config_entity, domain::curve_correlation_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<curve_correlation_config_entity>
curve_correlation_config_mapper::map(const std::vector<domain::curve_correlation_config>& v) {
    return map_vector<domain::curve_correlation_config, curve_correlation_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
