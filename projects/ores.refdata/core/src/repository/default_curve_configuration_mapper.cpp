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
#include "ores.refdata.core/repository/default_curve_configuration_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.refdata.api/domain/default_curve_configuration_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/default_curve_configuration_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::default_curve_configuration
default_curve_configuration_mapper::map(const default_curve_configuration_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::default_curve_configuration r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.is_inline = v.is_inline;
    r.priority = v.priority;
    r.default_curve_type = v.default_curve_type;
    r.discount_curve = v.discount_curve;
    r.day_counter = v.day_counter;
    r.recovery_rate = v.recovery_rate;
    r.start_date = v.start_date;
    r.has_quotes = v.has_quotes;
    r.benchmark_curve = v.benchmark_curve;
    r.reinterpreted_yield_curve = v.reinterpreted_yield_curve;
    r.source_curve = v.source_curve;
    r.pillars = v.pillars;
    r.spot_lag = v.spot_lag;
    r.calendar = v.calendar;
    r.conventions = v.conventions;
    r.extrapolation = v.extrapolation;
    r.running_spread = v.running_spread;
    r.index_term = v.index_term;
    r.imply_default_from_market = v.imply_default_from_market;
    r.allow_negative_rates = v.allow_negative_rates;
    r.price_is_upfront = v.price_is_upfront;
    r.initial_state = v.initial_state;
    r.states = v.states;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

default_curve_configuration_entity
default_curve_configuration_mapper::map(const domain::default_curve_configuration& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    default_curve_configuration_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.is_inline = v.is_inline;
    r.priority = v.priority;
    r.default_curve_type = v.default_curve_type;
    r.discount_curve = v.discount_curve;
    r.day_counter = v.day_counter;
    r.recovery_rate = v.recovery_rate;
    r.start_date = v.start_date;
    r.has_quotes = v.has_quotes;
    r.benchmark_curve = v.benchmark_curve;
    r.reinterpreted_yield_curve = v.reinterpreted_yield_curve;
    r.source_curve = v.source_curve;
    r.pillars = v.pillars;
    r.spot_lag = v.spot_lag;
    r.calendar = v.calendar;
    r.conventions = v.conventions;
    r.extrapolation = v.extrapolation;
    r.running_spread = v.running_spread;
    r.index_term = v.index_term;
    r.imply_default_from_market = v.imply_default_from_market;
    r.allow_negative_rates = v.allow_negative_rates;
    r.price_is_upfront = v.price_is_upfront;
    r.initial_state = v.initial_state;
    r.states = v.states;
    r.position = v.position;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::default_curve_configuration>
default_curve_configuration_mapper::map(const std::vector<default_curve_configuration_entity>& v) {
    return map_vector<default_curve_configuration_entity, domain::default_curve_configuration>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<default_curve_configuration_entity>
default_curve_configuration_mapper::map(const std::vector<domain::default_curve_configuration>& v) {
    return map_vector<domain::default_curve_configuration, default_curve_configuration_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
