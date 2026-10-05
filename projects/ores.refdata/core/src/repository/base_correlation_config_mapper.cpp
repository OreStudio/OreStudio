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
#include "ores.refdata.core/repository/base_correlation_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/base_correlation_config.hpp"
#include "ores.refdata.api/domain/base_correlation_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/base_correlation_config_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::base_correlation_config
base_correlation_config_mapper::map(const base_correlation_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::base_correlation_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.terms = v.terms;
    r.detachment_points = v.detachment_points;
    r.settlement_days = v.settlement_days;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.day_counter = v.day_counter;
    r.extrapolate = v.extrapolate;
    r.quote_name = v.quote_name;
    r.start_date = v.start_date;
    r.rule = v.rule;
    r.adjust_for_losses = v.adjust_for_losses;
    r.index_term = v.index_term;
    r.index_spread = v.index_spread;
    r.currency = v.currency;
    r.calibrate_constituents_to_index_spread = v.calibrate_constituents_to_index_spread;
    r.use_assumed_recovery = v.use_assumed_recovery;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

base_correlation_config_entity
base_correlation_config_mapper::map(const domain::base_correlation_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    base_correlation_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.terms = v.terms;
    r.detachment_points = v.detachment_points;
    r.settlement_days = v.settlement_days;
    r.calendar = v.calendar;
    r.business_day_convention = v.business_day_convention;
    r.day_counter = v.day_counter;
    r.extrapolate = v.extrapolate;
    r.quote_name = v.quote_name;
    r.start_date = v.start_date;
    r.rule = v.rule;
    r.adjust_for_losses = v.adjust_for_losses;
    r.index_term = v.index_term;
    r.index_spread = v.index_spread;
    r.currency = v.currency;
    r.calibrate_constituents_to_index_spread = v.calibrate_constituents_to_index_spread;
    r.use_assumed_recovery = v.use_assumed_recovery;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::base_correlation_config>
base_correlation_config_mapper::map(const std::vector<base_correlation_config_entity>& v) {
    return map_vector<base_correlation_config_entity, domain::base_correlation_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<base_correlation_config_entity>
base_correlation_config_mapper::map(const std::vector<domain::base_correlation_config>& v) {
    return map_vector<domain::base_correlation_config, base_correlation_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
