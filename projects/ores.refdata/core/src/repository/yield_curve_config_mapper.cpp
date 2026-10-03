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
#include "ores.refdata.core/repository/yield_curve_config_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/yield_curve_config_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::yield_curve_config yield_curve_config_mapper::map(const yield_curve_config_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::yield_curve_config r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.currency = v.currency;
    r.discount_curve = v.discount_curve;
    r.interpolation_variable = v.interpolation_variable;
    r.interpolation_method = v.interpolation_method;
    r.mixed_interpolation_cutoff = v.mixed_interpolation_cutoff;
    r.day_counter = v.day_counter;
    r.tolerance = v.tolerance;
    r.extrapolation = v.extrapolation;
    r.extrapolation_method = v.extrapolation_method;
    r.exclude_t0_from_interpolation = v.exclude_t0_from_interpolation;
    r.has_report = v.has_report;
    r.report_pillar_dates = v.report_pillar_dates;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

yield_curve_config_entity yield_curve_config_mapper::map(const domain::yield_curve_config& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    yield_curve_config_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.currency = v.currency;
    r.discount_curve = v.discount_curve;
    r.interpolation_variable = v.interpolation_variable;
    r.interpolation_method = v.interpolation_method;
    r.mixed_interpolation_cutoff = v.mixed_interpolation_cutoff;
    r.day_counter = v.day_counter;
    r.tolerance = v.tolerance;
    r.extrapolation = v.extrapolation;
    r.extrapolation_method = v.extrapolation_method;
    r.exclude_t0_from_interpolation = v.exclude_t0_from_interpolation;
    r.has_report = v.has_report;
    r.report_pillar_dates = v.report_pillar_dates;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::yield_curve_config>
yield_curve_config_mapper::map(const std::vector<yield_curve_config_entity>& v) {
    return map_vector<yield_curve_config_entity, domain::yield_curve_config>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<yield_curve_config_entity>
yield_curve_config_mapper::map(const std::vector<domain::yield_curve_config>& v) {
    return map_vector<domain::yield_curve_config, yield_curve_config_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
