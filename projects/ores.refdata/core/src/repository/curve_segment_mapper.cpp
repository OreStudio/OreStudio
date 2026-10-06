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
#include "ores.refdata.core/repository/curve_segment_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/curve_segment.hpp"
#include "ores.refdata.api/domain/curve_segment_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/curve_segment_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::curve_segment curve_segment_mapper::map(const curve_segment_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::curve_segment r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.curve_definition_id = boost::lexical_cast<boost::uuids::uuid>(v.curve_definition_id);
    r.segment_type = v.segment_type;
    r.position = v.position;
    r.conventions = v.conventions;
    r.pillar_choice = v.pillar_choice;
    r.priority = v.priority;
    r.min_distance = v.min_distance;
    r.projection_curve = v.projection_curve;
    r.discount_curve = v.discount_curve;
    r.spot_rate = v.spot_rate;
    r.projection_curve_domestic = v.projection_curve_domestic;
    r.projection_curve_foreign = v.projection_curve_foreign;
    r.projection_curve_pay = v.projection_curve_pay;
    r.projection_curve_receive = v.projection_curve_receive;
    r.projection_curve_long = v.projection_curve_long;
    r.projection_curve_short = v.projection_curve_short;
    r.reference_curve = v.reference_curve;
    r.reference_curve_2 = v.reference_curve_2;
    r.weight_1 = v.weight_1;
    r.weight_2 = v.weight_2;
    r.ibor_index = v.ibor_index;
    r.rfr_curve = v.rfr_curve;
    r.rfr_index = v.rfr_index;
    r.spread = v.spread;
    r.base_curve = v.base_curve;
    r.base_curve_currency = v.base_curve_currency;
    r.numerator_curve = v.numerator_curve;
    r.numerator_curve_currency = v.numerator_curve_currency;
    r.denominator_curve = v.denominator_curve;
    r.denominator_curve_currency = v.denominator_curve_currency;
    r.extrapolate_flat = v.extrapolate_flat;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

curve_segment_entity curve_segment_mapper::map(const domain::curve_segment& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    curve_segment_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.curve_definition_id = boost::uuids::to_string(v.curve_definition_id);
    r.segment_type = v.segment_type;
    r.position = v.position;
    r.conventions = v.conventions;
    r.pillar_choice = v.pillar_choice;
    r.priority = v.priority;
    r.min_distance = v.min_distance;
    r.projection_curve = v.projection_curve;
    r.discount_curve = v.discount_curve;
    r.spot_rate = v.spot_rate;
    r.projection_curve_domestic = v.projection_curve_domestic;
    r.projection_curve_foreign = v.projection_curve_foreign;
    r.projection_curve_pay = v.projection_curve_pay;
    r.projection_curve_receive = v.projection_curve_receive;
    r.projection_curve_long = v.projection_curve_long;
    r.projection_curve_short = v.projection_curve_short;
    r.reference_curve = v.reference_curve;
    r.reference_curve_2 = v.reference_curve_2;
    r.weight_1 = v.weight_1;
    r.weight_2 = v.weight_2;
    r.ibor_index = v.ibor_index;
    r.rfr_curve = v.rfr_curve;
    r.rfr_index = v.rfr_index;
    r.spread = v.spread;
    r.base_curve = v.base_curve;
    r.base_curve_currency = v.base_curve_currency;
    r.numerator_curve = v.numerator_curve;
    r.numerator_curve_currency = v.numerator_curve_currency;
    r.denominator_curve = v.denominator_curve;
    r.denominator_curve_currency = v.denominator_curve_currency;
    r.extrapolate_flat = v.extrapolate_flat;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::curve_segment>
curve_segment_mapper::map(const std::vector<curve_segment_entity>& v) {
    return map_vector<curve_segment_entity, domain::curve_segment>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<curve_segment_entity>
curve_segment_mapper::map(const std::vector<domain::curve_segment>& v) {
    return map_vector<domain::curve_segment, curve_segment_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
