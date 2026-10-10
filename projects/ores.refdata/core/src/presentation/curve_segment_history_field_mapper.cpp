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
 * Template: cpp_history_field_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/presentation/curve_segment_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/curve_segment.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_curve_segment_fields(const domain::curve_segment& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Segment Type", .value = v.segment_type});
    fields.push_back({.name = "Position", .value = std::to_string(v.position)});
    fields.push_back({.name = "Conventions", .value = v.conventions.value_or(std::string{})});
    fields.push_back({.name = "Pillar Choice", .value = v.pillar_choice.value_or(std::string{})});
    fields.push_back(
        {.name = "Priority", .value = v.priority ? std::to_string(*v.priority) : std::string{}});
    fields.push_back({.name = "Min Distance",
                      .value = v.min_distance ? std::to_string(*v.min_distance) : std::string{}});
    fields.push_back(
        {.name = "Projection Curve", .value = v.projection_curve.value_or(std::string{})});
    fields.push_back({.name = "Discount Curve", .value = v.discount_curve.value_or(std::string{})});
    fields.push_back({.name = "Spot Rate", .value = v.spot_rate.value_or(std::string{})});
    fields.push_back({.name = "Projection Curve Domestic",
                      .value = v.projection_curve_domestic.value_or(std::string{})});
    fields.push_back({.name = "Projection Curve Foreign",
                      .value = v.projection_curve_foreign.value_or(std::string{})});
    fields.push_back(
        {.name = "Projection Curve Pay", .value = v.projection_curve_pay.value_or(std::string{})});
    fields.push_back({.name = "Projection Curve Receive",
                      .value = v.projection_curve_receive.value_or(std::string{})});
    fields.push_back({.name = "Projection Curve Long",
                      .value = v.projection_curve_long.value_or(std::string{})});
    fields.push_back({.name = "Projection Curve Short",
                      .value = v.projection_curve_short.value_or(std::string{})});
    fields.push_back(
        {.name = "Reference Curve", .value = v.reference_curve.value_or(std::string{})});
    fields.push_back(
        {.name = "Reference Curve 2", .value = v.reference_curve_2.value_or(std::string{})});
    fields.push_back(
        {.name = "Weight 1", .value = v.weight_1 ? std::to_string(*v.weight_1) : std::string{}});
    fields.push_back(
        {.name = "Weight 2", .value = v.weight_2 ? std::to_string(*v.weight_2) : std::string{}});
    fields.push_back({.name = "Ibor Index", .value = v.ibor_index.value_or(std::string{})});
    fields.push_back({.name = "Rfr Curve", .value = v.rfr_curve.value_or(std::string{})});
    fields.push_back({.name = "Rfr Index", .value = v.rfr_index.value_or(std::string{})});
    fields.push_back({.name = "Spread", .value = v.spread ? v.spread->to_string() : std::string{}});
    fields.push_back({.name = "Base Curve", .value = v.base_curve.value_or(std::string{})});
    fields.push_back(
        {.name = "Base Curve Currency", .value = v.base_curve_currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Numerator Curve", .value = v.numerator_curve.value_or(std::string{})});
    fields.push_back({.name = "Numerator Curve Currency",
                      .value = v.numerator_curve_currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Denominator Curve", .value = v.denominator_curve.value_or(std::string{})});
    fields.push_back({.name = "Denominator Curve Currency",
                      .value = v.denominator_curve_currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Extrapolate Flat",
         .value = v.extrapolate_flat ? (*v.extrapolate_flat ? "true" : "false") : std::string{}});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
