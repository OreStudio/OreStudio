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
#include "ores.refdata.core/presentation/commodity_curve_config_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_commodity_curve_config_fields(const domain::commodity_curve_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back(
        {.name = "Base Price Curve", .value = v.base_price_curve.value_or(std::string{})});
    fields.push_back(
        {.name = "Base Yield Curve", .value = v.base_yield_curve.value_or(std::string{})});
    fields.push_back({.name = "Yield Curve", .value = v.yield_curve.value_or(std::string{})});
    fields.push_back({.name = "Spot Quote", .value = v.spot_quote.value_or(std::string{})});
    fields.push_back({.name = "Has Quotes", .value = v.has_quotes ? "true" : "false"});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back(
        {.name = "Interpolation Method", .value = v.interpolation_method.value_or(std::string{})});
    fields.push_back({.name = "Conventions", .value = v.conventions.value_or(std::string{})});
    fields.push_back({.name = "Extrapolation", .value = v.extrapolation.value_or(std::string{})});
    fields.push_back(
        {.name = "Has Basis Configuration", .value = v.has_basis_configuration ? "true" : "false"});
    fields.push_back({.name = "Basis Base Price Curve",
                      .value = v.basis_base_price_curve.value_or(std::string{})});
    fields.push_back({.name = "Basis Base Price Conventions",
                      .value = v.basis_base_price_conventions.value_or(std::string{})});
    fields.push_back(
        {.name = "Basis Conventions", .value = v.basis_conventions.value_or(std::string{})});
    fields.push_back(
        {.name = "Basis Day Counter", .value = v.basis_day_counter.value_or(std::string{})});
    fields.push_back({.name = "Basis Interpolation Method",
                      .value = v.basis_interpolation_method.value_or(std::string{})});
    fields.push_back(
        {.name = "Basis Add Basis", .value = v.basis_add_basis.value_or(std::string{})});
    fields.push_back(
        {.name = "Basis Month Offset",
         .value = v.basis_month_offset ? std::to_string(*v.basis_month_offset) : std::string{}});
    fields.push_back(
        {.name = "Basis Average Base", .value = v.basis_average_base.value_or(std::string{})});
    fields.push_back({.name = "Basis Price As Historical Fixing",
                      .value = v.basis_price_as_historical_fixing.value_or(std::string{})});
    fields.push_back(
        {.name = "Has Price Segments", .value = v.has_price_segments ? "true" : "false"});
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
