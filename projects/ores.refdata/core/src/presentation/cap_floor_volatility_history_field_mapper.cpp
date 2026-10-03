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
#include "ores.refdata.core/presentation/cap_floor_volatility_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_cap_floor_volatility_fields(const domain::cap_floor_volatility& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back(
        {.name = "Volatility Type", .value = v.volatility_type.value_or(std::string{})});
    fields.push_back({.name = "Output Volatility Type",
                      .value = v.output_volatility_type.value_or(std::string{})});
    fields.push_back({.name = "Model Shift",
                      .value = v.model_shift ? std::to_string(*v.model_shift) : std::string{}});
    fields.push_back({.name = "Output Shift",
                      .value = v.output_shift ? std::to_string(*v.output_shift) : std::string{}});
    fields.push_back({.name = "Extrapolation", .value = v.extrapolation.value_or(std::string{})});
    fields.push_back(
        {.name = "Interpolation Method", .value = v.interpolation_method.value_or(std::string{})});
    fields.push_back({.name = "Include Atm", .value = v.include_atm.value_or(std::string{})});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back({.name = "Business Day Convention",
                      .value = v.business_day_convention.value_or(std::string{})});
    fields.push_back({.name = "Tenors", .value = v.tenors.value_or(std::string{})});
    fields.push_back({.name = "Strikes", .value = v.strikes.value_or(std::string{})});
    fields.push_back(
        {.name = "Optional Quotes", .value = v.optional_quotes.value_or(std::string{})});
    fields.push_back({.name = "Ibor Index", .value = v.ibor_index.value_or(std::string{})});
    fields.push_back({.name = "Index", .value = v.index.value_or(std::string{})});
    fields.push_back({.name = "Rate Computation Period",
                      .value = v.rate_computation_period.value_or(std::string{})});
    fields.push_back({.name = "On Cap Settlement Days",
                      .value = v.on_cap_settlement_days ?
                                   std::to_string(*v.on_cap_settlement_days) :
                                   std::string{}});
    fields.push_back({.name = "Discount Curve", .value = v.discount_curve.value_or(std::string{})});
    fields.push_back({.name = "Atm Tenors", .value = v.atm_tenors.value_or(std::string{})});
    fields.push_back(
        {.name = "Settlement Days",
         .value = v.settlement_days ? std::to_string(*v.settlement_days) : std::string{}});
    fields.push_back({.name = "Interpolate On", .value = v.interpolate_on.value_or(std::string{})});
    fields.push_back(
        {.name = "Time Interpolation", .value = v.time_interpolation.value_or(std::string{})});
    fields.push_back(
        {.name = "Strike Interpolation", .value = v.strike_interpolation.value_or(std::string{})});
    fields.push_back({.name = "Input Type", .value = v.input_type.value_or(std::string{})});
    fields.push_back({.name = "Quote Includes Index Name",
                      .value = v.quote_includes_index_name.value_or(std::string{})});
    fields.push_back(
        {.name = "Flat First Period", .value = v.flat_first_period.value_or(std::string{})});
    fields.push_back({.name = "Use Effecive Volatility",
                      .value = v.use_effecive_volatility.value_or(std::string{})});
    fields.push_back({.name = "Use Effective Volatility",
                      .value = v.use_effective_volatility.value_or(std::string{})});
    fields.push_back({.name = "Has Proxy Config", .value = v.has_proxy_config ? "true" : "false"});
    fields.push_back({.name = "Proxy Source Curve ID",
                      .value = v.proxy_source_curve_id.value_or(std::string{})});
    fields.push_back(
        {.name = "Proxy Source Index", .value = v.proxy_source_index.value_or(std::string{})});
    fields.push_back({.name = "Proxy Source Rate Computation Period",
                      .value = v.proxy_source_rate_computation_period.value_or(std::string{})});
    fields.push_back(
        {.name = "Proxy Target Index", .value = v.proxy_target_index.value_or(std::string{})});
    fields.push_back({.name = "Proxy Target Rate Computation Period",
                      .value = v.proxy_target_rate_computation_period.value_or(std::string{})});
    fields.push_back({.name = "Proxy Target On Cap Settlement Days",
                      .value = v.proxy_target_on_cap_settlement_days ?
                                   std::to_string(*v.proxy_target_on_cap_settlement_days) :
                                   std::string{}});
    fields.push_back({.name = "Proxy Scaling Factor",
                      .value = v.proxy_scaling_factor ? std::to_string(*v.proxy_scaling_factor) :
                                                        std::string{}});
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
