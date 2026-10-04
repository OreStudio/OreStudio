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
#include "ores.refdata.core/presentation/swaption_volatility_config_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_swaption_volatility_config_fields(const domain::swaption_volatility_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Dimension", .value = v.dimension.value_or(std::string{})});
    fields.push_back(
        {.name = "Volatility Type", .value = v.volatility_type.value_or(std::string{})});
    fields.push_back({.name = "Interpolation", .value = v.interpolation.value_or(std::string{})});
    fields.push_back({.name = "Extrapolation", .value = v.extrapolation.value_or(std::string{})});
    fields.push_back({.name = "Output Volatility Type",
                      .value = v.output_volatility_type.value_or(std::string{})});
    fields.push_back({.name = "Model Shift", .value = v.model_shift.value_or(std::string{})});
    fields.push_back({.name = "Output Shift", .value = v.output_shift.value_or(std::string{})});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back({.name = "Business Day Convention",
                      .value = v.business_day_convention.value_or(std::string{})});
    fields.push_back({.name = "Option Tenors", .value = v.option_tenors.value_or(std::string{})});
    fields.push_back({.name = "Swap Tenors", .value = v.swap_tenors.value_or(std::string{})});
    fields.push_back({.name = "Short Swap Index Base",
                      .value = v.short_swap_index_base.value_or(std::string{})});
    fields.push_back(
        {.name = "Swap Index Base", .value = v.swap_index_base.value_or(std::string{})});
    fields.push_back(
        {.name = "Smile Option Tenors", .value = v.smile_option_tenors.value_or(std::string{})});
    fields.push_back(
        {.name = "Smile Swap Tenors", .value = v.smile_swap_tenors.value_or(std::string{})});
    fields.push_back({.name = "Smile Spreads", .value = v.smile_spreads.value_or(std::string{})});
    fields.push_back({.name = "Quote Tag", .value = v.quote_tag.value_or(std::string{})});
    fields.push_back({.name = "Has Proxy Config", .value = v.has_proxy_config ? "true" : "false"});
    fields.push_back({.name = "Proxy Source Curve ID",
                      .value = v.proxy_source_curve_id.value_or(std::string{})});
    fields.push_back({.name = "Proxy Source Short Swap Index Base",
                      .value = v.proxy_source_short_swap_index_base.value_or(std::string{})});
    fields.push_back({.name = "Proxy Source Swap Index Base",
                      .value = v.proxy_source_swap_index_base.value_or(std::string{})});
    fields.push_back({.name = "Proxy Target Short Swap Index Base",
                      .value = v.proxy_target_short_swap_index_base.value_or(std::string{})});
    fields.push_back({.name = "Proxy Target Swap Index Base",
                      .value = v.proxy_target_swap_index_base.value_or(std::string{})});
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
