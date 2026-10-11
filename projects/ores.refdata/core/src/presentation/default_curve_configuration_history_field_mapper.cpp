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
#include "ores.refdata.core/presentation/default_curve_configuration_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_default_curve_configuration_fields(const domain::default_curve_configuration& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Is Inline", .value = v.is_inline ? "true" : "false"});
    fields.push_back(
        {.name = "Priority", .value = v.priority ? std::to_string(*v.priority) : std::string{}});
    fields.push_back(
        {.name = "Default Curve Type", .value = v.default_curve_type.value_or(std::string{})});
    fields.push_back({.name = "Discount Curve", .value = v.discount_curve.value_or(std::string{})});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back({.name = "Recovery Rate", .value = v.recovery_rate.value_or(std::string{})});
    fields.push_back({.name = "Start Date", .value = v.start_date.value_or(std::string{})});
    fields.push_back({.name = "Has Quotes", .value = v.has_quotes ? "true" : "false"});
    fields.push_back(
        {.name = "Benchmark Curve", .value = v.benchmark_curve.value_or(std::string{})});
    fields.push_back({.name = "Reinterpreted Yield Curve",
                      .value = v.reinterpreted_yield_curve.value_or(std::string{})});
    fields.push_back({.name = "Source Curve", .value = v.source_curve.value_or(std::string{})});
    fields.push_back({.name = "Pillars", .value = v.pillars.value_or(std::string{})});
    fields.push_back(
        {.name = "Spot Lag", .value = v.spot_lag ? std::to_string(*v.spot_lag) : std::string{}});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back({.name = "Conventions", .value = v.conventions.value_or(std::string{})});
    fields.push_back({.name = "Extrapolation", .value = v.extrapolation.value_or(std::string{})});
    fields.push_back({.name = "Running Spread",
                      .value = v.running_spread ? v.running_spread->to_string() : std::string{}});
    fields.push_back({.name = "Index Term", .value = v.index_term.value_or(std::string{})});
    fields.push_back({.name = "Imply Default From Market",
                      .value = v.imply_default_from_market.value_or(std::string{})});
    fields.push_back(
        {.name = "Allow Negative Rates", .value = v.allow_negative_rates.value_or(std::string{})});
    fields.push_back(
        {.name = "Price Is Upfront", .value = v.price_is_upfront.value_or(std::string{})});
    fields.push_back({.name = "Initial State", .value = v.initial_state.value_or(std::string{})});
    fields.push_back({.name = "States", .value = v.states.value_or(std::string{})});
    fields.push_back({.name = "Position", .value = std::to_string(v.position)});
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
