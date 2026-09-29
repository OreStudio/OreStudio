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
#include "ores.refdata.core/presentation/cross_currency_basis_convention_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_cross_currency_basis_convention_fields(const domain::cross_currency_basis_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = v.id});
    fields.push_back({.name = "Settlement Days", .value = std::to_string(v.settlement_days)});
    fields.push_back(
        {.name = "Settlement Calendar", .value = v.settlement_calendar.value_or(std::string{})});
    fields.push_back({.name = "Roll Convention", .value = v.roll_convention});
    fields.push_back({.name = "Flat Index", .value = v.flat_index});
    fields.push_back({.name = "Spread Index", .value = v.spread_index});
    fields.push_back({.name = "Eom", .value = v.eom ? (*v.eom ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Is Resettable",
         .value = v.is_resettable ? (*v.is_resettable ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Flat Index Is Resettable",
                      .value = v.flat_index_is_resettable ?
                                   (*v.flat_index_is_resettable ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Flat Tenor", .value = v.flat_tenor.value_or(std::string{})});
    fields.push_back({.name = "Spread Tenor", .value = v.spread_tenor.value_or(std::string{})});
    fields.push_back(
        {.name = "Spread Payment Lag",
         .value = v.spread_payment_lag ? std::to_string(*v.spread_payment_lag) : std::string{}});
    fields.push_back(
        {.name = "Flat Payment Lag",
         .value = v.flat_payment_lag ? std::to_string(*v.flat_payment_lag) : std::string{}});
    fields.push_back({.name = "Spread Include Spread",
                      .value = v.spread_include_spread ?
                                   (*v.spread_include_spread ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Spread Lookback", .value = v.spread_lookback.value_or(std::string{})});
    fields.push_back(
        {.name = "Spread Fixing Days",
         .value = v.spread_fixing_days ? std::to_string(*v.spread_fixing_days) : std::string{}});
    fields.push_back(
        {.name = "Spread Rate Cutoff",
         .value = v.spread_rate_cutoff ? std::to_string(*v.spread_rate_cutoff) : std::string{}});
    fields.push_back({.name = "Spread Is Averaged",
                      .value = v.spread_is_averaged ? (*v.spread_is_averaged ? "true" : "false") :
                                                      std::string{}});
    fields.push_back({.name = "Spread Observation Shift",
                      .value = v.spread_observation_shift ?
                                   (*v.spread_observation_shift ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Flat Include Spread",
                      .value = v.flat_include_spread ? (*v.flat_include_spread ? "true" : "false") :
                                                       std::string{}});
    fields.push_back({.name = "Flat Lookback", .value = v.flat_lookback.value_or(std::string{})});
    fields.push_back(
        {.name = "Flat Fixing Days",
         .value = v.flat_fixing_days ? std::to_string(*v.flat_fixing_days) : std::string{}});
    fields.push_back(
        {.name = "Flat Rate Cutoff",
         .value = v.flat_rate_cutoff ? std::to_string(*v.flat_rate_cutoff) : std::string{}});
    fields.push_back(
        {.name = "Flat Is Averaged",
         .value = v.flat_is_averaged ? (*v.flat_is_averaged ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Flat Observation Shift",
                      .value = v.flat_observation_shift ?
                                   (*v.flat_observation_shift ? "true" : "false") :
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
