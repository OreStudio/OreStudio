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
#include "ores.trading.core/presentation/bond_leg_rate_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_bond_leg_rate_fields(const domain::bond_leg_rate& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back({.name = "Rate Kind", .value = v.rate_kind});
    fields.push_back({.name = "Index", .value = v.index.value_or(std::string{})});
    fields.push_back(
        {.name = "Is In Arrears",
         .value = v.is_in_arrears ? (*v.is_in_arrears ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Fixing Calendar", .value = v.fixing_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Last Recent Period", .value = v.last_recent_period.value_or(std::string{})});
    fields.push_back({.name = "Last Recent Period Calendar",
                      .value = v.last_recent_period_calendar.value_or(std::string{})});
    fields.push_back({.name = "Lookback", .value = v.lookback.value_or(std::string{})});
    fields.push_back(
        {.name = "Is Averaged",
         .value = v.is_averaged ? (*v.is_averaged ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Has Sub Periods",
         .value = v.has_sub_periods ? (*v.has_sub_periods ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Include Spread",
         .value = v.include_spread ? (*v.include_spread ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Is Not Resetting Xccy",
                      .value = v.is_not_resetting_xccy ?
                                   (*v.is_not_resetting_xccy ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Naked Option",
         .value = v.naked_option ? (*v.naked_option ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Local Cap Floor",
         .value = v.local_cap_floor ? (*v.local_cap_floor ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Stub Use Original Curve",
                      .value = v.stub_use_original_curve ?
                                   (*v.stub_use_original_curve ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Observation Shift",
         .value = v.observation_shift ? (*v.observation_shift ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Front Stub Short Index",
                      .value = v.front_stub_short_index.value_or(std::string{})});
    fields.push_back({.name = "Front Stub Long Index",
                      .value = v.front_stub_long_index.value_or(std::string{})});
    fields.push_back({.name = "Front Stub Rounding Type",
                      .value = v.front_stub_rounding_type.value_or(std::string{})});
    fields.push_back({.name = "Back Stub Short Index",
                      .value = v.back_stub_short_index.value_or(std::string{})});
    fields.push_back(
        {.name = "Back Stub Long Index", .value = v.back_stub_long_index.value_or(std::string{})});
    fields.push_back({.name = "Back Stub Rounding Type",
                      .value = v.back_stub_rounding_type.value_or(std::string{})});
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
