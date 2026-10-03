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
#include "ores.refdata.core/presentation/equity_volatility_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_equity_volatility_fields(const domain::equity_volatility& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Equity ID", .value = v.equity_id.value_or(std::string{})});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Dimension", .value = v.dimension.value_or(std::string{})});
    fields.push_back({.name = "Expiries", .value = v.expiries.value_or(std::string{})});
    fields.push_back({.name = "Strikes", .value = v.strikes.value_or(std::string{})});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back(
        {.name = "Time Extrapolation", .value = v.time_extrapolation.value_or(std::string{})});
    fields.push_back(
        {.name = "Strike Extrapolation", .value = v.strike_extrapolation.value_or(std::string{})});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back({.name = "Prefer Out Of The Money",
                      .value = v.prefer_out_of_the_money.value_or(std::string{})});
    fields.push_back(
        {.name = "Has Volatility Config", .value = v.has_volatility_config ? "true" : "false"});
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
