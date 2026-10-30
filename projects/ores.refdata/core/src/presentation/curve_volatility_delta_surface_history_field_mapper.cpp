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
#include "ores.refdata.core/presentation/curve_volatility_delta_surface_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/curve_volatility_delta_surface.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_curve_volatility_delta_surface_fields(const domain::curve_volatility_delta_surface& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Curve Volatility Kind ID",
                      .value = boost::uuids::to_string(v.curve_volatility_kind_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Delta Type", .value = v.delta_type});
    fields.push_back({.name = "Atm Type", .value = v.atm_type});
    fields.push_back({.name = "Atm Delta Type", .value = v.atm_delta_type.value_or(std::string{})});
    fields.push_back({.name = "Put Deltas", .value = v.put_deltas});
    fields.push_back({.name = "Call Deltas", .value = v.call_deltas});
    fields.push_back({.name = "Expiries", .value = v.expiries});
    fields.push_back({.name = "Time Interpolation", .value = v.time_interpolation});
    fields.push_back({.name = "Strike Interpolation", .value = v.strike_interpolation});
    fields.push_back({.name = "Extrapolation", .value = v.extrapolation ? "true" : "false"});
    fields.push_back({.name = "Time Extrapolation", .value = v.time_extrapolation});
    fields.push_back({.name = "Time Extrapolation Variance",
                      .value = v.time_extrapolation_variance ?
                                   (*v.time_extrapolation_variance ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Strike Extrapolation", .value = v.strike_extrapolation});
    fields.push_back({.name = "Future Price Correction",
                      .value = v.future_price_correction ?
                                   (*v.future_price_correction ? "true" : "false") :
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
