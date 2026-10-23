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
#include "ores.reporting.core/presentation/engine_settings_building_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/engine_settings_building.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::reporting::presentation {

std::vector<ores::diff::domain::field_value>
render_engine_settings_building_fields(const domain::engine_settings_building& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Engine Settings ID", .value = boost::uuids::to_string(v.engine_settings_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Observation Model", .value = v.observation_model.value_or(std::string{})});
    fields.push_back({.name = "Lazy Market Building",
                      .value = v.lazy_market_building ?
                                   (*v.lazy_market_building ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Build Failed Trades",
                      .value = v.build_failed_trades ? (*v.build_failed_trades ? "true" : "false") :
                                                       std::string{}});
    fields.push_back(
        {.name = "Continue On Error",
         .value = v.continue_on_error ? (*v.continue_on_error ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Use At Par Coupons Curves",
                      .value = v.use_at_par_coupons_curves ?
                                   (*v.use_at_par_coupons_curves ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Use At Par Coupons Trades",
                      .value = v.use_at_par_coupons_trades ?
                                   (*v.use_at_par_coupons_trades ? "true" : "false") :
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
