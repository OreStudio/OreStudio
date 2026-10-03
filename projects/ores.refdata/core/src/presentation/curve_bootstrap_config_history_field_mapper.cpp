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
#include "ores.refdata.core/presentation/curve_bootstrap_config_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_curve_bootstrap_config_fields(const domain::curve_bootstrap_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back(
        {.name = "Curve Definition ID", .value = boost::uuids::to_string(v.curve_definition_id)});
    fields.push_back({.name = "Default Curve Configuration ID",
                      .value = boost::uuids::to_string(v.default_curve_configuration_id)});
    fields.push_back(
        {.name = "Accuracy", .value = v.accuracy ? std::to_string(*v.accuracy) : std::string{}});
    fields.push_back(
        {.name = "Global Accuracy",
         .value = v.global_accuracy ? std::to_string(*v.global_accuracy) : std::string{}});
    fields.push_back({.name = "Dont Throw",
                      .value = v.dont_throw ? (*v.dont_throw ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Max Attempts",
                      .value = v.max_attempts ? std::to_string(*v.max_attempts) : std::string{}});
    fields.push_back({.name = "Max Factor",
                      .value = v.max_factor ? std::to_string(*v.max_factor) : std::string{}});
    fields.push_back({.name = "Min Factor",
                      .value = v.min_factor ? std::to_string(*v.min_factor) : std::string{}});
    fields.push_back(
        {.name = "Dont Throw Steps",
         .value = v.dont_throw_steps ? std::to_string(*v.dont_throw_steps) : std::string{}});
    fields.push_back(
        {.name = "Global", .value = v.global ? (*v.global ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Smoothness Lambda",
         .value = v.smoothness_lambda ? std::to_string(*v.smoothness_lambda) : std::string{}});
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
