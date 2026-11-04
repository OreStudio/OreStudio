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
#include "ores.synthetic.core/presentation/variance_calibration_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.synthetic.api/domain/variance_calibration.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::synthetic::presentation {

std::vector<ores::diff::domain::field_value>
render_variance_calibration_fields(const domain::variance_calibration& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Base Currency Code", .value = v.base_currency_code});
    fields.push_back({.name = "Quote Currency Code", .value = v.quote_currency_code});
    fields.push_back({.name = "Input ID",
                      .value = v.input_id ? boost::uuids::to_string(*v.input_id) : std::string{}});
    fields.push_back({.name = "Method", .value = v.method});
    fields.push_back({.name = "Method Version", .value = std::to_string(v.method_version)});
    fields.push_back(
        {.name = "Alpha", .value = v.alpha ? std::to_string(*v.alpha) : std::string{}});
    fields.push_back(
        {.name = "Gamma", .value = v.gamma ? std::to_string(*v.gamma) : std::string{}});
    fields.push_back({.name = "Beta", .value = v.beta ? std::to_string(*v.beta) : std::string{}});
    fields.push_back({.name = "Persistence",
                      .value = v.persistence ? std::to_string(*v.persistence) : std::string{}});
    fields.push_back(
        {.name = "Long Run Variance",
         .value = v.long_run_variance ? std::to_string(*v.long_run_variance) : std::string{}});
    fields.push_back({.name = "Starting Variance Ratio",
                      .value = v.starting_variance_ratio ?
                                   std::to_string(*v.starting_variance_ratio) :
                                   std::string{}});
    fields.push_back(
        {.name = "Log Likelihood",
         .value = v.log_likelihood ? std::to_string(*v.log_likelihood) : std::string{}});
    fields.push_back({.name = "Returns Used", .value = std::to_string(v.returns_used)});
    fields.push_back({.name = "Starts", .value = std::to_string(v.starts)});
    fields.push_back({.name = "Starts Agreed", .value = v.starts_agreed ? "true" : "false"});
    fields.push_back({.name = "State", .value = v.state});
    fields.push_back({.name = "State Reason", .value = v.state_reason});
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
