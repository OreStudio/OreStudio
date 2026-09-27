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
#include "ores.analytics.core/presentation/credit_simulation_config_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::analytics::presentation {

std::vector<ores::diff::domain::field_value>
render_credit_simulation_config_fields(const domain::credit_simulation_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "Market", .value = v.market});
    fields.push_back({.name = "Credit", .value = v.credit});
    fields.push_back({.name = "Zero Market Pnl", .value = v.zero_market_pnl ? "true" : "false"});
    fields.push_back({.name = "Evaluation", .value = v.evaluation});
    fields.push_back({.name = "Double Default", .value = v.double_default ? "true" : "false"});
    fields.push_back({.name = "Seed", .value = std::to_string(v.seed)});
    fields.push_back({.name = "Paths", .value = std::to_string(v.paths)});
    fields.push_back({.name = "Credit Mode", .value = v.credit_mode});
    fields.push_back({.name = "Loan Exposure Mode", .value = v.loan_exposure_mode});
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
