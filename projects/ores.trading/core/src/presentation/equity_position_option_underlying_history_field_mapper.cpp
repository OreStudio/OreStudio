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
#include "ores.trading.core/presentation/equity_position_option_underlying_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_equity_position_option_underlying_fields(
    const domain::equity_position_option_underlying& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back({.name = "Underlying Name", .value = v.underlying_name});
    fields.push_back({.name = "Strike", .value = v.strike.to_string()});
    fields.push_back({.name = "Weight", .value = v.weight ? v.weight->to_string() : std::string{}});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Option Type", .value = v.option_type});
    fields.push_back({.name = "Exercise Type", .value = v.exercise_type});
    fields.push_back({.name = "Settlement Type", .value = v.settlement_type});
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
