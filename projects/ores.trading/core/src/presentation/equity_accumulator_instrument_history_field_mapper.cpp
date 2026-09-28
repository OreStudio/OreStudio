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
#include "ores.trading.core/presentation/equity_accumulator_instrument_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_equity_accumulator_instrument_fields(const domain::equity_accumulator_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.identity.trade_id)});
    fields.push_back({.name = "Trade Type Code", .value = v.identity.trade_type_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Underlying Name", .value = v.underlying_name});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Strike", .value = v.strike.to_string()});
    fields.push_back({.name = "Fixing Amount", .value = v.fixing_amount.to_string()});
    fields.push_back({.name = "Start Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.start_date)});
    fields.push_back({.name = "Expiry Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.expiry_date)});
    fields.push_back({.name = "Fixing Frequency", .value = v.fixing_frequency});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Knock Out Level",
                      .value = v.knock_out_level ? v.knock_out_level->to_string() : std::string{}});
    fields.push_back({.name = "Target Amount",
                      .value = v.target_amount ? v.target_amount->to_string() : std::string{}});
    fields.push_back({.name = "Target Type", .value = v.target_type});
    fields.push_back({.name = "Payoff Type", .value = v.payoff_type});
    fields.push_back({.name = "Description", .value = v.description});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.audit.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.audit.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.audit.change_reason_code});
    fields.push_back(
        {.name = provenance_fields::change_commentary, .value = v.audit.change_commentary});
    fields.push_back(
        {.name = provenance_fields::recorded_at,
         .value = ores::platform::time::datetime::to_iso8601_utc(v.audit.recorded_at)});

    return fields;
}

}
