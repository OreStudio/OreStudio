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
#include "ores.trading.core/presentation/trade_booking_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_trade_booking_fields(const domain::trade_booking& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.trade_id)});
    fields.push_back(
        {.name = "Trade Activity ID", .value = boost::uuids::to_string(v.trade_activity_id)});
    fields.push_back({.name = "Book ID", .value = boost::uuids::to_string(v.book_id)});
    fields.push_back(
        {.name = "Netting Set ID",
         .value = v.netting_set_id ? boost::uuids::to_string(*v.netting_set_id) : std::string{}});
    fields.push_back({.name = "Counterparty Identifier ID",
                      .value = v.counterparty_identifier_id ?
                                   boost::uuids::to_string(*v.counterparty_identifier_id) :
                                   std::string{}});
    fields.push_back({.name = "Netting Set Identifier ID",
                      .value = v.netting_set_identifier_id ?
                                   boost::uuids::to_string(*v.netting_set_identifier_id) :
                                   std::string{}});
    fields.push_back({.name = "Trade Date",
                      .value = v.trade_date ?
                                   ores::platform::time::datetime::to_iso8601_date(*v.trade_date) :
                                   std::string{}});
    fields.push_back(
        {.name = "Execution Timestamp",
         .value = v.execution_timestamp ?
                      ores::platform::time::datetime::to_iso8601_utc(*v.execution_timestamp) :
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
