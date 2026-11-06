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
#include "ores.trading.core/presentation/instrument_underlying_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/instrument_underlying.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_instrument_underlying_fields(const domain::instrument_underlying& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.trade_id)});
    fields.push_back(
        {.name = "Trade Activity ID", .value = boost::uuids::to_string(v.trade_activity_id)});
    fields.push_back({.name = "Underlying Type", .value = v.underlying_type});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back(
        {.name = "Identifier Type", .value = v.identifier_type.value_or(std::string{})});
    fields.push_back({.name = "Currency", .value = v.currency.value_or(std::string{})});
    fields.push_back({.name = "Exchange", .value = v.exchange.value_or(std::string{})});
    fields.push_back({.name = "Weight", .value = v.weight ? v.weight->to_string() : std::string{}});
    fields.push_back({.name = "Price Type", .value = v.price_type.value_or(std::string{})});
    fields.push_back(
        {.name = "Future Month Offset",
         .value = v.future_month_offset ? std::to_string(*v.future_month_offset) : std::string{}});
    fields.push_back(
        {.name = "Delivery Roll Days",
         .value = v.delivery_roll_days ? std::to_string(*v.delivery_roll_days) : std::string{}});
    fields.push_back({.name = "Delivery Roll Calendar",
                      .value = v.delivery_roll_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Future Expiry Date", .value = v.future_expiry_date.value_or(std::string{})});
    fields.push_back({.name = "Future Contract Month",
                      .value = v.future_contract_month.value_or(std::string{})});
    fields.push_back({.name = "Interpolation", .value = v.interpolation.value_or(std::string{})});
    fields.push_back(
        {.name = "Bid Ask Adjustment",
         .value = v.bid_ask_adjustment ? v.bid_ask_adjustment->to_string() : std::string{}});
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
