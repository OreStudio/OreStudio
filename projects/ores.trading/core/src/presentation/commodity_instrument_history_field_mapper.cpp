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
#include "ores.trading.core/presentation/commodity_instrument_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_commodity_instrument_fields(const domain::commodity_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back(
        {.name = "Instrument ID", .value = boost::uuids::to_string(v.identity.instrument_id)});
    fields.push_back({.name = "Trade Type Code", .value = v.identity.trade_type_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Trade ID",
                      .value = v.identity.trade_id ? boost::uuids::to_string(*v.identity.trade_id) :
                                                     std::string{}});
    fields.push_back({.name = "Commodity Code", .value = v.commodity_code});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Quantity", .value = std::to_string(v.quantity)});
    fields.push_back({.name = "Unit", .value = v.unit});
    fields.push_back({.name = "Start Date", .value = v.start_date});
    fields.push_back({.name = "Maturity Date", .value = v.maturity_date});
    fields.push_back({.name = "Fixed Price",
                      .value = v.fixed_price ? std::to_string(*v.fixed_price) : std::string{}});
    fields.push_back({.name = "Option Type", .value = v.option_type});
    fields.push_back({.name = "Strike Price",
                      .value = v.strike_price ? std::to_string(*v.strike_price) : std::string{}});
    fields.push_back({.name = "Exercise Type", .value = v.exercise_type});
    fields.push_back({.name = "Average Type", .value = v.average_type});
    fields.push_back({.name = "Averaging Start Date", .value = v.averaging_start_date});
    fields.push_back({.name = "Averaging End Date", .value = v.averaging_end_date});
    fields.push_back({.name = "Spread Commodity Code", .value = v.spread_commodity_code});
    fields.push_back({.name = "Spread Amount",
                      .value = v.spread_amount ? std::to_string(*v.spread_amount) : std::string{}});
    fields.push_back({.name = "Strip Frequency Code", .value = v.strip_frequency_code});
    fields.push_back(
        {.name = "Variance Strike",
         .value = v.variance_strike ? std::to_string(*v.variance_strike) : std::string{}});
    fields.push_back(
        {.name = "Accumulation Amount",
         .value = v.accumulation_amount ? std::to_string(*v.accumulation_amount) : std::string{}});
    fields.push_back(
        {.name = "Knock Out Barrier",
         .value = v.knock_out_barrier ? std::to_string(*v.knock_out_barrier) : std::string{}});
    fields.push_back({.name = "Barrier Type", .value = v.barrier_type});
    fields.push_back({.name = "Lower Barrier",
                      .value = v.lower_barrier ? std::to_string(*v.lower_barrier) : std::string{}});
    fields.push_back({.name = "Upper Barrier",
                      .value = v.upper_barrier ? std::to_string(*v.upper_barrier) : std::string{}});
    fields.push_back({.name = "Basket Json", .value = v.basket_json});
    fields.push_back({.name = "Day Count Code", .value = v.day_count_code});
    fields.push_back({.name = "Payment Frequency Code", .value = v.payment_frequency_code});
    fields.push_back({.name = "Swaption Expiry Date", .value = v.swaption_expiry_date});
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
