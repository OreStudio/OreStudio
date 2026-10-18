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
#include "ores.trading.core/presentation/equity_cliquet_option_instrument_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/equity_cliquet_option_instrument.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_equity_cliquet_option_instrument_fields(const domain::equity_cliquet_option_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.trade_id)});
    fields.push_back(
        {.name = "Trade Activity ID", .value = boost::uuids::to_string(v.trade_activity_id)});
    fields.push_back({.name = "Underlying Name", .value = v.underlying_name});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Notional", .value = v.notional.to_string()});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Option Type", .value = v.option_type});
    fields.push_back(
        {.name = "Moneyness", .value = v.moneyness ? std::to_string(*v.moneyness) : std::string{}});
    fields.push_back(
        {.name = "Local Cap", .value = v.local_cap ? v.local_cap->to_string() : std::string{}});
    fields.push_back({.name = "Local Floor",
                      .value = v.local_floor ? v.local_floor->to_string() : std::string{}});
    fields.push_back(
        {.name = "Global Cap", .value = v.global_cap ? v.global_cap->to_string() : std::string{}});
    fields.push_back({.name = "Global Floor",
                      .value = v.global_floor ? v.global_floor->to_string() : std::string{}});
    fields.push_back(
        {.name = "Settlement Days",
         .value = v.settlement_days ? std::to_string(*v.settlement_days) : std::string{}});
    fields.push_back(
        {.name = "Premium", .value = v.premium ? v.premium->to_string() : std::string{}});
    fields.push_back(
        {.name = "Premium Currency", .value = v.premium_currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Premium Payment Date",
         .value = v.premium_payment_date ?
                      ores::platform::time::datetime::to_iso8601_date(*v.premium_payment_date) :
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
