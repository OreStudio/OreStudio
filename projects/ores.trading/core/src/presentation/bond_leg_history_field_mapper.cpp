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
#include "ores.trading.core/presentation/bond_leg_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_bond_leg_fields(const domain::bond_leg& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back(
        {.name = "Payer", .value = v.payer ? (*v.payer ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Leg Type", .value = v.leg_type.value_or(std::string{})});
    fields.push_back({.name = "Currency", .value = v.currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Payment Convention", .value = v.payment_convention.value_or(std::string{})});
    fields.push_back({.name = "Payment Lag", .value = v.payment_lag.value_or(std::string{})});
    fields.push_back(
        {.name = "Payment Calendar", .value = v.payment_calendar.value_or(std::string{})});
    fields.push_back({.name = "Day Counter", .value = v.day_counter.value_or(std::string{})});
    fields.push_back({.name = "Last Period Day Counter",
                      .value = v.last_period_day_counter.value_or(std::string{})});
    fields.push_back({.name = "Strict Notional Dates",
                      .value = v.strict_notional_dates ?
                                   (*v.strict_notional_dates ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Indexings From Asset Leg",
                      .value = v.indexings_from_asset_leg ?
                                   (*v.indexings_from_asset_leg ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Settlement FX Index", .value = v.settlement_fx_index.value_or(std::string{})});
    fields.push_back({.name = "Settlement Fixing Date",
                      .value = v.settlement_fixing_date.value_or(std::string{})});
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
