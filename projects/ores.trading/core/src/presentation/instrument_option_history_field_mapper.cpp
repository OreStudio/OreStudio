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
#include "ores.trading.core/presentation/instrument_option_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_instrument_option_fields(const domain::instrument_option& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Option Type", .value = v.option_type.value_or(std::string{})});
    fields.push_back({.name = "Payoff Type", .value = v.payoff_type.value_or(std::string{})});
    fields.push_back({.name = "Payoff Type 2", .value = v.payoff_type_2.value_or(std::string{})});
    fields.push_back({.name = "Style", .value = v.style.value_or(std::string{})});
    fields.push_back({.name = "Notice Period", .value = v.notice_period.value_or(std::string{})});
    fields.push_back(
        {.name = "Notice Calendar", .value = v.notice_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Notice Convention", .value = v.notice_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Mid Coupon Exercise", .value = v.mid_coupon_exercise.value_or(std::string{})});
    fields.push_back({.name = "Settlement", .value = v.settlement.value_or(std::string{})});
    fields.push_back(
        {.name = "Settlement Method", .value = v.settlement_method.value_or(std::string{})});
    fields.push_back(
        {.name = "Pay Off At Expiry", .value = v.pay_off_at_expiry.value_or(std::string{})});
    fields.push_back({.name = "Premium Amount", .value = v.premium_amount.value_or(std::string{})});
    fields.push_back(
        {.name = "Premium Currency", .value = v.premium_currency.value_or(std::string{})});
    fields.push_back(
        {.name = "Premium Pay Date", .value = v.premium_pay_date.value_or(std::string{})});
    fields.push_back(
        {.name = "Exercise Prices", .value = v.exercise_prices.value_or(std::string{})});
    fields.push_back({.name = "Exercise Fee Settlement Period",
                      .value = v.exercise_fee_settlement_period.value_or(std::string{})});
    fields.push_back({.name = "Exercise Fee Settlement Calendar",
                      .value = v.exercise_fee_settlement_calendar.value_or(std::string{})});
    fields.push_back({.name = "Exercise Fee Settlement Convention",
                      .value = v.exercise_fee_settlement_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Automatic Exercise", .value = v.automatic_exercise.value_or(std::string{})});
    fields.push_back(
        {.name = "Has Exercise Data", .value = v.has_exercise_data ? "true" : "false"});
    fields.push_back({.name = "Exercise Date", .value = v.exercise_date.value_or(std::string{})});
    fields.push_back(
        {.name = "Exercise Price",
         .value = v.exercise_price ? std::to_string(*v.exercise_price) : std::string{}});
    fields.push_back({.name = "Has Payment Data", .value = v.has_payment_data ? "true" : "false"});
    fields.push_back(
        {.name = "Payment Calendar", .value = v.payment_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Payment Convention", .value = v.payment_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Payment Relative To", .value = v.payment_relative_to.value_or(std::string{})});
    fields.push_back(
        {.name = "Has Settlement Data", .value = v.has_settlement_data ? "true" : "false"});
    fields.push_back({.name = "Settlement Pay Currency",
                      .value = v.settlement_pay_currency.value_or(std::string{})});
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
