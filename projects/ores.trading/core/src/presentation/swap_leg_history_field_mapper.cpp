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
#include "ores.trading.core/presentation/swap_leg_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_swap_leg_fields(const domain::swap_leg& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Trade ID", .value = boost::uuids::to_string(v.identity.trade_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Trade Activity ID",
                      .value = boost::uuids::to_string(v.identity.trade_activity_id)});
    fields.push_back(
        {.name = "Payer", .value = v.payer ? (*v.payer ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Leg Type Code", .value = v.leg_type_code});
    fields.push_back({.name = "Day Count Fraction Code", .value = v.day_count_fraction_code});
    fields.push_back(
        {.name = "Business Day Convention Code", .value = v.business_day_convention_code});
    fields.push_back({.name = "Payment Frequency Code", .value = v.payment_frequency_code});
    fields.push_back({.name = "Floating Index Code", .value = v.floating_index_code});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Leg Group", .value = v.leg_group.value_or(std::string{})});
    fields.push_back({.name = "Fixing Days",
                      .value = v.fixing_days ? std::to_string(*v.fixing_days) : std::string{}});
    fields.push_back(
        {.name = "Is In Arrears",
         .value = v.is_in_arrears ? (*v.is_in_arrears ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Include Spread",
         .value = v.include_spread ? (*v.include_spread ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Is Averaged",
         .value = v.is_averaged ? (*v.is_averaged ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Has Sub Periods",
         .value = v.has_sub_periods ? (*v.has_sub_periods ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Notional Initial Exchange",
                      .value = v.notional_initial_exchange ?
                                   (*v.notional_initial_exchange ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Notional Final Exchange",
                      .value = v.notional_final_exchange ?
                                   (*v.notional_final_exchange ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Payment Lag", .value = v.payment_lag.value_or(std::string{})});
    fields.push_back(
        {.name = "Payment Calendar", .value = v.payment_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Commodity Price Type", .value = v.commodity_price_type.value_or(std::string{})});
    fields.push_back(
        {.name = "Future Month Offset",
         .value = v.future_month_offset ? std::to_string(*v.future_month_offset) : std::string{}});
    fields.push_back(
        {.name = "Daily Expiry Offset",
         .value = v.daily_expiry_offset ? std::to_string(*v.daily_expiry_offset) : std::string{}});
    fields.push_back(
        {.name = "Pricing Calendar", .value = v.pricing_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Equity Return Type", .value = v.equity_return_type.value_or(std::string{})});
    fields.push_back({.name = "Equity Name", .value = v.equity_name.value_or(std::string{})});
    fields.push_back({.name = "Equity Underlying Type",
                      .value = v.equity_underlying_type.value_or(std::string{})});
    fields.push_back({.name = "Equity Underlying Identifier Type",
                      .value = v.equity_underlying_identifier_type.value_or(std::string{})});
    fields.push_back({.name = "Equity Quantity",
                      .value = v.equity_quantity ? v.equity_quantity->to_string() : std::string{}});
    fields.push_back(
        {.name = "Equity Initial Price",
         .value = v.equity_initial_price ? v.equity_initial_price->to_string() : std::string{}});
    fields.push_back({.name = "Equity Initial Price Currency",
                      .value = v.equity_initial_price_currency.value_or(std::string{})});
    fields.push_back({.name = "Equity Dividend Factor",
                      .value = v.equity_dividend_factor ? v.equity_dividend_factor->to_string() :
                                                          std::string{}});
    fields.push_back({.name = "Equity Notional Reset",
                      .value = v.equity_notional_reset ?
                                   (*v.equity_notional_reset ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Equity Currency", .value = v.equity_currency.value_or(std::string{})});
    fields.push_back({.name = "FX Index", .value = v.fx_index.value_or(std::string{})});
    fields.push_back({.name = "Indexings From Asset Leg",
                      .value = v.indexings_from_asset_leg ?
                                   (*v.indexings_from_asset_leg ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Floating Index Code 2",
                      .value = v.floating_index_code_2.value_or(std::string{})});
    fields.push_back({.name = "Cms Duration",
                      .value = v.cms_duration ? std::to_string(*v.cms_duration) : std::string{}});
    fields.push_back(
        {.name = "Naked Option",
         .value = v.naked_option ? (*v.naked_option ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Lookback", .value = v.lookback.value_or(std::string{})});
    fields.push_back({.name = "Rate Cutoff",
                      .value = v.rate_cutoff ? std::to_string(*v.rate_cutoff) : std::string{}});
    fields.push_back(
        {.name = "Local Cap Floor",
         .value = v.local_cap_floor ? (*v.local_cap_floor ? "true" : "false") : std::string{}});
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
