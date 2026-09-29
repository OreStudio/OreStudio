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
#include "ores.refdata.core/presentation/commodity_future_convention_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_commodity_future_convention_fields(const domain::commodity_future_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = v.id});
    fields.push_back({.name = "Contract Frequency", .value = v.contract_frequency});
    fields.push_back({.name = "Calendar", .value = v.calendar});
    fields.push_back(
        {.name = "Expiry Calendar", .value = v.expiry_calendar.value_or(std::string{})});
    fields.push_back(
        {.name = "Expiry Month Lag",
         .value = v.expiry_month_lag ? std::to_string(*v.expiry_month_lag) : std::string{}});
    fields.push_back(
        {.name = "One Contract Month", .value = v.one_contract_month.value_or(std::string{})});
    fields.push_back({.name = "Offset Days",
                      .value = v.offset_days ? std::to_string(*v.offset_days) : std::string{}});
    fields.push_back({.name = "Business Day Convention",
                      .value = v.business_day_convention.value_or(std::string{})});
    fields.push_back({.name = "Adjust Before Offset",
                      .value = v.adjust_before_offset ?
                                   (*v.adjust_before_offset ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Is Averaging",
         .value = v.is_averaging ? (*v.is_averaging ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Valid Contract Months",
                      .value = v.valid_contract_months.value_or(std::string{})});
    fields.push_back(
        {.name = "Anchor Day Of Month",
         .value = v.anchor_day_of_month ? std::to_string(*v.anchor_day_of_month) : std::string{}});
    fields.push_back({.name = "Anchor Calendar Days Before",
                      .value = v.anchor_calendar_days_before ?
                                   std::to_string(*v.anchor_calendar_days_before) :
                                   std::string{}});
    fields.push_back({.name = "Anchor Business Days After",
                      .value = v.anchor_business_days_after ?
                                   std::to_string(*v.anchor_business_days_after) :
                                   std::string{}});
    fields.push_back(
        {.name = "Anchor Nth Nth",
         .value = v.anchor_nth_nth ? std::to_string(*v.anchor_nth_nth) : std::string{}});
    fields.push_back(
        {.name = "Anchor Nth Weekday", .value = v.anchor_nth_weekday.value_or(std::string{})});
    fields.push_back(
        {.name = "Anchor Last Weekday", .value = v.anchor_last_weekday.value_or(std::string{})});
    fields.push_back({.name = "Anchor Weekly Day Of The Week",
                      .value = v.anchor_weekly_day_of_the_week.value_or(std::string{})});
    fields.push_back({.name = "Option Expiry Month Lag",
                      .value = v.option_expiry_month_lag ?
                                   std::to_string(*v.option_expiry_month_lag) :
                                   std::string{}});
    fields.push_back({.name = "Option Contract Frequency",
                      .value = v.option_contract_frequency.value_or(std::string{})});
    fields.push_back({.name = "Option Expiry Offset",
                      .value = v.option_expiry_offset ? std::to_string(*v.option_expiry_offset) :
                                                        std::string{}});
    fields.push_back({.name = "Option Calendar Days Before",
                      .value = v.option_calendar_days_before ?
                                   std::to_string(*v.option_calendar_days_before) :
                                   std::string{}});
    fields.push_back({.name = "Option Min Business Days Before",
                      .value = v.option_min_business_days_before ?
                                   std::to_string(*v.option_min_business_days_before) :
                                   std::string{}});
    fields.push_back(
        {.name = "Option Expiry Day",
         .value = v.option_expiry_day ? std::to_string(*v.option_expiry_day) : std::string{}});
    fields.push_back(
        {.name = "Option Nth Nth",
         .value = v.option_nth_nth ? std::to_string(*v.option_nth_nth) : std::string{}});
    fields.push_back(
        {.name = "Option Nth Weekday", .value = v.option_nth_weekday.value_or(std::string{})});
    fields.push_back({.name = "Option Expiry Last Weekday Of Month",
                      .value = v.option_expiry_last_weekday_of_month.value_or(std::string{})});
    fields.push_back({.name = "Option Expiry Weekly Day Of The Week",
                      .value = v.option_expiry_weekly_day_of_the_week.value_or(std::string{})});
    fields.push_back({.name = "Option Business Day Convention",
                      .value = v.option_business_day_convention.value_or(std::string{})});
    fields.push_back({.name = "Hours Per Day",
                      .value = v.hours_per_day ? std::to_string(*v.hours_per_day) : std::string{}});
    fields.push_back({.name = "Off Peak Index", .value = v.off_peak_index.value_or(std::string{})});
    fields.push_back({.name = "Peak Index", .value = v.peak_index.value_or(std::string{})});
    fields.push_back(
        {.name = "Off Peak Hours",
         .value = v.off_peak_hours ? std::to_string(*v.off_peak_hours) : std::string{}});
    fields.push_back({.name = "Peak Calendar", .value = v.peak_calendar.value_or(std::string{})});
    fields.push_back({.name = "Index Name", .value = v.index_name.value_or(std::string{})});
    fields.push_back({.name = "Savings Time", .value = v.savings_time.value_or(std::string{})});
    fields.push_back(
        {.name = "Delivery Location", .value = v.delivery_location.value_or(std::string{})});
    fields.push_back({.name = "Balance Of The Month",
                      .value = v.balance_of_the_month ?
                                   (*v.balance_of_the_month ? "true" : "false") :
                                   std::string{}});
    fields.push_back({.name = "Balance Of The Month Pricing Calendar",
                      .value = v.balance_of_the_month_pricing_calendar.value_or(std::string{})});
    fields.push_back({.name = "Option Underlying Future Convention",
                      .value = v.option_underlying_future_convention.value_or(std::string{})});
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
