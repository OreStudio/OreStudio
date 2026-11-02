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
#include "ores.refdata.core/presentation/commodity_future_contract_terms_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/commodity_future_contract_terms.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_commodity_future_contract_terms_fields(const domain::commodity_future_contract_terms& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Convention ID", .value = boost::uuids::to_string(v.convention_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
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
    fields.push_back({.name = "Hours Per Day",
                      .value = v.hours_per_day ? std::to_string(*v.hours_per_day) : std::string{}});
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
    fields.push_back(
        {.name = "Prohibited Expiries", .value = v.prohibited_expiries.value_or(std::string{})});
    fields.push_back({.name = "Future Continuation Mappings",
                      .value = v.future_continuation_mappings.value_or(std::string{})});
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
