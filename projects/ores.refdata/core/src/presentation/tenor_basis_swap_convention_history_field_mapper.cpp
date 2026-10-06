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
#include "ores.refdata.core/presentation/tenor_basis_swap_convention_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/tenor_basis_swap_convention.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_tenor_basis_swap_convention_fields(const domain::tenor_basis_swap_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = v.id});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Pay Index", .value = v.pay_index.value_or(std::string{})});
    fields.push_back({.name = "Pay Frequency", .value = v.pay_frequency.value_or(std::string{})});
    fields.push_back({.name = "Receive Index", .value = v.receive_index.value_or(std::string{})});
    fields.push_back(
        {.name = "Receive Frequency", .value = v.receive_frequency.value_or(std::string{})});
    fields.push_back(
        {.name = "Spread On Rec",
         .value = v.spread_on_rec ? (*v.spread_on_rec ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Include Spread",
         .value = v.include_spread ? (*v.include_spread ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Sub Periods Coupon Type",
                      .value = v.sub_periods_coupon_type.value_or(std::string{})});
    fields.push_back(
        {.name = "Pay Is Averaged",
         .value = v.pay_is_averaged ? (*v.pay_is_averaged ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Rec Is Averaged",
         .value = v.rec_is_averaged ? (*v.rec_is_averaged ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Long Index", .value = v.long_index.value_or(std::string{})});
    fields.push_back({.name = "Long Pay Tenor", .value = v.long_pay_tenor.value_or(std::string{})});
    fields.push_back({.name = "Short Index", .value = v.short_index.value_or(std::string{})});
    fields.push_back(
        {.name = "Short Pay Tenor", .value = v.short_pay_tenor.value_or(std::string{})});
    fields.push_back(
        {.name = "Spread On Short",
         .value = v.spread_on_short ? (*v.spread_on_short ? "true" : "false") : std::string{}});
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
