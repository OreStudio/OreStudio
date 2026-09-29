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
#include "ores.refdata.core/presentation/bma_basis_swap_convention_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_bma_basis_swap_convention_fields(const domain::bma_basis_swap_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = v.id});
    fields.push_back({.name = "Index", .value = v.index});
    fields.push_back({.name = "Bma Index", .value = v.bma_index});
    fields.push_back(
        {.name = "Bma Payment Calendar", .value = v.bma_payment_calendar.value_or(std::string{})});
    fields.push_back({.name = "Bma Payment Convention",
                      .value = v.bma_payment_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Bma Payment Lag",
         .value = v.bma_payment_lag ? std::to_string(*v.bma_payment_lag) : std::string{}});
    fields.push_back({.name = "Index Payment Calendar",
                      .value = v.index_payment_calendar.value_or(std::string{})});
    fields.push_back({.name = "Index Payment Convention",
                      .value = v.index_payment_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Index Payment Lag",
         .value = v.index_payment_lag ? std::to_string(*v.index_payment_lag) : std::string{}});
    fields.push_back({.name = "Index Settlement Days",
                      .value = v.index_settlement_days ? std::to_string(*v.index_settlement_days) :
                                                         std::string{}});
    fields.push_back(
        {.name = "Index Payment Period", .value = v.index_payment_period.value_or(std::string{})});
    fields.push_back({.name = "Overnight Lockout Days",
                      .value = v.overnight_lockout_days ?
                                   std::to_string(*v.overnight_lockout_days) :
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
