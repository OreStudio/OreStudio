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
#include "ores.refdata.core/presentation/cross_currency_fix_float_convention_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/cross_currency_fix_float_convention.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value> render_cross_currency_fix_float_convention_fields(
    const domain::cross_currency_fix_float_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = v.id});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Settlement Days", .value = std::to_string(v.settlement_days)});
    fields.push_back({.name = "Settlement Calendar", .value = v.settlement_calendar});
    fields.push_back({.name = "Settlement Convention", .value = v.settlement_convention});
    fields.push_back({.name = "Fixed Currency", .value = v.fixed_currency});
    fields.push_back({.name = "Fixed Frequency", .value = v.fixed_frequency});
    fields.push_back({.name = "Fixed Convention", .value = v.fixed_convention});
    fields.push_back({.name = "Fixed Day Count Fraction", .value = v.fixed_day_count_fraction});
    fields.push_back({.name = "Index", .value = v.index});
    fields.push_back({.name = "Eom", .value = v.eom ? (*v.eom ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Is Resettable",
         .value = v.is_resettable ? (*v.is_resettable ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Float Index Is Resettable",
                      .value = v.float_index_is_resettable ?
                                   (*v.float_index_is_resettable ? "true" : "false") :
                                   std::string{}});
    fields.push_back(
        {.name = "Include Spread",
         .value = v.include_spread ? (*v.include_spread ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Lookback", .value = v.lookback.value_or(std::string{})});
    fields.push_back({.name = "Fixing Days",
                      .value = v.fixing_days ? std::to_string(*v.fixing_days) : std::string{}});
    fields.push_back({.name = "Rate Cutoff",
                      .value = v.rate_cutoff ? std::to_string(*v.rate_cutoff) : std::string{}});
    fields.push_back(
        {.name = "Is Averaged",
         .value = v.is_averaged ? (*v.is_averaged ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Observation Shift",
         .value = v.observation_shift ? (*v.observation_shift ? "true" : "false") : std::string{}});
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
