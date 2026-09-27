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
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value> render_swap_leg_fields(const domain::swap_leg& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.identity.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back(
        {.name = "Instrument ID", .value = boost::uuids::to_string(v.identity.instrument_id)});
    fields.push_back({.name = "Leg Number", .value = std::to_string(v.identity.leg_number)});
    fields.push_back({.name = "Leg Type Code", .value = v.leg_type_code});
    fields.push_back({.name = "Day Count Fraction Code", .value = v.day_count_fraction_code});
    fields.push_back(
        {.name = "Business Day Convention Code", .value = v.business_day_convention_code});
    fields.push_back({.name = "Payment Frequency Code", .value = v.payment_frequency_code});
    fields.push_back({.name = "Floating Index Code", .value = v.floating_index_code});
    fields.push_back({.name = "Fixed Rate", .value = std::to_string(v.fixed_rate)});
    fields.push_back({.name = "Spread", .value = std::to_string(v.spread)});
    fields.push_back({.name = "Notional", .value = std::to_string(v.notional)});
    fields.push_back({.name = "Currency", .value = v.currency});
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
