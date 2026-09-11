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
#include "ores.trading.core/presentation/bond_forward_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_bond_forward_fields(const domain::bond_forward& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back(
        {.name = "Long In Forward", .value = v.long_in_forward.value_or(std::string{})});
    fields.push_back({.name = "Forward Maturity Date",
                      .value = v.forward_maturity_date.value_or(std::string{})});
    fields.push_back({.name = "Forward Settlement Date",
                      .value = v.forward_settlement_date.value_or(std::string{})});
    fields.push_back({.name = "Settlement", .value = v.settlement.value_or(std::string{})});
    fields.push_back(
        {.name = "Amount", .value = v.amount ? std::to_string(*v.amount) : std::string{}});
    fields.push_back(
        {.name = "Lock Rate", .value = v.lock_rate ? std::to_string(*v.lock_rate) : std::string{}});
    fields.push_back({.name = "Dv01", .value = v.dv01 ? std::to_string(*v.dv01) : std::string{}});
    fields.push_back({.name = "Lock Rate Day Counter",
                      .value = v.lock_rate_day_counter.value_or(std::string{})});
    fields.push_back(
        {.name = "Settlement Dirty", .value = v.settlement_dirty.value_or(std::string{})});
    fields.push_back({.name = "Premium Amount", .value = v.premium_amount.value_or(std::string{})});
    fields.push_back({.name = "Premium Date", .value = v.premium_date.value_or(std::string{})});
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
