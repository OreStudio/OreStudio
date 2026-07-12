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
#include "ores.refdata.core/presentation/currency_pair_convention_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_currency_pair_convention_fields(const domain::currency_pair_convention& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Pair Code", .value = v.pair_code});
    fields.push_back({.name = "Pip Factor", .value = std::to_string(v.pip_factor)});
    fields.push_back({.name = "Tick Size", .value = std::to_string(v.tick_size)});
    fields.push_back({.name = "Decimal Places", .value = std::to_string(v.decimal_places)});
    fields.push_back(
        {.name = "Advance Calendar", .value = v.advance_calendar.value_or(std::string{})});
    fields.push_back({.name = "Business Day Convention",
                      .value = v.business_day_convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Spot Relative",
         .value = v.spot_relative ? (*v.spot_relative ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "End Of Month",
         .value = v.end_of_month ? (*v.end_of_month ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Modified By", .value = v.modified_by});
    fields.push_back({.name = "Performed By", .value = v.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.change_commentary});
    fields.push_back({.name = "Recorded At",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
