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
#include "ores.refdata.core/presentation/calendar_rule_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_calendar_rule_fields(const domain::calendar_rule& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Calendar Code", .value = v.calendar_code});
    fields.push_back({.name = "Kind", .value = v.kind});
    fields.push_back(
        {.name = "Month", .value = v.month ? std::to_string(*v.month) : std::string{}});
    fields.push_back({.name = "Day", .value = v.day ? std::to_string(*v.day) : std::string{}});
    fields.push_back(
        {.name = "Weekday", .value = v.weekday ? std::to_string(*v.weekday) : std::string{}});
    fields.push_back({.name = "Occurrence",
                      .value = v.occurrence ? std::to_string(*v.occurrence) : std::string{}});
    fields.push_back({.name = "Day Offset",
                      .value = v.day_offset ? std::to_string(*v.day_offset) : std::string{}});
    fields.push_back({.name = "Shift", .value = v.shift});
    fields.push_back(
        {.name = "Effective From",
         .value = v.effective_from ? std::to_string(*v.effective_from) : std::string{}});
    fields.push_back({.name = "Effective To",
                      .value = v.effective_to ? std::to_string(*v.effective_to) : std::string{}});
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
