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
#include "ores.refdata.core/presentation/calendar_date_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_calendar_date_fields(const repository::calendar_date_history_row& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    // The link's id is both keys, parent first, so a stream of many links can
    // tell one link's versions from another's.
    fields.push_back({.name = "ID",
                      .value = v.link.calendar_code + "/" +
                               ores::platform::time::datetime::to_iso8601_date(v.link.date)});
    fields.push_back({.name = "Calendar", .value = v.link.calendar_code});
    fields.push_back(
        {.name = "Date", .value = ores::platform::time::datetime::to_iso8601_date(v.link.date)});
    fields.push_back(
        {.name = "Is Business Day",
         .value = (v.link.is_business_day ? std::string("true") : std::string("false"))});
    fields.push_back({.name = "Source", .value = v.link.source});
    fields.push_back({.name = "Closed At",
                      .value = v.closed_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.closed_at) :
                                   std::string()});
    fields.push_back({.name = "Closed By", .value = v.closed_by});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.link.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.link.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.link.change_reason_code});
    fields.push_back(
        {.name = provenance_fields::change_commentary, .value = v.link.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}
