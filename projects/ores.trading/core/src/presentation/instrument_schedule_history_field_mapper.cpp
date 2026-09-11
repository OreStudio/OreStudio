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
#include "ores.trading.core/presentation/instrument_schedule_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_instrument_schedule_fields(const domain::instrument_schedule& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back({.name = "Schedule Kind", .value = v.schedule_kind});
    fields.push_back({.name = "Start Date", .value = v.start_date.value_or(std::string{})});
    fields.push_back({.name = "End Date", .value = v.end_date.value_or(std::string{})});
    fields.push_back({.name = "Adjust End Date To Previous Month End",
                      .value = v.adjust_end_date_to_previous_month_end.value_or(std::string{})});
    fields.push_back({.name = "Tenor", .value = v.tenor.value_or(std::string{})});
    fields.push_back({.name = "Calendar", .value = v.calendar.value_or(std::string{})});
    fields.push_back({.name = "Convention", .value = v.convention.value_or(std::string{})});
    fields.push_back(
        {.name = "Term Convention", .value = v.term_convention.value_or(std::string{})});
    fields.push_back({.name = "Rule", .value = v.rule.value_or(std::string{})});
    fields.push_back({.name = "End Of Month", .value = v.end_of_month.value_or(std::string{})});
    fields.push_back({.name = "End Of Month Convention",
                      .value = v.end_of_month_convention.value_or(std::string{})});
    fields.push_back({.name = "First Date", .value = v.first_date.value_or(std::string{})});
    fields.push_back({.name = "Last Date", .value = v.last_date.value_or(std::string{})});
    fields.push_back(
        {.name = "Remove First Date",
         .value = v.remove_first_date ? (*v.remove_first_date ? "true" : "false") : std::string{}});
    fields.push_back(
        {.name = "Remove Last Date",
         .value = v.remove_last_date ? (*v.remove_last_date ? "true" : "false") : std::string{}});
    fields.push_back({.name = "Include Duplicate Dates",
                      .value = v.include_duplicate_dates.value_or(std::string{})});
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
