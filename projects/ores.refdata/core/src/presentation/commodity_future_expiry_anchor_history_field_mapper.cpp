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
#include "ores.refdata.core/presentation/commodity_future_expiry_anchor_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/commodity_future_expiry_anchor.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value>
render_commodity_future_expiry_anchor_fields(const domain::commodity_future_expiry_anchor& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Convention ID", .value = boost::uuids::to_string(v.convention_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Anchor Day Of Month",
         .value = v.anchor_day_of_month ? std::to_string(*v.anchor_day_of_month) : std::string{}});
    fields.push_back({.name = "Anchor Calendar Days Before",
                      .value = v.anchor_calendar_days_before ?
                                   std::to_string(*v.anchor_calendar_days_before) :
                                   std::string{}});
    fields.push_back({.name = "Anchor Business Days After",
                      .value = v.anchor_business_days_after ?
                                   std::to_string(*v.anchor_business_days_after) :
                                   std::string{}});
    fields.push_back(
        {.name = "Anchor Nth Nth",
         .value = v.anchor_nth_nth ? std::to_string(*v.anchor_nth_nth) : std::string{}});
    fields.push_back(
        {.name = "Anchor Nth Weekday", .value = v.anchor_nth_weekday.value_or(std::string{})});
    fields.push_back(
        {.name = "Anchor Last Weekday", .value = v.anchor_last_weekday.value_or(std::string{})});
    fields.push_back({.name = "Anchor Weekly Day Of The Week",
                      .value = v.anchor_weekly_day_of_the_week.value_or(std::string{})});
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
