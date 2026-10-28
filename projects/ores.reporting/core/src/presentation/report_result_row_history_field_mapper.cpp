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
#include "ores.reporting.core/presentation/report_result_row_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/domain/report_result_row.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::reporting::presentation {

std::vector<ores::diff::domain::field_value>
render_report_result_row_fields(const domain::report_result_row& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Report Instance ID", .value = boost::uuids::to_string(v.report_instance_id)});
    fields.push_back(
        {.name = "As Of", .value = ores::platform::time::datetime::to_iso8601_utc(v.as_of)});
    fields.push_back({.name = "Subject Type", .value = v.subject_type});
    fields.push_back({.name = "Subject ID", .value = boost::uuids::to_string(v.subject_id)});
    fields.push_back({.name = "Measure", .value = v.measure});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back(
        {.name = "Report Definition ID", .value = boost::uuids::to_string(v.report_definition_id)});
    fields.push_back(
        {.name = "Counterparty ID", .value = boost::uuids::to_string(v.counterparty_id)});
    fields.push_back({.name = "Figure", .value = std::to_string(v.figure)});
    fields.push_back({.name = "Source", .value = v.source});

    return fields;
}

}
