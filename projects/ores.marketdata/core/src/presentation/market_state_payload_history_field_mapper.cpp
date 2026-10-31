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
#include "ores.marketdata.core/presentation/market_state_payload_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.marketdata.api/domain/market_state_payload.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::presentation {

std::vector<ores::diff::domain::field_value>
render_market_state_payload_fields(const domain::market_state_payload& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Report Instance ID", .value = boost::uuids::to_string(v.report_instance_id)});
    fields.push_back({.name = "Family", .value = v.family});
    fields.push_back({.name = "File Name", .value = v.file_name});
    fields.push_back(
        {.name = "As Of", .value = ores::platform::time::datetime::to_iso8601_utc(v.as_of)});
    fields.push_back(
        {.name = "Report Definition ID", .value = boost::uuids::to_string(v.report_definition_id)});
    fields.push_back({.name = "Row Count", .value = std::to_string(v.row_count)});
    fields.push_back({.name = "Column Count", .value = std::to_string(v.column_count)});
    fields.push_back({.name = "Column Names", .value = v.column_names});
    fields.push_back({.name = "Key Values", .value = v.key_values});
    fields.push_back({.name = "Keys Truncated", .value = v.keys_truncated ? "true" : "false"});
    fields.push_back({.name = "Payload", .value = v.payload});
    fields.push_back({.name = "Payload Bytes", .value = std::to_string(v.payload_bytes)});
    fields.push_back({.name = "Content Sha256", .value = v.content_sha256});
    fields.push_back({.name = "Source", .value = v.source});

    return fields;
}

}
