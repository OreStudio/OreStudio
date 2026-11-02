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
#include "ores.marketdata.core/presentation/market_calibration_value_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.marketdata.api/domain/market_calibration_value.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::presentation {

std::vector<ores::diff::domain::field_value>
render_market_calibration_value_fields(const domain::market_calibration_value& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back(
        {.name = "Report Instance ID", .value = boost::uuids::to_string(v.report_instance_id)});
    fields.push_back({.name = "Result ID", .value = boost::uuids::to_string(v.result_id)});
    fields.push_back({.name = "Source", .value = v.source});
    fields.push_back({.name = "Line", .value = std::to_string(v.line)});
    fields.push_back(
        {.name = "As Of", .value = ores::platform::time::datetime::to_iso8601_utc(v.as_of)});
    fields.push_back(
        {.name = "Report Definition ID", .value = boost::uuids::to_string(v.report_definition_id)});
    fields.push_back({.name = "Market Object Type", .value = v.market_object_type});
    fields.push_back({.name = "Market Object ID", .value = v.market_object_id});
    fields.push_back({.name = "Result Name", .value = v.result_name});
    fields.push_back({.name = "Result Key 1", .value = v.result_key_1});
    fields.push_back({.name = "Result Key 2", .value = v.result_key_2});
    fields.push_back({.name = "Result Key 3", .value = v.result_key_3});
    fields.push_back({.name = "Result Type", .value = v.result_type});
    fields.push_back({.name = "Value", .value = v.value});
    fields.push_back(
        {.name = "Figure", .value = v.figure ? std::to_string(*v.figure) : std::string{}});

    return fields;
}

}
