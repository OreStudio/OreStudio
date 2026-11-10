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
#include "ores.marketdata.core/presentation/fx_forward_publish_point_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish_point.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::presentation {

std::vector<ores::diff::domain::field_value>
render_fx_forward_publish_point_fields(const domain::fx_forward_publish_point& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Publish ID", .value = boost::uuids::to_string(v.publish_id)});
    fields.push_back({.name = "Sequence Index", .value = std::to_string(v.sequence_index)});
    fields.push_back({.name = "Tenor Code", .value = v.tenor_code});
    fields.push_back({.name = "Near Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.near_date)});
    fields.push_back({.name = "Value Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.value_date)});
    fields.push_back({.name = "Base Discount", .value = std::to_string(v.base_discount)});
    fields.push_back({.name = "Quote Discount", .value = std::to_string(v.quote_discount)});
    fields.push_back({.name = "Forward Rate", .value = std::to_string(v.forward_rate)});
    fields.push_back({.name = "Forward Points", .value = std::to_string(v.forward_points)});
    fields.push_back({.name = "Swap Points", .value = std::to_string(v.swap_points)});
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
