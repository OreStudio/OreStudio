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
#include "ores.marketdata.core/presentation/fx_forward_publish_history_field_mapper.hpp"
#include "ores.diff/domain/field_value.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.marketdata.api/domain/fx_forward_publish.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <string>
#include <vector>

namespace ores::marketdata::presentation {

std::vector<ores::diff::domain::field_value>
render_fx_forward_publish_fields(const domain::fx_forward_publish& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Request ID", .value = boost::uuids::to_string(v.request_id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Pair Code", .value = v.pair_code});
    fields.push_back(
        {.name = "As Of", .value = ores::platform::time::datetime::to_iso8601_utc(v.as_of)});
    fields.push_back({.name = "Spot Date",
                      .value = ores::platform::time::datetime::to_iso8601_date(v.spot_date)});
    fields.push_back({.name = "Spot Rate", .value = std::to_string(v.spot_rate)});
    fields.push_back({.name = "Base Curve Publish ID",
                      .value = boost::uuids::to_string(v.base_curve_publish_id)});
    fields.push_back({.name = "Quote Curve Publish ID",
                      .value = boost::uuids::to_string(v.quote_curve_publish_id)});
    fields.push_back({.name = "Convention Version", .value = std::to_string(v.convention_version)});
    fields.push_back({.name = "Calendar Codes", .value = v.calendar_codes});
    fields.push_back({.name = "Method Code", .value = v.method_code});
    fields.push_back({.name = "Published By", .value = v.published_by});
    fields.push_back({.name = "Published At",
                      .value = v.published_at ?
                                   ores::platform::time::datetime::to_iso8601_utc(*v.published_at) :
                                   std::string{}});
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
