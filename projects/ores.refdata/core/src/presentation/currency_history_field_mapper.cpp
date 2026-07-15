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
#include "ores.refdata.core/presentation/currency_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

std::vector<ores::diff::domain::field_value> render_currency_fields(const domain::currency& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ISO Code", .value = v.iso_code});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "Numeric Code", .value = v.numeric_code});
    fields.push_back({.name = "Symbol", .value = v.symbol});
    fields.push_back({.name = "Fraction Symbol", .value = v.fraction_symbol});
    fields.push_back({.name = "Fractions Per Unit", .value = std::to_string(v.fractions_per_unit)});
    fields.push_back({.name = "Rounding Type", .value = v.rounding_type});
    fields.push_back({.name = "Rounding Precision", .value = std::to_string(v.rounding_precision)});
    fields.push_back({.name = "Format", .value = v.format});
    fields.push_back({.name = "Monetary Nature", .value = v.monetary_nature});
    fields.push_back({.name = "Market Tier", .value = v.market_tier});
    fields.push_back({.name = "Image ID",
                      .value = v.image_id ? boost::uuids::to_string(*v.image_id) : std::string{}});
    fields.push_back({.name = "Spot Days", .value = std::to_string(v.spot_days)});
    fields.push_back({.name = "Day Basis", .value = v.day_basis});
    fields.push_back({.name = "Base Precedence", .value = std::to_string(v.base_precedence)});
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
