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
#include "ores.refdata.core/presentation/currency_field_mapper.hpp"
#include "ores.diff/engine/compare.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::refdata::presentation {

namespace {

std::string image_id_text(const std::optional<boost::uuids::uuid>& id) {
    if (!id.has_value())
        return "(none)";
    return boost::uuids::to_string(*id);
}

}

std::vector<ores::diff::domain::field_value> currency_field_mapper::map(const domain::currency& c) {
    return {{.name = "ISO Code", .value = c.iso_code},
            {.name = "Name", .value = c.name},
            {.name = "Numeric Code", .value = c.numeric_code},
            {.name = "Symbol", .value = c.symbol},
            {.name = "Fraction Symbol", .value = c.fraction_symbol},
            {.name = "Rounding Type", .value = c.rounding_type},
            {.name = "Format", .value = c.format},
            {.name = "Monetary Nature", .value = c.monetary_nature},
            {.name = "Market Tier", .value = c.market_tier},
            {.name = "Fractions Per Unit", .value = std::to_string(c.fractions_per_unit)},
            {.name = "Rounding Precision", .value = std::to_string(c.rounding_precision)},
            {.name = "Change Reason", .value = c.change_reason_code},
            {.name = "Commentary", .value = c.change_commentary},
            {.name = "Flag", .value = image_id_text(c.image_id)}};
}

std::vector<domain::currency_version>
currency_field_mapper::build_versions(const std::vector<domain::currency>& currencies) {
    std::vector<domain::currency_version> r;
    r.reserve(currencies.size());

    for (const auto& c : currencies) {
        domain::currency_version cv;
        cv.data = c;
        cv.version_number = c.version;
        cv.modified_by = c.modified_by;
        cv.recorded_at = c.recorded_at;
        cv.fields = map(c);
        r.push_back(std::move(cv));
    }

    // Versions arrive newest first; each diffs against the next older
    // one. The oldest keeps empty changes.
    for (std::size_t i = 0; i + 1 < r.size(); ++i)
        r[i].changes = ores::diff::engine::compute(r[i + 1].fields, r[i].fields);

    return r;
}

}
