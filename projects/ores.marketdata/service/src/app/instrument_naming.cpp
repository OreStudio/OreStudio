/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#include "ores.marketdata.service/app/instrument_naming.hpp"

namespace ores::marketdata::service::app {

namespace dt = ores::marketdata::datum;

std::string field_text(const dt::market_datum& d, dt::field f) {
    if (!d.holds(f))
        return {};
    return dt::text_of(d.at(f));
}

std::string kind_of(const dt::schema_row& row, std::size_t axis_count) {
    using dt::instrument_type;
    if (row.type == instrument_type::fx_spot)
        return "pair";
    if (row.type == instrument_type::discount || row.type == instrument_type::zero)
        return "curve";
    if (axis_count >= 2)
        return "surface";
    if (axis_count == 1)
        return "ladder";
    return {};
}

std::string name_of(const dt::market_datum& s,
                    const dt::schema_row& row,
                    const std::string& kind,
                    const std::string& type_spelling) {
    const auto subject = field_text(s, row.subject);
    const auto curve_id = field_text(s, dt::field::curve_id);
    const auto surface_id = field_text(s, dt::field::surface_id);

    if (kind == "pair")
        return subject + "/" + field_text(s, dt::field::ccy);
    if (kind == "curve" && !curve_id.empty())
        return curve_id;
    if (kind == "surface" && !surface_id.empty())
        return surface_id;

    std::string out = subject.empty() ? type_spelling : subject + " " + type_spelling;
    for (const auto& f : row.fields) {
        if (f.role != dt::field_role::identity || f.name == row.subject)
            continue;
        if (f.name == dt::field::curve_id || f.name == dt::field::surface_id)
            continue;
        const auto v = field_text(s, f.name);
        if (!v.empty())
            out += " " + v;
    }
    return out;
}

}
