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
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

refdata::domain::zero_inflation_index_convention
conventions_mapper::map_zero_inflation_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero inflation index convention: " << text_of(v, "Id");

    refdata::domain::zero_inflation_index_convention r;
    r.id = text_of(v, "Id");
    r.region_name = text_of(v, "RegionName");
    r.region_code = text_of(v, "RegionCode");
    r.revised = flag_of(v, "Revised");
    r.frequency = frequency_of(v, "Frequency");
    r.availability_lag = text_of(v, "AvailabilityLag");
    r.currency = spelling_of(v, "Currency", "currency code");

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_zero_inflation_index(
    const refdata::domain::zero_inflation_index_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("RegionName", v.region_name);
    w.set_text("RegionCode", v.region_code);
    w.set_flag("Revised", v.revised);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    w.set_text("AvailabilityLag", v.availability_lag);
    put_enum(w, "Currency", v.currency, "currency code");
}

namespace conventions_family {

void pack_zero_inflation_index(const record_reader& root,
                               refdata::service::conventions_bundle& bundle) {
    pack_family(bundle,
                "zero_inflation_index",
                root.children("ZeroInflationIndex"),
                &conventions_mapper::map_zero_inflation_index);
}

void join_zero_inflation_index(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_zero_inflation_index(
        unpack_row<refdata::domain::zero_inflation_index_convention>(row),
        root.append("ZeroInflationIndex"));
}

}

}
