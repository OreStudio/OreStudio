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

refdata::domain::cms_spread_option_convention
conventions_mapper::map_cms_spread_option(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CMS spread option convention: " << text_of(v, "Id");

    refdata::domain::cms_spread_option_convention r;
    r.id = text_of(v, "Id");
    r.forward_start = text_of(v, "ForwardStart");
    r.spot_days = text_of(v, "SpotDays");
    r.swap_tenor = text_of(v, "SwapTenor");
    r.fixing_days = integer_of(v, "FixingDays");
    r.calendar = text_of(v, "Calendar");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    r.roll_convention = bdc_of(v, "RollConvention");

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_cms_spread_option(
    const refdata::domain::cms_spread_option_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("ForwardStart", v.forward_start);
    w.set_text("SpotDays", v.spot_days);
    w.set_text("SwapTenor", v.swap_tenor);
    w.set_integer("FixingDays", v.fixing_days);
    w.set_text("Calendar", v.calendar);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    put_bdc(w, "RollConvention", v.roll_convention);
}

namespace conventions_family {

void pack_cms_spread_option(const record_reader& root,
                            refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "cms_spread_option",
                root.children("CmsSpreadOption"),
                &conventions_mapper::map_cms_spread_option);
}

void join_cms_spread_option(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_cms_spread_option(
        unpack_row<refdata::domain::cms_spread_option_convention>(row),
        root.append("CmsSpreadOption"));
}

}

}
