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

refdata::domain::swap_index_convention conventions_mapper::map_swap_index(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap index convention: " << text_of(v, "Id");

    refdata::domain::swap_index_convention r;
    r.id = text_of(v, "Id");
    r.conventions = text_of(v, "Conventions");
    assign(r.fixing_calendar, opt_text(v, "FixingCalendar"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_swap_index(const refdata::domain::swap_index_convention& v,
                                            record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Conventions", v.conventions);
    w.set_optional_text("FixingCalendar", v.fixing_calendar);
}

namespace conventions_family {

void pack_swap_index(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(
        bundle, "swap_index", root.children("SwapIndex"), &conventions_mapper::map_swap_index);
}

void join_swap_index(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_swap_index(unpack_row<refdata::domain::swap_index_convention>(row),
                                           root.append("SwapIndex"));
}

}

}
