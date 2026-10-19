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

refdata::domain::zero_convention conventions_mapper::map_zero(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping zero convention: " << text_of(v, "Id");

    refdata::domain::zero_convention r;
    r.id = text_of(v, "Id");
    r.tenor_based = flag_of(v, "TenorBased");
    r.day_count_fraction = day_counter_of(v, "DayCounter");
    assign(r.compounding, opt_compounding(v, "Compounding"));
    assign(r.compounding_frequency, opt_frequency(v, "CompoundingFrequency"));
    assign(r.tenor_calendar, opt_text(v, "TenorCalendar"));
    assign(r.spot_lag, opt_integer(v, "SpotLag"));
    assign(r.spot_calendar, opt_text(v, "SpotCalendar"));
    assign(r.roll_convention, opt_bdc(v, "RollConvention"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_zero(const refdata::domain::zero_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_flag("TenorBased", v.tenor_based);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    put_enum(w, "Compounding", v.compounding, "compounding");
    put_enum(w, "CompoundingFrequency", v.compounding_frequency, "frequency type");
    w.set_optional_text("TenorCalendar", v.tenor_calendar);
    w.set_optional_integer("SpotLag", as_integer(v.spot_lag));
    w.set_optional_text("SpotCalendar", v.spot_calendar);
    put_bdc(w, "RollConvention", v.roll_convention);
    w.set_optional_flag("EOM", v.end_of_month);
}

namespace conventions_family {

void pack_zero(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(bundle, "zero", root.children("Zero"), &conventions_mapper::map_zero);
}

void join_zero(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_zero(unpack_row<refdata::domain::zero_convention>(row),
                                     root.append("Zero"));
}

}

}
