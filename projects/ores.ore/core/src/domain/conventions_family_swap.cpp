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

refdata::domain::swap_convention conventions_mapper::map_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping swap convention: " << text_of(v, "Id");

    refdata::domain::swap_convention r;
    r.id = text_of(v, "Id");
    assign(r.fixed_calendar, opt_text(v, "FixedCalendar"));
    r.fixed_frequency = frequency_of(v, "FixedFrequency");
    assign(r.fixed_convention, opt_bdc(v, "FixedConvention"));
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    r.index = text_of(v, "Index");
    assign(r.float_frequency, opt_frequency(v, "FloatFrequency"));
    assign(r.sub_periods_coupon_type, opt_spelling(v, "SubPeriodsCouponType"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_swap(const refdata::domain::swap_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_text("Index", v.index);
    put_enum(w, "FloatFrequency", v.float_frequency, "frequency type");
    put_enum(w, "SubPeriodsCouponType", v.sub_periods_coupon_type, "sub-periods coupon type");
}

namespace conventions_family {

void pack_swap(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle, "swap", root.children("Swap"), &conventions_mapper::map_swap);
}

void join_swap(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_swap(unpack_row<refdata::domain::swap_convention>(row),
                                     root.append("Swap"));
}

}

}
