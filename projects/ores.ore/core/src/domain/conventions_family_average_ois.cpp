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

refdata::domain::average_ois_convention
conventions_mapper::map_average_ois(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping averaging OIS convention: " << text_of(v, "Id");

    refdata::domain::average_ois_convention r;
    r.id = text_of(v, "Id");
    r.spot_lag = integer_of(v, "SpotLag");
    r.fixed_tenor = text_of(v, "FixedTenor");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    // ORE makes the first three of these required, so every document in the
    // corpus carries them. Only the frequency is optional, and the corpus never
    // sets it.
    r.fixed_calendar = text_of(v, "FixedCalendar");
    r.fixed_convention = bdc_of(v, "FixedConvention");
    r.fixed_payment_convention = bdc_of(v, "FixedPaymentConvention");
    assign(r.fixed_frequency, opt_frequency(v, "FixedFrequency"));
    r.index = text_of(v, "Index");
    r.on_tenor = text_of(v, "OnTenor");
    r.rate_cutoff = text_of(v, "RateCutoff");
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_average_ois(const refdata::domain::average_ois_convention& v,
                                             record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SpotLag", v.spot_lag);
    w.set_text("FixedTenor", v.fixed_tenor);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_bdc(w, "FixedPaymentConvention", v.fixed_payment_convention);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    w.set_text("Index", v.index);
    w.set_text("OnTenor", v.on_tenor);
    w.set_text("RateCutoff", v.rate_cutoff);
}

namespace conventions_family {

void pack_average_ois(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(
        bundle, "average_ois", root.children("AverageOIS"), &conventions_mapper::map_average_ois);
}

void join_average_ois(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_average_ois(
        unpack_row<refdata::domain::average_ois_convention>(row), root.append("AverageOIS"));
}

}

}
