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

refdata::domain::ois_convention conventions_mapper::map_ois(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping OIS convention: " << text_of(v, "Id");

    refdata::domain::ois_convention r;
    r.id = text_of(v, "Id");
    r.spot_lag = integer_of(v, "SpotLag");
    r.index = text_of(v, "Index");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    assign(r.fixed_calendar, opt_text(v, "FixedCalendar"));
    assign(r.payment_lag, opt_integer(v, "PaymentLag"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    assign(r.fixed_frequency, opt_frequency(v, "FixedFrequency"));
    assign(r.fixed_convention, opt_bdc(v, "FixedConvention"));
    assign(r.fixed_payment_convention, opt_bdc(v, "FixedPaymentConvention"));
    assign(r.rule, opt_date_rule(v, "Rule"));
    assign(r.payment_calendar, opt_text(v, "PaymentCalendar"));
    assign(r.rate_cutoff, opt_integer(v, "RateCutoff"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_ois(const refdata::domain::ois_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SpotLag", v.spot_lag);
    w.set_text("Index", v.index);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_optional_text("FixedCalendar", v.fixed_calendar);
    w.set_optional_integer("PaymentLag", as_integer(v.payment_lag));
    w.set_optional_flag("EOM", v.end_of_month);
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_bdc(w, "FixedPaymentConvention", v.fixed_payment_convention);
    put_enum(w, "Rule", v.rule, "date rule");
    w.set_optional_text("PaymentCalendar", v.payment_calendar);
    w.set_optional_integer("RateCutoff", as_integer(v.rate_cutoff));
}

namespace conventions_family {

void pack_ois(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle, "ois", root.children("OIS"), &conventions_mapper::map_ois);
}

void join_ois(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_ois(unpack_row<refdata::domain::ois_convention>(row),
                                    root.append("OIS"));
}

}

}
