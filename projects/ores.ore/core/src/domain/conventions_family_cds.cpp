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

refdata::domain::cds_convention conventions_mapper::map_cds(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping CDS convention: " << text_of(v, "Id");

    refdata::domain::cds_convention r;
    r.id = text_of(v, "Id");
    assign(r.settlement_days, opt_integer(v, "SettlementDays"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.frequency, opt_frequency(v, "Frequency"));
    assign(r.payment_convention, opt_bdc(v, "PaymentConvention"));
    assign(r.rule, opt_date_rule(v, "Rule"));
    assign(r.day_count_fraction, opt_day_counter(v, "DayCounter"));
    assign(r.upfront_settlement_days, opt_integer(v, "UpfrontSettlementDays"));
    r.settles_accrual = flag_of(v, "SettlesAccrual");
    r.pays_at_default_time = flag_of(v, "PaysAtDefaultTime");
    assign(r.last_period_day_count_fraction, opt_day_counter(v, "LastPeriodDayCounter"));

    set_audit(r);
    return r;
}

void conventions_mapper::reverse_cds(const refdata::domain::cds_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_text("Calendar", v.calendar);
    put_enum(w, "Frequency", v.frequency, "frequency type");
    put_bdc(w, "PaymentConvention", v.payment_convention);
    put_enum(w, "Rule", v.rule, "date rule");
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_optional_integer("UpfrontSettlementDays", as_integer(v.upfront_settlement_days));
    w.set_flag("SettlesAccrual", v.settles_accrual);
    w.set_flag("PaysAtDefaultTime", v.pays_at_default_time);
    put_day_counter(w, "LastPeriodDayCounter", v.last_period_day_count_fraction);
}

namespace conventions_family {

void pack_cds(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle, "cds", root.children("CDS"), &conventions_mapper::map_cds);
}

void join_cds(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_cds(unpack_row<refdata::domain::cds_convention>(row),
                                    root.append("CDS"));
}

}

}
