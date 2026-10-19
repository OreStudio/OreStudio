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

refdata::domain::deposit_convention conventions_mapper::map_deposit(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping deposit convention: " << text_of(v, "Id");

    refdata::domain::deposit_convention r;
    r.id = text_of(v, "Id");
    r.index_based = flag_of(v, "IndexBased");
    assign(r.index, opt_text(v, "Index"));
    assign(r.calendar, opt_text(v, "Calendar"));
    assign(r.convention, opt_bdc(v, "Convention"));
    assign(r.end_of_month, opt_flag(v, "EOM"));
    assign(r.day_count_fraction, opt_day_counter(v, "DayCounter"));
    assign(r.settlement_days, opt_integer(v, "SettlementDays"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_deposit(const refdata::domain::deposit_convention& v,
                                         record_writer w) {
    w.set_text("Id", v.id);
    w.set_flag("IndexBased", v.index_based);
    w.set_optional_text("Index", v.index);
    w.set_optional_text("Calendar", v.calendar);
    put_bdc(w, "Convention", v.convention);
    w.set_optional_flag("EOM", v.end_of_month);
    put_day_counter(w, "DayCounter", v.day_count_fraction);
    w.set_optional_integer("SettlementDays", as_integer(v.settlement_days));
}

namespace conventions_family {

void pack_deposit(const record_reader& root, refdata::service::conventions_bundle& bundle) {
    pack_family(bundle, "deposit", root.children("Deposit"), &conventions_mapper::map_deposit);
}

void join_deposit(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_deposit(unpack_row<refdata::domain::deposit_convention>(row),
                                        root.append("Deposit"));
}

}

}
