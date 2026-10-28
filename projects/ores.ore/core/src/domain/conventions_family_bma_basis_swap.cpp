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

refdata::domain::bma_basis_swap_convention
conventions_mapper::map_bma_basis_swap(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping BMA basis swap convention: " << text_of(v, "Id");

    refdata::domain::bma_basis_swap_convention r;
    r.id = text_of(v, "Id");
    r.index = text_of(v, "Index");
    r.bma_index = text_of(v, "BMAIndex");
    assign(r.bma_payment_calendar, opt_text(v, "BMAPaymentCalendar"));
    assign(r.bma_payment_convention, opt_bdc(v, "BMAPaymentConvention"));
    assign(r.bma_payment_lag, opt_integer(v, "BMAPaymentLag"));
    assign(r.index_payment_calendar, opt_text(v, "IndexPaymentCalendar"));
    assign(r.index_payment_convention, opt_bdc(v, "IndexPaymentConvention"));
    assign(r.index_payment_lag, opt_integer(v, "IndexPaymentLag"));
    assign(r.index_settlement_days, opt_integer(v, "IndexSettlementDays"));
    assign(r.index_payment_period, opt_text(v, "IndexPaymentPeriod"));
    assign(r.overnight_lockout_days, opt_integer(v, "OvernightLockoutDays"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_bma_basis_swap(const refdata::domain::bma_basis_swap_convention& v,
                                                record_writer w) {
    w.set_text("Id", v.id);
    w.set_text("Index", v.index);
    w.set_text("BMAIndex", v.bma_index);
    w.set_optional_text("BMAPaymentCalendar", v.bma_payment_calendar);
    put_bdc(w, "BMAPaymentConvention", v.bma_payment_convention);
    w.set_optional_integer("BMAPaymentLag", as_integer(v.bma_payment_lag));
    w.set_optional_text("IndexPaymentCalendar", v.index_payment_calendar);
    put_bdc(w, "IndexPaymentConvention", v.index_payment_convention);
    w.set_optional_integer("IndexPaymentLag", as_integer(v.index_payment_lag));
    w.set_optional_integer("IndexSettlementDays", as_integer(v.index_settlement_days));
    w.set_optional_text("IndexPaymentPeriod", v.index_payment_period);
    w.set_optional_integer("OvernightLockoutDays", as_integer(v.overnight_lockout_days));
}

namespace conventions_family {

void pack_bma_basis_swap(const record_reader& root, refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "bma_basis_swap",
                root.children("BMABasisSwap"),
                &conventions_mapper::map_bma_basis_swap);
}

void join_bma_basis_swap(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_bma_basis_swap(
        unpack_row<refdata::domain::bma_basis_swap_convention>(row), root.append("BMABasisSwap"));
}

}

}
