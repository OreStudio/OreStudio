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

refdata::domain::cross_currency_basis_convention
conventions_mapper::map_cross_currency_basis(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency basis convention: " << text_of(v, "Id");

    refdata::domain::cross_currency_basis_convention r;
    r.id = text_of(v, "Id");
    r.settlement_days = integer_of(v, "SettlementDays");
    assign(r.settlement_calendar, opt_text(v, "SettlementCalendar"));
    r.roll_convention = bdc_of(v, "RollConvention");
    r.flat_index = text_of(v, "FlatIndex");
    r.spread_index = text_of(v, "SpreadIndex");
    assign(r.eom, opt_flag(v, "EOM"));
    assign(r.is_resettable, opt_flag(v, "IsResettable"));
    assign(r.flat_index_is_resettable, opt_flag(v, "FlatIndexIsResettable"));
    assign(r.flat_tenor, opt_text(v, "FlatTenor"));
    assign(r.spread_tenor, opt_text(v, "SpreadTenor"));
    assign(r.spread_payment_lag, opt_integer(v, "SpreadPaymentLag"));
    assign(r.flat_payment_lag, opt_integer(v, "FlatPaymentLag"));
    assign(r.spread_include_spread, opt_flag(v, "SpreadIncludeSpread"));
    assign(r.spread_lookback, opt_text(v, "SpreadLookback"));
    assign(r.spread_fixing_days, opt_integer(v, "SpreadFixingDays"));
    assign(r.spread_rate_cutoff, opt_integer(v, "SpreadRateCutoff"));
    assign(r.spread_is_averaged, opt_flag(v, "SpreadIsAveraged"));
    assign(r.spread_observation_shift, opt_flag(v, "SpreadObservationShift"));
    assign(r.flat_include_spread, opt_flag(v, "FlatIncludeSpread"));
    assign(r.flat_lookback, opt_text(v, "FlatLookback"));
    assign(r.flat_fixing_days, opt_integer(v, "FlatFixingDays"));
    assign(r.flat_rate_cutoff, opt_integer(v, "FlatRateCutoff"));
    assign(r.flat_is_averaged, opt_flag(v, "FlatIsAveraged"));
    assign(r.flat_observation_shift, opt_flag(v, "FlatObservationShift"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_cross_currency_basis(
    const refdata::domain::cross_currency_basis_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_optional_text("SettlementCalendar", v.settlement_calendar);
    put_bdc(w, "RollConvention", v.roll_convention);
    w.set_text("FlatIndex", v.flat_index);
    w.set_text("SpreadIndex", v.spread_index);
    w.set_optional_flag("EOM", v.eom);
    w.set_optional_flag("IsResettable", v.is_resettable);
    w.set_optional_flag("FlatIndexIsResettable", v.flat_index_is_resettable);
    w.set_optional_text("FlatTenor", v.flat_tenor);
    w.set_optional_text("SpreadTenor", v.spread_tenor);
    w.set_optional_integer("SpreadPaymentLag", as_integer(v.spread_payment_lag));
    w.set_optional_integer("FlatPaymentLag", as_integer(v.flat_payment_lag));
    w.set_optional_flag("SpreadIncludeSpread", v.spread_include_spread);
    w.set_optional_text("SpreadLookback", v.spread_lookback);
    w.set_optional_integer("SpreadFixingDays", as_integer(v.spread_fixing_days));
    w.set_optional_integer("SpreadRateCutoff", as_integer(v.spread_rate_cutoff));
    w.set_optional_flag("SpreadIsAveraged", v.spread_is_averaged);
    w.set_optional_flag("SpreadObservationShift", v.spread_observation_shift);
    w.set_optional_flag("FlatIncludeSpread", v.flat_include_spread);
    w.set_optional_text("FlatLookback", v.flat_lookback);
    w.set_optional_integer("FlatFixingDays", as_integer(v.flat_fixing_days));
    w.set_optional_integer("FlatRateCutoff", as_integer(v.flat_rate_cutoff));
    w.set_optional_flag("FlatIsAveraged", v.flat_is_averaged);
    w.set_optional_flag("FlatObservationShift", v.flat_observation_shift);
}

namespace conventions_family {

void pack_cross_currency_basis(const record_reader& root,
                               refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "cross_currency_basis",
                root.children("CrossCurrencyBasis"),
                &conventions_mapper::map_cross_currency_basis);
}

void join_cross_currency_basis(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_cross_currency_basis(
        unpack_row<refdata::domain::cross_currency_basis_convention>(row),
        root.append("CrossCurrencyBasis"));
}

}

}
