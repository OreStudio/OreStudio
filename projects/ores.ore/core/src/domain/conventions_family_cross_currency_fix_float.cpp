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

refdata::domain::cross_currency_fix_float_convention
conventions_mapper::map_cross_currency_fix_float(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency fix-float convention: "
                               << text_of(v, "Id");

    refdata::domain::cross_currency_fix_float_convention r;
    r.id = text_of(v, "Id");
    r.settlement_days = integer_of(v, "SettlementDays");
    r.settlement_calendar = text_of(v, "SettlementCalendar");
    r.settlement_convention = bdc_of(v, "SettlementConvention");
    r.fixed_currency = spelling_of(v, "FixedCurrency", "currency code");
    r.fixed_frequency = frequency_of(v, "FixedFrequency");
    r.fixed_convention = bdc_of(v, "FixedConvention");
    r.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    r.index = text_of(v, "Index");
    assign(r.eom, opt_flag(v, "EOM"));
    assign(r.is_resettable, opt_flag(v, "IsResettable"));
    assign(r.float_index_is_resettable, opt_flag(v, "FloatIndexIsResettable"));
    assign(r.include_spread, opt_flag(v, "IncludeSpread"));
    assign(r.lookback, opt_text(v, "Lookback"));
    assign(r.fixing_days, opt_integer(v, "FixingDays"));
    assign(r.rate_cutoff, opt_integer(v, "RateCutoff"));
    assign(r.is_averaged, opt_flag(v, "IsAveraged"));
    assign(r.observation_shift, opt_flag(v, "ObservationShift"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_cross_currency_fix_float(
    const refdata::domain::cross_currency_fix_float_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_integer("SettlementDays", v.settlement_days);
    w.set_text("SettlementCalendar", v.settlement_calendar);
    put_bdc(w, "SettlementConvention", v.settlement_convention);
    put_enum(w, "FixedCurrency", v.fixed_currency, "currency code");
    put_enum(w, "FixedFrequency", v.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", v.fixed_convention);
    put_day_counter(w, "FixedDayCounter", v.fixed_day_count_fraction);
    w.set_text("Index", v.index);
    w.set_optional_flag("EOM", v.eom);
    w.set_optional_flag("IsResettable", v.is_resettable);
    w.set_optional_flag("FloatIndexIsResettable", v.float_index_is_resettable);
    w.set_optional_flag("IncludeSpread", v.include_spread);
    w.set_optional_text("Lookback", v.lookback);
    w.set_optional_integer("FixingDays", as_integer(v.fixing_days));
    w.set_optional_integer("RateCutoff", as_integer(v.rate_cutoff));
    w.set_optional_flag("IsAveraged", v.is_averaged);
    w.set_optional_flag("ObservationShift", v.observation_shift);
}

namespace conventions_family {

void pack_cross_currency_fix_float(const record_reader& root,
                                   refdata::service::conventions_bundle& bundle) {
    pack_family(bundle,
                "cross_currency_fix_float",
                root.children("CrossCurrencyFixFloat"),
                &conventions_mapper::map_cross_currency_fix_float);
}

void join_cross_currency_fix_float(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_cross_currency_fix_float(
        unpack_row<refdata::domain::cross_currency_fix_float_convention>(row),
        root.append("CrossCurrencyFixFloat"));
}

}

}
