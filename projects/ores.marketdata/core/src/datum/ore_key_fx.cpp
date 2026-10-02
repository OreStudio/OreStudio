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
#include "ore_key_reading.hpp"
#include <stdexcept>

/**
 * @file ore_key_fx.cpp
 * @brief The FX instruments: spot, forward and option. ORE's parseMarketDatum,
 * lines 606 to 628.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

}

market_datum read_fx(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
    case it::fx_spot:
        // FX/RATE/unitCcy/ccy
        require_size(rest, {2});
        return datum_builder(t, q)
            .set(f::unit_ccy, text(rest[0]))
            .set(f::ccy, text(rest[1]))
            .build();
    case it::fx_fwd:
        // FXFWD/RATE/unitCcy/ccy/term, the term a period, a date, ON, TN or SN.
        require_size(rest, {3});
        return datum_builder(t, q)
            .set(f::unit_ccy, text(rest[0]))
            .set(f::ccy, text(rest[1]))
            .set(f::term,
                 term_of(rest[2], {term::kind::period, term::kind::date, term::kind::fx_tenor}))
            .build();
    case it::fx_option:
        // FX_OPTION/qt/unitCcy/ccy/expiry/strike, the strike a label ORE keeps
        // as text: ATM, 25RR, 25BF.
        require_size(rest, {4});
        return datum_builder(t, q)
            .set(f::unit_ccy, text(rest[0]))
            .set(f::ccy, text(rest[1]))
            .set(f::expiry, period(rest[2]))
            .set(f::strike_label, text(rest[3]))
            .build();
    default:
        throw std::logic_error("read_fx called for a type it does not read");
    }
}

}
