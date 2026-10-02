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
 * @file ore_key_inflation.cpp
 * @brief The inflation instruments: zero coupon and year-on-year swaps and
 * cap/floors, and seasonality. ORE's parseMarketDatum, lines 630 to 674.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

}

market_datum read_inflation(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
    case it::zc_inflation_swap:
    case it::yy_inflation_swap:
        // ZC_INFLATIONSWAP|YY_INFLATIONSWAP/RATE/index/term
        require_size(rest, {2});
        return datum_builder(t, q).set(f::index, text(rest[0])).set(f::term, period(rest[1])).build();
    case it::zc_inflation_capfloor:
    case it::yy_inflation_capfloor:
        // ZC_INFLATIONCAPFLOOR|YY_INFLATIONCAPFLOOR/qt/index/term/C|F/strike
        require_size(rest, {4});
        return datum_builder(t, q)
            .set(f::index, text(rest[0]))
            .set(f::term, period(rest[1]))
            .set(f::cap_floor, token(rest[2]))
            .set(f::strike_level, number(rest[3]))
            .build();
    case it::seasonality:
        // SEASONALITY/RATE/type/index/month
        require_size(rest, {3});
        return datum_builder(t, q)
            .set(f::seasonality_type, text(rest[0]))
            .set(f::index, text(rest[1]))
            .set(f::month, text(rest[2]))
            .build();
    default:
        throw std::logic_error("read_inflation called for a type it does not read");
    }
}

}
