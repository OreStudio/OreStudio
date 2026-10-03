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
 * @file ore_key_volatility.cpp
 * @brief The interest rate volatility instruments: SWAPTION, CAPFLOOR and
 * BOND_OPTION, each with its volatility and its shift forms. ORE's
 * parseMarketDatum, lines 525 to 604.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_swaption(quote_type q, tokens rest) {
    // SWAPTION/qt/ccy[/tag]/expiry/term/ATM[/P|R]
    // SWAPTION/qt/ccy[/tag]/expiry/term/Smile/strike[/P|R]
    // SWAPTION/qt/ccy[/tag]/term                       (the shift)
    // A tag is present when the token after the currency is not one period. ORE
    // counts all tokens, so n here is the key's own count.
    const auto n = rest.size() + 2;
    if (n < 4 || n > 9)
        refuse("a swaption key has four to nine tokens");
    const std::size_t off = is_one_period(rest[1]) ? 0 : 1;
    const std::size_t pr = (rest.back() == "P" || rest.back() == "R") ? 1 : 0;
    if (q == quote_type::price && pr == 0)
        refuse("a swaption PRICE must end in P or R");

    datum_builder b(it::swaption, q);
    b.set(f::ccy, text(rest[0]));
    if (off)
        b.set(f::quote_tag, text(rest[1]));

    if (n >= 6 + off + pr) {
        const auto& dimension = rest[3 + off];
        if (dimension == "ATM") {
            if (n != 6 + off + pr)
                refuse("an ATM swaption takes no strike");
        } else if (dimension == "Smile") {
            if (n != 7 + off + pr)
                refuse("a Smile swaption takes one strike");
            b.set(f::strike_level, number(rest[4 + off]));
        } else {
            refuse("a swaption's dimension is ATM or Smile");
        }
        b.set(f::expiry, period(rest[1 + off]))
            .set(f::term, period(rest[2 + off]))
            .set(f::dimension, token(dimension));
        if (pr)
            b.set(f::payer_receiver, token(rest.back()));
        return b.build();
    }

    // The shift quote. ORE ignores anything after the term; the datum cannot
    // keep what it would not write back, so that is refused.
    if (n != 4 + off)
        refuse("a swaption shift is ccy, an optional tag and a term");
    require_quote(q, {quote_type::shift});
    return b.set(f::term, period(rest[1 + off])).build();
}

market_datum read_capfloor(quote_type q, tokens rest) {
    // CAPFLOOR/qt/ccy[/index]/term/tenor/atm/relative/strike[/C|F]
    // CAPFLOOR/qt/ccy[/index]/indexTenor                 (the shift)
    // An index is present when the key has one token more than its form needs.
    const auto n = rest.size() + 2;
    const std::size_t cf = (rest.back() == "C" || rest.back() == "F") ? 1 : 0;
    if (q == quote_type::price && cf == 0)
        refuse("a cap/floor PRICE must end in C or F");
    const std::size_t off = (n == 9 + cf || n == 5 + cf) ? 1 : 0;

    datum_builder b(it::capfloor, q);
    b.set(f::ccy, text(rest[0]));
    if (off)
        b.set(f::index_name, text(rest[1]));

    if (n == 8 + cf || n == 9 + cf) {
        b.set(f::term, period(rest[1 + off]))
            .set(f::index_tenor, period(rest[2 + off]))
            .set(f::atm, token(rest[3 + off]))
            .set(f::relative, token(rest[4 + off]))
            .set(f::strike_level, number(rest[5 + off]));
        if (cf)
            b.set(f::cap_floor, token(rest.back()));
        return b.build();
    }

    // The shift quote: four tokens, or five with an index, and no flag. ORE
    // would read other counts and ignore tokens; they are refused.
    if (cf || (n != 4 && n != 5))
        refuse("a cap/floor is a volatility of eight to ten tokens or a shift of four or five");
    require_quote(q, {quote_type::shift});
    return b.set(f::index_tenor, period(rest[1 + off])).build();
}

market_datum read_bond_option(quote_type q, tokens rest) {
    // BOND_OPTION/qt/qualifier/expiry/term/ATM
    // BOND_OPTION/qt/qualifier/term                     (the shift)
    require_size(rest, {2, 4});
    datum_builder b(it::bond_option, q);
    b.set(f::qualifier, text(rest[0]));
    if (rest.size() == 2) {
        require_quote(q, {quote_type::shift});
        return b.set(f::term, period(rest[1])).build();
    }
    if (rest[3] != "ATM")
        refuse("a bond option volatility is ATM only");
    return b.set(f::expiry, period(rest[1]))
        .set(f::term, period(rest[2]))
        .set(f::dimension, token(rest[3]))
        .build();
}

}

market_datum read_volatility(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
        case it::swaption:
            return read_swaption(q, rest);
        case it::capfloor:
            return read_capfloor(q, rest);
        case it::bond_option:
            return read_bond_option(q, rest);
        default:
            throw std::logic_error("read_volatility called for a type it does not read");
    }
}

}
