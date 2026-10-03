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
 * @file ore_key_securities.cpp
 * @brief The instruments keyed by a security, a contract or a named pair:
 * bonds, bond futures and their options, correlation, CPR, rating transitions
 * and intraday power shape factors. ORE's parseMarketDatum, lines 737 to 762
 * and 937 to 1016.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_bond_future(quote_type q, tokens rest) {
    // BOND_FUTURE/PRICE/securityID
    // BOND_FUTURE/CONVERSION_FACTOR/securityID/futureContract
    require_quote(q, {quote_type::price, quote_type::conversion_factor});
    datum_builder b(it::bond_future, q);
    if (q == quote_type::price) {
        require_size(rest, {1});
        return b.set(f::security_id, text(rest[0])).build();
    }
    require_size(rest, {2});
    return b.set(f::security_id, text(rest[0])).set(f::future_contract, text(rest[1])).build();
}

market_datum read_bond_future_option(quote_type q, tokens rest) {
    // BOND_FUTURE_OPTION/qt/contract/expiry/strike[/C|P], the call or put
    // required for PRICE.
    require_quote(q, {quote_type::rate_lnvol, quote_type::price});
    if (q == quote_type::price)
        require_size(rest, {4});
    require_size(rest, {3, 4});
    datum_builder b(it::bond_future_option, q);
    b.set(f::contract_name, text(rest[0]))
        .set(f::expiry, period_or_date(rest[1]))
        .set(f::strike, base_strike(rest[2]));
    if (rest.size() == 4) {
        if (rest[3] != "C" && rest[3] != "P")
            refuse("a bond future option ends in C or P");
        b.set(f::option_type, token(rest[3]));
    }
    return b.build();
}

market_datum read_shape_profile(quote_type q, tokens rest) {
    // SHAPE_PROFILE/SHAPE_FACTOR/name/deliveryDate/startTimeInSec/unit[/DST].
    // ORE reads any sixth token as "not DST" and ignores tokens past it; only
    // DST can be written back, so anything else is refused.
    require_quote(q, {quote_type::shape_factor});
    require_size(rest, {4, 5});
    datum_builder b(it::shape_profile, q);
    b.set(f::quote_name, text(rest[0]))
        .set(f::delivery_date, term_of(rest[1], {term::kind::date}))
        .set(f::start_time_in_sec, integer(rest[2]))
        .set(f::time_unit, token(rest[3]));
    if (rest.size() == 5)
        b.set(f::dst, token(rest[4]));
    return b.build();
}

}

market_datum read_securities(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
        case it::bond:
            // BOND/PRICE|YIELD_SPREAD/securityID
            require_quote(q, {quote_type::price, quote_type::yield_spread});
            require_size(rest, {1});
            return datum_builder(t, q).set(f::security_id, text(rest[0])).build();
        case it::bond_future:
            return read_bond_future(q, rest);
        case it::bond_future_option:
            return read_bond_future_option(q, rest);
        case it::correlation:
            // CORRELATION/RATE|PRICE/index1/index2/expiry/strike, the strike a
            // label ORE keeps as text.
            require_quote(q, {quote_type::rate, quote_type::price});
            require_size(rest, {4});
            return datum_builder(t, q)
                .set(f::index1, text(rest[0]))
                .set(f::index2, text(rest[1]))
                .set(f::expiry, period_or_date(rest[2]))
                .set(f::strike_label, text(rest[3]))
                .build();
        case it::cpr:
            // CPR/RATE/securityID
            require_quote(q, {quote_type::rate});
            require_size(rest, {1});
            return datum_builder(t, q).set(f::security_id, text(rest[0])).build();
        case it::rating:
            // RATING/TRANSITION_PROBABILITY/name/from/to
            require_quote(q, {quote_type::transition_probability});
            require_size(rest, {3});
            return datum_builder(t, q)
                .set(f::rating_name, text(rest[0]))
                .set(f::from_rating, text(rest[1]))
                .set(f::to_rating, text(rest[2]))
                .build();
        case it::shape_profile:
            return read_shape_profile(q, rest);
        default:
            throw std::logic_error("read_securities called for a type it does not read");
    }
}

}
