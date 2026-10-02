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
 * @file ore_key_commodity.cpp
 * @brief The commodity instruments: spot, forward, option and calendar spread
 * option. ORE's parseMarketDatum, lines 842 to 935.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_commodity_option(quote_type q, tokens rest) {
    // COMMODITY_OPTION/qt/name/ccy/expiry/strike[/C|P], the strike in ORE's
    // strike grammar and the quote type a volatility or PRICE.
    // COMMODITY_OPTION/SHIFT/name
    using qt = quote_type;
    if (q == qt::shift) {
        require_size(rest, {1});
        return datum_builder(it::commodity_option, q).set(f::commodity_name, text(rest[0])).build();
    }
    require_quote(q, {qt::rate_lnvol, qt::rate_nvol, qt::rate_slnvol, qt::price});
    if (rest.size() < 4)
        refuse("a commodity option is name, ccy, expiry and strike");
    const std::size_t cp = (rest.back() == "C" || rest.back() == "P") ? 1 : 0;
    const auto strike_tokens = rest.subspan(3, rest.size() - 3 - cp);
    if (strike_tokens.empty())
        refuse("a commodity option needs a strike");

    datum_builder b(it::commodity_option, q);
    b.set(f::commodity_name, text(rest[0]))
        .set(f::ccy, text(rest[1]))
        .set(f::expiry, expiry(rest[2]))
        .set(f::strike, base_strike(join(strike_tokens)));
    if (cp)
        b.set(f::option_type, token(rest.back()));
    return b.build();
}

}

market_datum read_commodity(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
        case it::commodity_spot:
            // COMMODITY/PRICE/name/ccy
            require_quote(q, {quote_type::price});
            require_size(rest, {2});
            return datum_builder(t, q)
                .set(f::commodity_name, text(rest[0]))
                .set(f::ccy, text(rest[1]))
                .build();
        case it::commodity_fwd:
            // COMMODITY_FWD/PRICE/name/ccy/expiry, the expiry ON, TN, SN, a period
            // or a date.
            require_quote(q, {quote_type::price});
            require_size(rest, {3});
            return datum_builder(t, q)
                .set(f::commodity_name, text(rest[0]))
                .set(f::ccy, text(rest[1]))
                .set(f::expiry,
                     term_of(rest[2], {term::kind::period, term::kind::date, term::kind::fx_tenor}))
                .build();
        case it::commodity_option:
            return read_commodity_option(q, rest);
        case it::commodity_calendar_spread_option:
            // COMMODITY_CALENDAR_SPREAD_OPTION/RATE_NVOL/name/offset/ccy/expiry/strike
            require_quote(q, {quote_type::rate_nvol});
            require_size(rest, {5});
            return datum_builder(t, q)
                .set(f::commodity_name, text(rest[0]))
                .set(f::offset, integer(rest[1]))
                .set(f::ccy, text(rest[2]))
                .set(f::expiry, expiry(rest[3]))
                .set(f::strike, base_strike(rest[4]))
                .build();
        default:
            throw std::logic_error("read_commodity called for a type it does not read");
    }
}

}
