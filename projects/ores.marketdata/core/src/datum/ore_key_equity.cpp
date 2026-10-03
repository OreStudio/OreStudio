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
 * @file ore_key_equity.cpp
 * @brief The equity instruments: spot, forward, dividend and option. ORE's
 * parseMarketDatum, lines 675 to 735.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_equity_option(quote_type q, tokens rest) {
    // EQUITY_OPTION/qt/name/ccy/expiry/strike[/C|P], the strike any number of
    // tokens: ATM, ATMF, an absolute level, or the ATM, delta and moneyness
    // forms of ORE's strike grammar.
    require_quote(q, {quote_type::rate_lnvol, quote_type::price});
    if (rest.size() < 4)
        refuse("an equity option is name, ccy, expiry and strike");
    const std::size_t cp = (rest.back() == "C" || rest.back() == "P") ? 1 : 0;
    const auto strike_tokens = rest.subspan(3, rest.size() - 3 - cp);
    if (strike_tokens.empty())
        refuse("an equity option needs a strike");
    auto parsed = strike::parse(join(strike_tokens));
    if (!parsed)
        refuse(parsed.error());

    datum_builder b(it::equity_option, q);
    b.set(f::eq_name, text(rest[0]))
        .set(f::ccy, text(rest[1]))
        .set(f::expiry, period_or_date(rest[2]))
        .set(f::strike, std::move(*parsed));
    if (cp)
        b.set(f::option_type, token(rest.back()));
    return b.build();
}

}

market_datum read_equity(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
        case it::equity_spot:
            // EQUITY/PRICE/name/ccy
            require_quote(q, {quote_type::price});
            require_size(rest, {2});
            return datum_builder(t, q)
                .set(f::eq_name, text(rest[0]))
                .set(f::ccy, text(rest[1]))
                .build();
        case it::equity_fwd:
        case it::equity_dividend:
            // EQUITY_FWD/PRICE/name/ccy/expiry and EQUITY_DIVIDEND/RATE/name/ccy/expiry,
            // the expiry a period or a date.
            require_quote(q, {t == it::equity_fwd ? quote_type::price : quote_type::rate});
            require_size(rest, {3});
            return datum_builder(t, q)
                .set(f::eq_name, text(rest[0]))
                .set(f::ccy, text(rest[1]))
                .set(f::expiry, period_or_date(rest[2]))
                .build();
        case it::equity_option:
            return read_equity_option(q, rest);
        default:
            throw std::logic_error("read_equity called for a type it does not read");
    }
}

}
