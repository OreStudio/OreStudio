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
 * @file ore_key_rates.cpp
 * @brief The interest rate curve instruments: ZERO, DISCOUNT, MM, the futures,
 * the FRAs and the swaps. ORE's parseMarketDatum, lines 274 to 425.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_zero(quote_type q, tokens rest) {
    // ZERO/RATE|YIELD_SPREAD/ccy/curve/daycounter/tenor-or-date
    require_quote(q, {quote_type::rate, quote_type::yield_spread});
    require_size(rest, {4});
    return datum_builder(it::zero, q)
        .set(f::ccy, text(rest[0]))
        .set(f::curve_id, text(rest[1]))
        .set(f::day_counter, text(rest[2]))
        .set(f::term, period_or_date(rest[3]))
        .build();
}

market_datum read_discount(quote_type q, tokens rest) {
    // DISCOUNT/RATE/ccy/curve/tenor-or-date
    require_size(rest, {3});
    return datum_builder(it::discount, q)
        .set(f::ccy, text(rest[0]))
        .set(f::curve_id, text(rest[1]))
        .set(f::term, period_or_date(rest[2]))
        .build();
}

market_datum read_mm(quote_type q, tokens rest) {
    // MM/RATE/ccy[/index]/fwdStart/term
    require_size(rest, {3, 4});
    const std::size_t off = rest.size() == 4 ? 1 : 0;
    datum_builder b(it::mm, q);
    b.set(f::ccy, text(rest[0]));
    if (off)
        b.set(f::index_name, text(rest[1]));
    return b.set(f::fwd_start, period(rest[1 + off])).set(f::term, period(rest[2 + off])).build();
}

market_datum read_future(instrument_type t, quote_type q, tokens rest) {
    // MM_FUTURE|OI_FUTURE/PRICE/ccy/contract month/contract/tenor
    require_size(rest, {4});
    return datum_builder(t, q)
        .set(f::ccy, text(rest[0]))
        .set(f::contract_month, text(rest[1]))
        .set(f::contract, text(rest[2]))
        .set(f::tenor, period(rest[3]))
        .build();
}

market_datum read_fra(quote_type q, tokens rest) {
    // FRA/RATE/ccy/fwdStart/term
    require_size(rest, {3});
    return datum_builder(it::fra, q)
        .set(f::ccy, text(rest[0]))
        .set(f::fwd_start, period(rest[1]))
        .set(f::term, period(rest[2]))
        .build();
}

market_datum read_imm_fra(quote_type q, tokens rest) {
    // IMM_FRA/RATE/ccy/imm1/imm2, with the second IMM date after the first.
    require_size(rest, {3});
    const auto imm1 = integer(rest[1]);
    const auto imm2 = integer(rest[2]);
    if (std::stoul(imm2.text()) <= std::stoul(imm1.text()))
        refuse("the second IMM date must come after the first");
    return datum_builder(it::imm_fra, q)
        .set(f::ccy, text(rest[0]))
        .set(f::imm1, imm1)
        .set(f::imm2, imm2)
        .build();
}

market_datum read_ir_swap(quote_type q, tokens rest) {
    // IR_SWAP/RATE/ccy[/index]/start/tenor/end, with start and end both periods
    // or both dates.
    require_size(rest, {4, 5});
    const std::size_t off = rest.size() == 5 ? 1 : 0;
    auto start = period_or_date(rest[1 + off]);
    auto end = period_or_date(rest[3 + off]);
    if (start.which() != end.which())
        refuse("a swap's start and end must both be periods or both be dates");
    datum_builder b(it::ir_swap, q);
    b.set(f::ccy, text(rest[0]));
    if (off)
        b.set(f::index_name, text(rest[1]));
    return b.set(f::fwd_start, std::move(start))
        .set(f::tenor, period(rest[2 + off]))
        .set(f::term, std::move(end))
        .build();
}

market_datum read_basis_swap(quote_type q, tokens rest) {
    // BASIS_SWAP/BASIS_SPREAD/flatTerm/term/ccy[/identifier]/maturity. ORE
    // ignores the identifier; the datum keeps it.
    require_size(rest, {4, 5});
    datum_builder b(it::basis_swap, q);
    b.set(f::flat_term, period(rest[0])).set(f::term, period(rest[1])).set(f::ccy, text(rest[2]));
    if (rest.size() == 5)
        b.set(f::identifier, text(rest[3]));
    return b.set(f::maturity, period(rest.back())).build();
}

market_datum read_bma_swap(quote_type q, tokens rest) {
    // BMA_SWAP/RATIO/ccy/term/maturity
    require_size(rest, {3});
    return datum_builder(it::bma_swap, q)
        .set(f::ccy, text(rest[0]))
        .set(f::term, period(rest[1]))
        .set(f::maturity, period(rest[2]))
        .build();
}

market_datum read_cc_basis_swap(quote_type q, tokens rest) {
    // CC_BASIS_SWAP/BASIS_SPREAD/flatCcy/flatTerm/ccy/term/maturity
    require_size(rest, {5});
    return datum_builder(it::cc_basis_swap, q)
        .set(f::flat_ccy, text(rest[0]))
        .set(f::flat_term, period(rest[1]))
        .set(f::ccy, text(rest[2]))
        .set(f::term, period(rest[3]))
        .set(f::maturity, period(rest[4]))
        .build();
}

market_datum read_cc_fix_float_swap(quote_type q, tokens rest) {
    // CC_FIX_FLOAT_SWAP/RATE/floatCcy/floatTenor/fixedCcy/fixedTenor/maturity
    require_size(rest, {5});
    return datum_builder(it::cc_fix_float_swap, q)
        .set(f::float_ccy, text(rest[0]))
        .set(f::float_tenor, period(rest[1]))
        .set(f::fixed_ccy, text(rest[2]))
        .set(f::fixed_tenor, period(rest[3]))
        .set(f::maturity, period(rest[4]))
        .build();
}

}

market_datum read_rates(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
    case it::zero:
        return read_zero(q, rest);
    case it::discount:
        return read_discount(q, rest);
    case it::mm:
        return read_mm(q, rest);
    case it::mm_future:
    case it::oi_future:
        return read_future(t, q, rest);
    case it::fra:
        return read_fra(q, rest);
    case it::imm_fra:
        return read_imm_fra(q, rest);
    case it::ir_swap:
        return read_ir_swap(q, rest);
    case it::basis_swap:
        return read_basis_swap(q, rest);
    case it::bma_swap:
        return read_bma_swap(q, rest);
    case it::cc_basis_swap:
        return read_cc_basis_swap(q, rest);
    case it::cc_fix_float_swap:
        return read_cc_fix_float_swap(q, rest);
    default:
        throw std::logic_error("read_rates called for a type it does not read");
    }
}

}
