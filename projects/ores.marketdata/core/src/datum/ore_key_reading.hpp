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
#ifndef ORES_MARKETDATA_CORE_DATUM_ORE_KEY_READING_HPP
#define ORES_MARKETDATA_CORE_DATUM_ORE_KEY_READING_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include <initializer_list>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file ore_key_reading.hpp
 * @brief What the per-family key readers share. Private to the codec.
 *
 * A reader receives the quote type and the tokens after the quote token, and
 * throws refusal when ORE's grammar does not admit them. The tokens are views
 * into the key, which outlives the reader's call.
 */

namespace ores::marketdata::datum::detail {

/// Why a key is not one ORE's grammar admits.
struct refusal : std::runtime_error {
    using std::runtime_error::runtime_error;
};

[[noreturn]] void refuse(const std::string& why);

using tokens = std::span<const std::string_view>;

/// The fields of one datum, all none until a reader sets them.
class datum_builder final {
public:
    datum_builder(instrument_type type, quote_type quote);

    datum_builder& set(field f, value v);

    /// The datum, checked against its schema row; throws refusal on a mismatch.
    [[nodiscard]] market_datum build();

private:
    instrument_type type_;
    quote_type quote_;
    std::vector<field_value> fields_;
};

/// A free-text token: any non-empty text, kept as written.
[[nodiscard]] std::string text(std::string_view t);

/// A term of one of the kinds @p allowed.
[[nodiscard]] term term_of(std::string_view t, std::initializer_list<term::kind> allowed);
[[nodiscard]] term period(std::string_view t);
[[nodiscard]] term period_or_date(std::string_view t);

/// An expiry as ORE's parseExpiry reads one: a period, a date or a continuation.
[[nodiscard]] term expiry(std::string_view t);

[[nodiscard]] decimal number(std::string_view t);

/// A non-negative integer, as ORE's parseInteger reads one in a key.
[[nodiscard]] decimal integer(std::string_view t);

[[nodiscard]] code token(std::string_view t);

/// A strike in ORE's parseBaseStrike grammar: no ATM or ATMF shorthand, which
/// only the equity option reader adds.
[[nodiscard]] strike base_strike(std::string_view t);

/// An FX option strike label in its canonical spelling: ATM, 25RR, 25BF, 25C,
/// 25P or a level.
[[nodiscard]] strike_label strike_label_of(std::string_view t);

/// @p parts joined with '/'.
[[nodiscard]] std::string join(tokens parts);

void require_size(tokens rest, std::initializer_list<std::size_t> allowed);
void require_quote(quote_type q, std::initializer_list<quote_type> allowed);

/// ORE's isOnePeriod: digits followed by one of D, W, M or Y, in either case.
[[nodiscard]] bool is_one_period(std::string_view t);

/// ORE's CDS documentation clauses: CR, MM, MR, XR and their 2014 forms.
[[nodiscard]] bool is_doc_clause(std::string_view t);

[[nodiscard]] bool is_number(std::string_view t);

// One reader per family, each covering the types its file names.
[[nodiscard]] market_datum read_rates(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_credit(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_volatility(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_fx(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_inflation(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_equity(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_commodity(instrument_type t, quote_type q, tokens rest);
[[nodiscard]] market_datum read_securities(instrument_type t, quote_type q, tokens rest);

}

#endif
