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
#ifndef ORES_MARKETDATA_CORE_SRC_VENDOR_NOTATION_SUPPORT_HPP
#define ORES_MARKETDATA_CORE_SRC_VENDOR_NOTATION_SUPPORT_HPP

#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include <boost/spirit/home/x3.hpp>
#include <algorithm>
#include <expected>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::vendor::detail {

namespace x3 = boost::spirit::x3;

/// An error of the given kind.
inline std::unexpected<notation_error> fail(notation_error::kind reason, std::string message) {
    return std::unexpected(notation_error{reason, std::move(message)});
}

/// Whether @p code is a currency code: three upper case ASCII letters.
inline bool is_currency_code(std::string_view code) {
    return code.size() == 3 &&
           std::ranges::all_of(code, [](unsigned char c) { return c >= 'A' && c <= 'Z'; });
}

/// Three upper case letters, kept as text.
inline const auto currency_code = x3::raw[x3::repeat(3)[x3::upper]];

/// The text a currency_code matched, written into @p out.
inline auto assign_to(std::string& out) {
    return [&out](auto& ctx) {
        const auto& range = x3::_attr(ctx);
        out.assign(range.begin(), range.end());
    };
}

/// Parses all of @p text with @p grammar. Anything left over is a failure.
template <typename Grammar>
bool parse_all(std::string_view text, const Grammar& grammar) {
    auto first = text.begin();
    return x3::parse(first, text.end(), grammar >> x3::eoi);
}

/// The fx_spot_quote of a description, or an error when it holds another alternative.
inline std::expected<fx_spot_quote, notation_error> fx_spot_of(const quote_description& d) {
    if (const auto* fx = std::get_if<fx_spot_quote>(&d))
        return *fx;
    return fail(notation_error::kind::not_representable,
                "The notation writes only the spot quote of a currency pair.");
}

/// A pair that can be written: two different currency codes.
inline std::expected<void, notation_error> check_pair(const fx_spot_quote& q) {
    if (!is_currency_code(q.base) || !is_currency_code(q.quote))
        return fail(notation_error::kind::not_representable,
                    "A currency is three upper case letters: " + q.base + "/" + q.quote + ".");
    if (q.base == q.quote)
        return fail(notation_error::kind::not_representable,
                    "A pair holds two different currencies: " + q.base + "/" + q.quote + ".");
    return {};
}

}

#endif
