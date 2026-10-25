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

/**
 * @brief A tenor of a curve template: a count of days, weeks, months or years, such as 1D, 3M or
 * 2Y.
 *
 * The count is one to three digits and does not begin with a zero, because a tenor the catalogue
 * holds is written one way. A template names its pillars by these codes and by SPOT, so the
 * grammar keeps to them and a date is not a tenor.
 */
inline const auto period_tenor =
    x3::raw[x3::char_("1-9") >> x3::repeat(0, 2)[x3::digit] >> x3::char_("DWMY")];

/// A pillar's start: SPOT, or a period. A pillar never starts and ends nowhere.
inline const auto start_tenor =
    x3::raw[x3::string("SPOT") |
            (x3::char_("1-9") >> x3::repeat(0, 2)[x3::digit] >> x3::char_("DWMY"))];

/**
 * @brief A refdata instrument code, such as DEPO, FRA or IRS: two to eight upper case letters.
 *
 * The grammar stops at the shape of the code and not at the codes the catalogue happens to hold,
 * so the notation does not carry a second copy of that catalogue.
 */
inline const auto instrument_code = x3::raw[x3::repeat(2, 8)[x3::upper]];

/**
 * @brief An index family, such as sofr: one to eight lower case letters, in the catalogue's own
 * spelling.
 *
 * A provider that writes the family in upper case does so in its id and reads it back to lower
 * case, so the description holds one spelling whichever provider wrote the id.
 */
inline const auto index_family = x3::raw[x3::repeat(1, 8)[x3::lower]];

/// The same family as a provider that spells it in upper case writes it.
inline const auto upper_index_family = x3::raw[x3::repeat(1, 8)[x3::upper]];

/// @p text in lower case, as the catalogue spells an index family.
inline std::string lower_case(std::string text) {
    std::ranges::transform(text, text.begin(), [](unsigned char c) {
        return static_cast<char>(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
    });
    return text;
}

/// @p text in upper case, as a provider that spells an index family so writes it.
inline std::string upper_case(std::string text) {
    std::ranges::transform(text, text.begin(), [](unsigned char c) {
        return static_cast<char>(c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
    });
    return text;
}

/// The ir_curve_pillar of a description, or an error when it holds another alternative.
inline std::expected<ir_curve_pillar, notation_error> ir_pillar_of(const quote_description& d) {
    if (const auto* pillar = std::get_if<ir_curve_pillar>(&d))
        return *pillar;
    return fail(notation_error::kind::not_representable,
                "The notation writes only one pillar of an interest rate curve.");
}

/**
 * @brief A pillar that can be written: a currency, an index family, an instrument code and two
 * tenors, the first of which the pillar ends after.
 */
inline std::expected<void, notation_error> check_pillar(const ir_curve_pillar& p) {
    const auto text = p.currency + "/" + p.index_family + "/" + p.instrument_code + "/" +
                      p.start_tenor + "/" + p.end_tenor;
    if (!is_currency_code(p.currency))
        return fail(notation_error::kind::not_representable,
                    "A currency is three upper case letters: '" + text + "'.");
    if (!parse_all(p.index_family, index_family))
        return fail(notation_error::kind::not_representable,
                    "An index family is one to eight lower case letters: '" + text + "'.");
    if (!parse_all(p.instrument_code, instrument_code))
        return fail(notation_error::kind::not_representable,
                    "An instrument code is two to eight upper case letters: '" + text + "'.");
    if (!parse_all(p.start_tenor, start_tenor) || !parse_all(p.end_tenor, period_tenor))
        return fail(notation_error::kind::not_representable,
                    "A pillar starts at SPOT or at a period and ends at a period: '" + text + "'.");
    if (p.start_tenor == p.end_tenor)
        return fail(notation_error::kind::not_representable,
                    "A pillar ends after it starts: '" + text + "'.");
    return {};
}

}

#endif
