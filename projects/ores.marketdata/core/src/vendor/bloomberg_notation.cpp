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
#include "ores.marketdata.core/vendor/bloomberg_notation.hpp"
#include "notation_support.hpp"
#include <array>
#include <optional>

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view scheme = "bbg_ticker";
constexpr std::string_view yellow_key = "Curncy";

// The yellow keys of Bloomberg: the market sector that ends a ticker.
bool is_yellow_key(std::string_view word) {
    constexpr std::array<std::string_view, 10> keys{
        "Govt", "Corp", "Mtge", "M-Mkt", "Muni", "Pfd", "Equity", "Comdty", "Index", "Curncy"};
    return std::ranges::find(keys, word) != keys.end();
}

std::string_view field_of(price_side side) {
    switch (side) {
        case price_side::last:
            return "PX_LAST";
        case price_side::bid:
            return "PX_BID";
        case price_side::ask:
            return "PX_ASK";
        case price_side::mid:
            return "PX_MID";
    }
    return "PX_LAST";
}

std::optional<price_side> side_of(std::string_view field) {
    if (field == "PX_LAST")
        return price_side::last;
    if (field == "PX_BID")
        return price_side::bid;
    if (field == "PX_ASK")
        return price_side::ask;
    if (field == "PX_MID")
        return price_side::mid;
    return std::nullopt;
}

}

std::string_view bloomberg_notation::provider() const {
    return "bloomberg";
}

std::string_view bloomberg_notation::id_scheme() const {
    return scheme;
}

price_side bloomberg_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
bloomberg_notation::read(const messaging::vendor_id& id) const {
    namespace x3 = detail::x3;
    using detail::fail;
    if (id.id_scheme != scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "Bloomberg ids use the scheme bbg_ticker, not '" + id.id_scheme + "'.");

    // A pillar of a curve: the currency, the index family, the instrument, the tenor it starts at
    // and the tenor it ends at, as words before the yellow key. Such a ticker names a curve and
    // not a pair, so the pair reader below never sees it.
    {
        ir_curve_pillar pillar;
        std::string family;
        const auto curve_ticker =
            detail::currency_code[detail::assign_to(pillar.currency)] >> x3::lit(' ') >>
            detail::upper_index_family[detail::assign_to(family)] >> x3::lit(' ') >>
            detail::instrument_code[detail::assign_to(pillar.instrument_code)] >> x3::lit(' ') >>
            detail::start_tenor[detail::assign_to(pillar.start_tenor)] >> x3::lit(' ') >>
            detail::period_tenor[detail::assign_to(pillar.end_tenor)] >> x3::lit(' ') >>
            x3::lit(std::string(yellow_key));
        if (detail::parse_all(id.external_id, curve_ticker)) {
            if (id.field != field_of(price_side::last))
                return fail(notation_error::kind::wrong_field,
                            "A Bloomberg curve pillar carries the last price: '" + id.field + "'.");
            pillar.index_family = detail::lower_case(std::move(family));
            if (pillar.start_tenor == pillar.end_tenor)
                return fail(notation_error::kind::malformed,
                            "A pillar ends after it starts: '" + id.external_id + "'.");
            return quote_description{pillar};
        }
    }

    // The ticker of a currency pair: six upper case letters, a space and the yellow key.
    // A ticker with another yellow key is a valid Bloomberg id that this description cannot hold.
    std::string base;
    std::string quote;
    const auto ticker = detail::currency_code[detail::assign_to(base)] >>
                        detail::currency_code[detail::assign_to(quote)] >> x3::lit(' ') >>
                        x3::lit(std::string(yellow_key));
    if (!detail::parse_all(id.external_id, ticker)) {
        // A ticker of words and a yellow key that is not the plain pair-and-Curncy form is a valid
        // Bloomberg id that this description cannot hold: another market sector, or a pair with
        // a pricing source. Anything else is not a Bloomberg ticker at all.
        const auto last_space = id.external_id.rfind(' ');
        if (last_space != std::string::npos &&
            is_yellow_key(id.external_id.substr(last_space + 1))) {
            const auto word = +(x3::upper | x3::digit | x3::char_("/.-"));
            if (detail::parse_all(std::string_view(id.external_id).substr(0, last_space),
                                  word % x3::lit(' ')))
                return fail(notation_error::kind::unsupported,
                            "Only the plain currency pair is described: '" + id.external_id + "'.");
        }
        return fail(notation_error::kind::malformed,
                    "Not a Bloomberg currency ticker: '" + id.external_id + "'.");
    }
    if (base == quote)
        return fail(notation_error::kind::malformed,
                    "A pair holds two different currencies: '" + id.external_id + "'.");

    const auto side = side_of(id.field);
    if (!side)
        return fail(notation_error::kind::wrong_field,
                    "Not a Bloomberg price field: '" + id.field + "'.");
    return quote_description{fx_spot_quote{base, quote, *side}};
}

std::expected<messaging::vendor_id, notation_error>
bloomberg_notation::write(const quote_description& description) const {
    if (const auto* pillar = std::get_if<ir_curve_pillar>(&description)) {
        if (const auto ok = detail::check_pillar(*pillar); !ok)
            return std::unexpected(ok.error());
        return messaging::vendor_id{
            .id_scheme = std::string(scheme),
            .external_id = pillar->currency + " " + detail::upper_case(pillar->index_family) + " " +
                           pillar->instrument_code + " " + pillar->start_tenor + " " +
                           pillar->end_tenor + " " + std::string(yellow_key),
            .field = std::string(field_of(price_side::last))};
    }
    const auto fx = detail::fx_spot_of(description);
    if (!fx)
        return std::unexpected(fx.error());
    if (const auto ok = detail::check_pair(*fx); !ok)
        return std::unexpected(ok.error());
    return messaging::vendor_id{.id_scheme = std::string(scheme),
                                .external_id = fx->base + fx->quote + " " + std::string(yellow_key),
                                .field = std::string(field_of(fx->side))};
}

}
