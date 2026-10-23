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
#include "ores.marketdata.core/vendor/lseg_notation.hpp"
#include "notation_support.hpp"
#include <array>
#include <optional>

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view scheme = "ric";

// The currencies the market quotes as the base against USD, so that EUR/USD is written "EUR=".
// Every other currency is quoted as the quote of USD, so that USD/JPY is written "JPY=".
constexpr std::array<std::string_view, 4> base_against_usd{"EUR", "GBP", "AUD", "NZD"};

bool is_base_against_usd(std::string_view code) {
    return std::ranges::find(base_against_usd, code) != base_against_usd.end();
}

std::string_view field_of(price_side side) {
    switch (side) {
        case price_side::bid:
            return "BID";
        case price_side::ask:
            return "ASK";
        case price_side::last:
            return "TRDPRC_1";
        case price_side::mid:
            return {};
    }
    return {};
}

std::optional<price_side> side_of(std::string_view field) {
    if (field == "BID")
        return price_side::bid;
    if (field == "ASK")
        return price_side::ask;
    if (field == "TRDPRC_1")
        return price_side::last;
    return std::nullopt;
}

}

std::string_view lseg_notation::provider() const {
    return "lseg";
}

std::string_view lseg_notation::id_scheme() const {
    return scheme;
}

price_side lseg_notation::primary_side() const {
    return price_side::bid;
}

std::expected<quote_description, notation_error>
lseg_notation::read(const messaging::vendor_id& id) const {
    namespace x3 = detail::x3;
    using detail::fail;
    if (id.id_scheme != scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "LSEG ids use the scheme ric, not '" + id.id_scheme + "'.");

    // "EURGBP=" is a cross of two currencies, and "EUR=" is a USD pair written with the other
    // currency. The longer form is tried first.
    std::string first;
    std::string second;
    fx_spot_quote quote;
    const auto cross = detail::currency_code[detail::assign_to(first)] >>
                       detail::currency_code[detail::assign_to(second)] >> x3::lit('=');
    const auto usd_pair = detail::currency_code[detail::assign_to(first)] >> x3::lit('=');
    if (detail::parse_all(id.external_id, cross)) {
        if (first == second || first == "USD" || second == "USD")
            return fail(notation_error::kind::malformed,
                        "A pair that holds USD is written with the other currency alone: '" +
                            id.external_id + "'.");
        quote.base = first;
        quote.quote = second;
    } else if (detail::parse_all(id.external_id, usd_pair)) {
        if (first == "USD")
            return fail(notation_error::kind::malformed,
                        "USD alone is not a pair: '" + id.external_id + "'.");
        if (is_base_against_usd(first)) {
            quote.base = first;
            quote.quote = "USD";
        } else {
            quote.base = "USD";
            quote.quote = first;
        }
    } else {
        return fail(notation_error::kind::malformed,
                    "Not an LSEG currency RIC: '" + id.external_id + "'.");
    }

    const auto side = side_of(id.field);
    if (!side)
        return fail(notation_error::kind::wrong_field,
                    "Not an LSEG price field: '" + id.field + "'.");
    quote.side = *side;
    return quote_description{quote};
}

std::expected<messaging::vendor_id, notation_error>
lseg_notation::write(const quote_description& description) const {
    using detail::fail;
    const auto fx = detail::fx_spot_of(description);
    if (!fx)
        return std::unexpected(fx.error());
    if (const auto ok = detail::check_pair(*fx); !ok)
        return std::unexpected(ok.error());
    const auto field = field_of(fx->side);
    if (field.empty())
        return fail(notation_error::kind::not_representable, "LSEG has no field for a mid price.");

    std::string ric;
    if (fx->quote == "USD" && is_base_against_usd(fx->base))
        ric = fx->base + "=";
    else if (fx->base == "USD" && !is_base_against_usd(fx->quote))
        ric = fx->quote + "=";
    else if (fx->base == "USD" || fx->quote == "USD")
        return fail(notation_error::kind::not_representable,
                    "The RIC of " + fx->base + "/" + fx->quote +
                        " would read back as the other order of the pair.");
    else
        ric = fx->base + fx->quote + "=";
    return messaging::vendor_id{
        .id_scheme = std::string(scheme), .external_id = ric, .field = std::string(field)};
}

}
