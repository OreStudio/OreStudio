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
#include "ores.marketdata.core/vendor/demo_notation.hpp"
#include "notation_support.hpp"
#include <optional>

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view scheme = "demo_key";
constexpr std::string_view only_field = "value";

}

std::string_view demo_notation::provider() const {
    return "demo";
}

std::string_view demo_notation::id_scheme() const {
    return scheme;
}

price_side demo_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
demo_notation::read(const messaging::vendor_id& id) const {
    namespace x3 = detail::x3;
    using detail::fail;
    if (id.id_scheme != scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "Demo ids use the scheme demo_key, not '" + id.id_scheme + "'.");

    // A pillar of a curve: the currency, the index family, the instrument, the tenor it starts at
    // and the tenor it ends at, each its own slash separated word. A pillar has five words where a
    // pair has two, so the pair reader below never sees a pillar.
    {
        ir_curve_pillar pillar;
        const auto curve_key =
            detail::currency_code[detail::assign_to(pillar.currency)] >> x3::lit('/') >>
            detail::index_family[detail::assign_to(pillar.index_family)] >> x3::lit('/') >>
            detail::instrument_code[detail::assign_to(pillar.instrument_code)] >> x3::lit('/') >>
            detail::start_tenor[detail::assign_to(pillar.start_tenor)] >> x3::lit('/') >>
            detail::period_tenor[detail::assign_to(pillar.end_tenor)];
        if (detail::parse_all(id.external_id, curve_key)) {
            if (id.field != only_field)
                return fail(notation_error::kind::wrong_field,
                            "The only Demo field is 'value', not '" + id.field + "'.");
            if (pillar.start_tenor == pillar.end_tenor)
                return fail(notation_error::kind::malformed,
                            "A pillar ends after it starts: '" + id.external_id + "'.");
            return quote_description{pillar};
        }
    }

    std::string base;
    std::string quote;
    const auto pair = detail::currency_code[detail::assign_to(base)] >> x3::lit('/') >>
                      detail::currency_code[detail::assign_to(quote)];
    if (!detail::parse_all(id.external_id, pair))
        return fail(notation_error::kind::malformed, "Not a Demo pair: '" + id.external_id + "'.");
    if (base == quote)
        return fail(notation_error::kind::malformed,
                    "A pair holds two different currencies: '" + id.external_id + "'.");
    if (id.field != only_field)
        return fail(notation_error::kind::wrong_field,
                    "The only Demo field is 'value', not '" + id.field + "'.");
    return quote_description{fx_spot_quote{base, quote, price_side::last}};
}

std::expected<messaging::vendor_id, notation_error>
demo_notation::write(const quote_description& description) const {
    using detail::fail;
    if (const auto* pillar = std::get_if<ir_curve_pillar>(&description)) {
        if (const auto ok = detail::check_pillar(*pillar); !ok)
            return std::unexpected(ok.error());
        return messaging::vendor_id{.id_scheme = std::string(scheme),
                                    .external_id = pillar->currency + "/" + pillar->index_family +
                                                   "/" + pillar->instrument_code + "/" +
                                                   pillar->start_tenor + "/" + pillar->end_tenor,
                                    .field = std::string(only_field)};
    }
    const auto fx = detail::fx_spot_of(description);
    if (!fx)
        return std::unexpected(fx.error());
    if (const auto ok = detail::check_pair(*fx); !ok)
        return std::unexpected(ok.error());
    if (fx->side != price_side::last)
        return fail(notation_error::kind::not_representable,
                    "Demo has one price for a pair, which it reads as the last price.");
    return messaging::vendor_id{.id_scheme = std::string(scheme),
                                .external_id = fx->base + "/" + fx->quote,
                                .field = std::string(only_field)};
}

}
