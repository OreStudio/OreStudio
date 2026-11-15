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
#include "ores.marketdata.core/vendor/ice_notation.hpp"
#include "notation_support.hpp"

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view ice_notation_scheme = "ice_ticker";
constexpr std::string_view ice_notation_field = "BID";

}

std::string_view ice_notation::provider() const {
    return "ice";
}

std::string_view ice_notation::id_scheme() const {
    return ice_notation_scheme;
}

price_side ice_notation::primary_side() const {
    return price_side::bid;
}

bool ice_notation::names_index_tenor() const {
    return true;
}

std::expected<quote_description, notation_error>
ice_notation::read(const messaging::vendor_id& id) const {
    using detail::fail;
    if (id.id_scheme != ice_notation_scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "ICE ids use the scheme ice_ticker, not '" + id.id_scheme + "'.");

    ir_curve_pillar pillar;
    if (!detail::parse_dot_pillar_with_tenor(id.external_id, pillar))
        return fail(notation_error::kind::malformed,
                    "Not an ICE curve pillar: '" + id.external_id + "'.");
    if (id.field != ice_notation_field)
        return fail(notation_error::kind::wrong_field,
                    "An ICE curve pillar carries the bid price: '" + id.field + "'.");
    if (pillar.start_tenor == pillar.end_tenor)
        return fail(notation_error::kind::malformed,
                    "A pillar ends after it starts: '" + id.external_id + "'.");
    return quote_description{pillar};
}

std::expected<messaging::vendor_id, notation_error>
ice_notation::write(const quote_description& description) const {
    using detail::fail;
    const auto pillar = detail::ir_pillar_of(description);
    if (!pillar)
        return std::unexpected(pillar.error());
    if (const auto ok = detail::check_pillar(*pillar); !ok)
        return std::unexpected(ok.error());
    if (pillar->index_tenor.empty())
        return fail(
            notation_error::kind::not_representable,
            "An ICE id names the tenor of the curve's index, which this quote leaves empty.");
    return messaging::vendor_id{.id_scheme = std::string(ice_notation_scheme),
                                .external_id = detail::write_dot_pillar_with_tenor(*pillar),
                                .field = std::string(ice_notation_field)};
}

}
