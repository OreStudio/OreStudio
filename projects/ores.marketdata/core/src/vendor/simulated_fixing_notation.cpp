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
#include "ores.marketdata.core/vendor/simulated_fixing_notation.hpp"
#include "notation_support.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"

namespace ores::marketdata::vendor {

namespace {

constexpr std::string_view simulated_fixing_scheme = "oresmd";
constexpr std::string_view simulated_fixing_field = "value";

/**
 * @brief The fixing @p item names, in the codec's own spelling, or why it names none.
 *
 * The codec writes an address in one spelling, so an item that reads back in another spelling
 * is refused: two ids would otherwise name one fixing.
 */
std::expected<std::string, notation_error> canonical_fixing(const std::string& item) {
    using detail::fail;
    const auto index = datum::oresmd_uri_codec::read_index(item);
    if (!index)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' names no fixing: " + index.error());
    const auto written = datum::oresmd_uri_codec::write_index(*index);
    if (!written)
        return fail(notation_error::kind::not_representable,
                    "'" + item + "' cannot be written back: " + written.error());
    if (*written != item)
        return fail(notation_error::kind::malformed,
                    "'" + item + "' is not written as the codec writes it, '" + *written + "'.");
    return *written;
}

}

std::string_view simulated_fixing_notation::provider() const {
    return simulated_fixings_provider;
}

std::string_view simulated_fixing_notation::id_scheme() const {
    return simulated_fixing_scheme;
}

price_side simulated_fixing_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
simulated_fixing_notation::read(const messaging::vendor_id& id) const {
    using detail::fail;
    if (id.id_scheme != simulated_fixing_scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "The scheme '" + id.id_scheme + "' is not '" +
                        std::string(simulated_fixing_scheme) + "'.");
    if (id.field != simulated_fixing_field)
        return fail(notation_error::kind::wrong_field,
                    "The field '" + id.field + "' is not '" + std::string(simulated_fixing_field) +
                        "'.");
    const auto item = canonical_fixing(id.external_id);
    if (!item)
        return std::unexpected(item.error());
    return quote_description{index_fixing{.item = *item}};
}

std::expected<messaging::vendor_id, notation_error>
simulated_fixing_notation::write(const quote_description& description) const {
    using detail::fail;
    const auto* fixing = std::get_if<index_fixing>(&description);
    if (fixing == nullptr)
        return fail(notation_error::kind::not_representable,
                    "The simulated fixing publisher writes only fixings.");
    const auto item = canonical_fixing(fixing->item);
    if (!item)
        return std::unexpected(item.error());
    return messaging::vendor_id{.id_scheme = std::string(simulated_fixing_scheme),
                                .external_id = *item,
                                .field = std::string(simulated_fixing_field)};
}

}
