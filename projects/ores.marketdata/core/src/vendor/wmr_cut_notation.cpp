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
#include "ores.marketdata.core/vendor/wmr_cut_notation.hpp"
#include "notation_support.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include <utility>
#include <vector>

namespace ores::marketdata::vendor {

namespace {

namespace datum = ores::marketdata::datum;

constexpr std::string_view wmr_provider = "wmr";
constexpr std::string_view wmr_scheme = "wmr_cut";
constexpr std::string_view wmr_field = "value";

/// The fx fixing @p item names, in the codec's own spelling, or why it names none.
std::expected<datum::market_index, notation_error> fixing_of(const std::string& item) {
    using detail::fail;
    const auto index = datum::oresmd_uri_codec::read_index(item);
    if (!index)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' names no fixing: " + index.error());
    if (index->family() != datum::index_family::fx)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' is not an fx fixing: only the fx family names a cut.");
    const auto written = datum::oresmd_uri_codec::write_index(*index);
    if (!written)
        return fail(notation_error::kind::not_representable,
                    "'" + item + "' cannot be written back: " + written.error());
    if (*written != item)
        return fail(notation_error::kind::malformed,
                    "'" + item + "' is not written as the codec writes it, '" + *written + "'.");
    return *index;
}

/// The item a cut, a base currency and a quote currency name.
std::expected<std::string, notation_error>
cut_item_of(const std::string& cut, const std::string& base, const std::string& quote) {
    using detail::fail;
    const auto index = datum::market_index::make(
        datum::index_family::fx,
        base,
        std::vector<datum::market_index::field_text>{{"source", cut}, {"ccy", quote}});
    if (!index)
        return fail(notation_error::kind::malformed,
                    "'" + cut + " " + base + quote + "' names no fixing: " + index.error());
    const auto item = datum::oresmd_uri_codec::write_index(*index);
    if (!item)
        return fail(notation_error::kind::not_representable,
                    "'" + cut + " " + base + quote + "' has no address: " + item.error());
    return *item;
}

}

std::string_view wmr_cut_notation::provider() const {
    return wmr_provider;
}

std::string_view wmr_cut_notation::id_scheme() const {
    return wmr_scheme;
}

price_side wmr_cut_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
wmr_cut_notation::read(const messaging::vendor_id& id) const {
    using detail::fail;
    if (id.id_scheme != wmr_scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "The scheme '" + id.id_scheme + "' is not '" + std::string(wmr_scheme) + "'.");
    if (id.field != wmr_field)
        return fail(notation_error::kind::wrong_field,
                    "The field '" + id.field + "' is not '" + std::string(wmr_field) + "'.");
    const auto space = id.external_id.find(' ');
    if (space == std::string::npos || space == 0)
        return fail(notation_error::kind::malformed,
                    "'" + id.external_id + "' names no cut and pair.");
    const std::string cut = id.external_id.substr(0, space);
    const std::string pair = id.external_id.substr(space + 1);
    if (pair.size() != 6)
        return fail(notation_error::kind::malformed,
                    "'" + pair + "' is not a currency pair: six letters are due.");
    const std::string base = pair.substr(0, 3);
    const std::string quote = pair.substr(3);
    if (!detail::is_currency_code(base) || !detail::is_currency_code(quote))
        return fail(notation_error::kind::malformed,
                    "'" + pair + "' is not a currency pair: six upper case letters are due.");
    const auto item = cut_item_of(cut, base, quote);
    if (!item)
        return std::unexpected(item.error());
    return quote_description{index_fixing{.item = *item}};
}

std::expected<messaging::vendor_id, notation_error>
wmr_cut_notation::write(const quote_description& description) const {
    using detail::fail;
    const auto* fixing = std::get_if<index_fixing>(&description);
    if (fixing == nullptr)
        return fail(notation_error::kind::not_representable,
                    "The WM/Reuters publisher writes only fixings.");
    const auto index = fixing_of(fixing->item);
    if (!index)
        return std::unexpected(index.error());
    const auto* cut = index->get("source");
    const auto* quote = index->get("ccy");
    if (cut == nullptr || quote == nullptr)
        return fail(notation_error::kind::unsupported,
                    "'" + fixing->item + "' names no cut and quote currency.");
    return messaging::vendor_id{.id_scheme = std::string(wmr_scheme),
                                .external_id = *cut + " " + index->subject() + *quote,
                                .field = std::string(wmr_field)};
}

}
