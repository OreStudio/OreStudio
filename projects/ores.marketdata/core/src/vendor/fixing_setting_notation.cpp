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
#include "ores.marketdata.core/vendor/fixing_setting_notation.hpp"
#include "notation_support.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include <utility>
#include <vector>

namespace ores::marketdata::vendor {

namespace {

namespace datum = ores::marketdata::datum;

constexpr std::string_view setting_scheme = "setting";
constexpr std::string_view setting_field = "value";

/// The setting @p item names, in the codec's own spelling, or why it names none.
std::expected<datum::market_index, notation_error> setting_of(const std::string& item) {
    using detail::fail;
    const auto index = datum::oresmd_uri_codec::read_index(item);
    if (!index)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' names no fixing: " + index.error());
    if (index->family() != datum::index_family::ibor)
        return fail(notation_error::kind::unsupported,
                    "'" + item + "' is not a rate setting: only an ibor family names one.");
    const auto written = datum::oresmd_uri_codec::write_index(*index);
    if (!written)
        return fail(notation_error::kind::not_representable,
                    "'" + item + "' cannot be written back: " + written.error());
    if (*written != item)
        return fail(notation_error::kind::malformed,
                    "'" + item + "' is not written as the codec writes it, '" + *written + "'.");
    return *index;
}

/// The item a currency, an index name and a tenor name.
std::expected<std::string, notation_error>
setting_item_of(const std::string& currency, const std::string& name, const std::string& tenor) {
    using detail::fail;
    std::vector<datum::market_index::field_text> fields{{"name", name}};
    if (!tenor.empty())
        fields.push_back({"tenor", tenor});
    const auto index =
        datum::market_index::make(datum::index_family::ibor, currency, std::move(fields));
    if (!index)
        return fail(notation_error::kind::malformed,
                    "'" + currency + " " + name + "' names no rate setting: " + index.error());
    const auto item = datum::oresmd_uri_codec::write_index(*index);
    if (!item)
        return fail(notation_error::kind::not_representable,
                    "'" + currency + " " + name + "' has no address: " + item.error());
    return *item;
}

}

fixing_setting_notation::fixing_setting_notation(std::string provider)
    : provider_(std::move(provider)) {}

std::string_view fixing_setting_notation::provider() const {
    return provider_;
}

std::string_view fixing_setting_notation::id_scheme() const {
    return setting_scheme;
}

price_side fixing_setting_notation::primary_side() const {
    return price_side::last;
}

std::expected<quote_description, notation_error>
fixing_setting_notation::read(const messaging::vendor_id& id) const {
    using detail::fail;
    if (id.id_scheme != setting_scheme)
        return fail(notation_error::kind::wrong_scheme,
                    "The scheme '" + id.id_scheme + "' is not '" + std::string(setting_scheme) +
                        "'.");
    if (id.field != setting_field)
        return fail(notation_error::kind::wrong_field,
                    "The field '" + id.field + "' is not '" + std::string(setting_field) + "'.");
    const auto first = id.external_id.find(' ');
    if (first == std::string::npos)
        return fail(notation_error::kind::malformed,
                    "'" + id.external_id + "' names no setting: a currency and a name are due.");
    const std::string currency = id.external_id.substr(0, first);
    std::string rest = id.external_id.substr(first + 1);
    std::string tenor;
    const auto second = rest.find(' ');
    if (second != std::string::npos) {
        tenor = rest.substr(second + 1);
        rest = rest.substr(0, second);
    }
    if (!detail::is_currency_code(currency) || rest.empty() || tenor.find(' ') != std::string::npos)
        return fail(notation_error::kind::malformed,
                    "'" + id.external_id + "' is not a currency, a name and an optional tenor.");
    const auto item = setting_item_of(currency, rest, tenor);
    if (!item)
        return std::unexpected(item.error());
    return quote_description{index_fixing{.item = *item}};
}

std::expected<messaging::vendor_id, notation_error>
fixing_setting_notation::write(const quote_description& description) const {
    using detail::fail;
    const auto* fixing = std::get_if<index_fixing>(&description);
    if (fixing == nullptr)
        return fail(notation_error::kind::not_representable,
                    "A rate publisher writes only fixings.");
    const auto index = setting_of(fixing->item);
    if (!index)
        return std::unexpected(index.error());
    std::string external = index->subject();
    if (const auto* name = index->get("name"); name != nullptr)
        external += " " + *name;
    if (const auto* tenor = index->get("tenor"); tenor != nullptr)
        external += " " + *tenor;
    return messaging::vendor_id{.id_scheme = std::string(setting_scheme),
                                .external_id = std::move(external),
                                .field = std::string(setting_field)};
}

}
