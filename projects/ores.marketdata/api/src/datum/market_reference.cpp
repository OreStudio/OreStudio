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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include <boost/url.hpp>
#include <utility>

namespace ores::marketdata::datum {

namespace {

/*
 * Whether the address names a fixing. The codec's two readers each refuse the
 * other's kind, so the type decides which one judges the address and whose
 * reason a refusal carries. Text that is not a URI at all goes to the datum
 * reader, whose refusal then gives the reason.
 */
bool names_a_fixing(std::string_view uri) {
    const auto parsed = boost::urls::parse_uri(uri);
    if (!parsed)
        return false;
    const auto type = parsed->params().find("type");
    return type != parsed->params().end() && (*type).has_value && (*type).value == "fixing";
}

}

market_reference::market_reference(std::variant<market_index, market_datum> address,
                                   std::string uri)
    : address_(std::move(address))
    , uri_(std::move(uri)) {}

std::expected<market_reference, std::string> market_reference::parse(std::string_view uri) {
    if (names_a_fixing(uri)) {
        auto index = oresmd_uri_codec::read_index(uri);
        if (!index)
            return std::unexpected(std::move(index.error()));
        return of(std::move(*index));
    }
    auto datum = oresmd_uri_codec::read(uri);
    if (!datum)
        return std::unexpected(std::move(datum.error()));
    return of(std::move(*datum));
}

std::expected<market_reference, std::string> market_reference::of(market_index index) {
    auto uri = oresmd_uri_codec::write_index(index);
    if (!uri)
        return std::unexpected(std::move(uri.error()));
    return market_reference(std::move(index), std::move(*uri));
}

std::expected<market_reference, std::string> market_reference::of(market_datum datum) {
    auto uri = oresmd_uri_codec::write(datum);
    if (!uri)
        return std::unexpected(std::move(uri.error()));
    return market_reference(std::move(datum), std::move(*uri));
}

}
