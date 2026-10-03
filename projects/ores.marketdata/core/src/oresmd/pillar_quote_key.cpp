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
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include "ores.marketdata.core/datum/ore_key_codec.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include <format>
#include <stdexcept>

namespace ores::marketdata::core {

pillar_quote_key make_pillar_quote_key(const std::string& ccy,
                                       const std::string& start_tenor_code,
                                       std::chrono::year_month_day start_date,
                                       std::chrono::year_month_day end_date) {
    pillar_quote_key key;
    key.series_type = "IR_SWAP";
    key.metric = "RATE";
    // ORE reads a swap's start and end as two periods or two dates, never one of
    // each: a spot pillar is 0D to the days its end date lies after spot, and a
    // dated pillar is its start date to its end date.
    if (start_tenor_code == "SPOT") {
        key.qualifier = ccy + "/0D/1D";
        const auto days = std::chrono::sys_days{end_date} - std::chrono::sys_days{start_date};
        key.point = std::format("{}D", days.count());
    } else {
        key.qualifier = ccy + "/" + std::format("{:%Y%m%d}", start_date) + "/1D";
        key.point = std::format("{:%Y%m%d}", end_date);
    }
    return key;
}

namespace {

// The datum the pillar's key names. Both projections below start here, so a key
// the codec refuses fails once, in one place.
datum::market_datum pillar_datum(const pillar_quote_key& key) {
    const auto text = key.series_type + "/" + key.metric + "/" + key.qualifier + "/" + key.point;
    auto d = datum::ore_key_codec::read(text);
    if (!d)
        throw std::invalid_argument("pillar key: " + d.error());
    return std::move(*d);
}

std::string uri_of(const datum::market_datum& d) {
    auto uri = datum::oresmd_uri_codec::write(d);
    if (!uri)
        throw std::invalid_argument("pillar key: " + uri.error());
    return std::move(*uri);
}

}

std::string pillar_series_uri(const pillar_quote_key& key) {
    return uri_of(datum::series_of(pillar_datum(key)));
}

std::string pillar_datum_uri(const pillar_quote_key& key) {
    return uri_of(pillar_datum(key));
}

}
