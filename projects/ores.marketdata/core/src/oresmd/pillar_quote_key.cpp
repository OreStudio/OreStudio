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
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
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
    key.qualifier =
        ccy + "/" +
        (start_tenor_code == "SPOT" ? std::string("0D") : std::format("{:%Y%m%d}", start_date)) +
        "/1D";
    key.point = std::format("{:%Y%m%d}", end_date);
    return key;
}

namespace {

// The grammar's own reading of the pillar's key. Both projections below start
// here, so a key the grammar cannot name fails once, in one place.
domain::market_data_identifier pillar_identifier(const pillar_quote_key& key) {
    const std::string datum_key =
        key.series_type + "/" + key.metric + "/" + key.qualifier + "/" + key.point;
    const auto identifier = oresmd_projections::from_ore_key(datum_key);
    if (!identifier)
        throw std::invalid_argument("pillar key: the grammar names no series as '" + datum_key +
                                    "'");
    return *identifier;
}

} // namespace

std::string pillar_series_uri(const pillar_quote_key& key) {
    return oresmd_parser::to_series_uri(pillar_identifier(key)).value;
}

std::string pillar_datum_uri(const pillar_quote_key& key) {
    return oresmd_parser::to_uri(pillar_identifier(key)).value;
}

}
