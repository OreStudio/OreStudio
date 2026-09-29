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
#include "ores.marketdata.core/oresmd/oresmd_parser.hpp"
#include "ores.marketdata.core/oresmd/oresmd_projections.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

/**
 * @file oresmd_identity_round_trip_tests.cpp
 * @brief Pins what a binding's identity goes back to.
 *
 * The feed binding names its series by the identity, and the ingest loop projects
 * that identity back to an ORE key for the subject it republishes under and for
 * the registry's decomposition. The subject a consumer subscribes to changes
 * silently if a key does not survive the trip, so the first case walks the FX key
 * a bound feed publishes and asserts the identity is the same on both sides.
 *
 * The second case pins the limit rather than leaving it to be discovered: the
 * projection back to a key exists only where the series is itself a quote key. An
 * instrument whose maturity is its point -- money market, FRA, and the swap whose
 * fixed-frequency segment is the point -- has a series key the quote grammar does
 * not accept, so a binding for one projects to no key and the loop reports it and
 * drops its ticks. That matches the bound path, which decodes the FX spot tick and
 * nothing else.
 */

namespace {

using ores::marketdata::core::oresmd_parser;
using ores::marketdata::core::oresmd_projections;
using ores::marketdata::domain::oresmd_uri;

std::string identity_of(const std::string& key) {
    const auto identifier = oresmd_projections::from_ore_key(key);
    REQUIRE(identifier);
    return oresmd_parser::to_series_uri(*identifier).value;
}

}

TEST_CASE("the FX key a bound feed publishes survives its identity and back",
          "[oresmd][identity]") {
    for (const auto& key : {"FX/RATE/EUR/USD", "FX/RATE/GBP/JPY"}) {
        INFO("key: " << key);
        const auto uri = identity_of(key);

        // What the loop holds: the identity back to a key.
        const auto series_key =
            oresmd_projections::to_quote_key(oresmd_parser::parse(oresmd_uri{uri}));
        REQUIRE(series_key);
        CHECK(*series_key == key);

        // And that key's identity, which must be the one the binding carried.
        CHECK(identity_of(*series_key) == uri);
    }
}

TEST_CASE("a family whose series key is not a quote key projects back to nothing",
          "[oresmd][identity]") {
    // The money market maturity is the point and the swap's fixed-frequency
    // segment is the datum's, so neither series has a key the quote grammar
    // accepts.
    for (const auto& key : {"MM/RATE/USD/2D/3M", "IR_SWAP/RATE/USD/2D/1D/5Y"}) {
        INFO("key: " << key);
        const auto uri = identity_of(key);

        const auto series_key =
            oresmd_projections::to_quote_key(oresmd_parser::parse(oresmd_uri{uri}));
        CHECK_FALSE(series_key.has_value());
    }
}
