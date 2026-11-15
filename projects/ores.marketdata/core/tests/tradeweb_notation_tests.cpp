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
#include "ores.marketdata.core/vendor/provider_notation.hpp"
#include "ores.marketdata.core/vendor/tradeweb_notation.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The simulated Tradeweb collection quotes five dealer composite curves: the one and six month
// EURIBOR curves, the one month GBP and JPY LIBOR curves, and the six month CHF LIBOR curve. This
// file proves the notation writes one pillar of each curve as the provider's spelling, reads it
// back as the same quote, and refuses what it cannot write.

namespace {

namespace vendor = ores::marketdata::vendor;

/// A pillar of one of the curves the Tradeweb feeds quote, and the id the notation writes for it.
struct pillar_case final {
    vendor::ir_curve_pillar pillar;
    std::string external_id;
};

}

TEST_CASE("tradeweb_notation_writes_and_reads_every_curve_its_feeds_quote",
          "[vendor_notation][tradeweb]") {
    const vendor::tradeweb_notation sut;
    CHECK(sut.provider() == "tradeweb");
    CHECK(sut.id_scheme() == "tradeweb_ticker");
    CHECK(sut.primary_side() == vendor::price_side::bid);
    CHECK(sut.names_index_tenor());

    const std::vector<pillar_case> cases{
        {{"EUR", "euribor", "DEPO", "SPOT", "1M", "1M"}, "EUREURIBOR1M.DEPO.SPOT.1M="},
        {{"EUR", "euribor", "DEPO", "SPOT", "6M", "6M"}, "EUREURIBOR6M.DEPO.SPOT.6M="},
        {{"GBP", "libor", "DEPO", "SPOT", "1M", "1M"}, "GBPLIBOR1M.DEPO.SPOT.1M="},
        {{"CHF", "libor", "DEPO", "SPOT", "6M", "6M"}, "CHFLIBOR6M.DEPO.SPOT.6M="},
        {{"JPY", "libor", "DEPO", "SPOT", "1M", "1M"}, "JPYLIBOR1M.DEPO.SPOT.1M="}};

    for (const auto& c : cases) {
        INFO(c.external_id);
        const vendor::quote_description description = c.pillar;
        const auto written = sut.write(description);
        REQUIRE(written.has_value());
        CHECK(written->id_scheme == "tradeweb_ticker");
        CHECK(written->external_id == c.external_id);
        CHECK(written->field == "BID");

        // What the notation writes it reads back as the same description.
        const auto read = sut.read(*written);
        REQUIRE(read.has_value());
        CHECK(*read == description);
    }
}

TEST_CASE("tradeweb_notation_refuses_a_malformed_id", "[vendor_notation][tradeweb]") {
    const vendor::tradeweb_notation sut;
    for (const auto& external_id : {"EUREURIBOR1M.DEPO.SPOT.1M",
                                    "EUREURIBOR1M DEPO SPOT 1M=",
                                    "EUREURIBOR1M.DEPO.1M=",
                                    "EUREURIBOR1M.DEPO.SPOT.1M==",
                                    "EUREURIBOR1M.DEPO.SPOT.SPOT=",
                                    "EUREURIBOR.DEPO.SPOT.1M=",
                                    ""}) {
        INFO(external_id);
        const auto read = sut.read({"tradeweb_ticker", external_id, "BID"});
        REQUIRE_FALSE(read.has_value());
        CHECK(read.error().reason == vendor::notation_error::kind::malformed);
    }
}

TEST_CASE("tradeweb_notation_refuses_an_id_of_another_scheme", "[vendor_notation][tradeweb]") {
    const vendor::tradeweb_notation sut;
    for (const auto& scheme : {"ric", "bbg_ticker", "ice_ticker", "demo_key"}) {
        INFO(scheme);
        const auto read = sut.read({scheme, "EUREURIBOR1M.DEPO.SPOT.1M=", "BID"});
        REQUIRE_FALSE(read.has_value());
        CHECK(read.error().reason == vendor::notation_error::kind::wrong_scheme);
    }
}

TEST_CASE("tradeweb_notation_refuses_a_field_it_does_not_price", "[vendor_notation][tradeweb]") {
    const vendor::tradeweb_notation sut;
    for (const auto& field : {"ASK", "PX_LAST", "value", ""}) {
        INFO(field);
        const auto read = sut.read({"tradeweb_ticker", "EUREURIBOR1M.DEPO.SPOT.1M=", field});
        REQUIRE_FALSE(read.has_value());
        CHECK(read.error().reason == vendor::notation_error::kind::wrong_field);
    }
}

TEST_CASE("tradeweb_notation_refuses_a_description_it_cannot_write",
          "[vendor_notation][tradeweb]") {
    const vendor::tradeweb_notation sut;

    // Tradeweb's simulated feeds quote interest rate curves only.
    const auto pair = sut.write(vendor::fx_spot_quote{"EUR", "USD", vendor::price_side::bid});
    REQUIRE_FALSE(pair.has_value());
    CHECK(pair.error().reason == vendor::notation_error::kind::not_representable);

    // A pillar must end after it starts.
    const auto empty = sut.write(vendor::ir_curve_pillar{"EUR", "euribor", "DEPO", "3M", "3M"});
    REQUIRE_FALSE(empty.has_value());
    CHECK(empty.error().reason == vendor::notation_error::kind::not_representable);

    // A Tradeweb id names the tenor of the curve's index, so a description without one is refused.
    const auto untoned = sut.write(vendor::ir_curve_pillar{"EUR", "euribor", "DEPO", "SPOT", "3M"});
    REQUIRE_FALSE(untoned.has_value());
    CHECK(untoned.error().reason == vendor::notation_error::kind::not_representable);
}
