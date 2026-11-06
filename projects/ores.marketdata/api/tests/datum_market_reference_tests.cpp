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
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/datum/market_reference.hpp"
#include "ores.marketdata.api/datum/oresmd_uri_codec.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::logging;
using ores::marketdata::datum::index_family;
using ores::marketdata::datum::market_reference;
using ores::marketdata::datum::oresmd_uri_codec;

constexpr auto euribor_6m = "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M";

}

TEST_CASE("a fixing address is a reference to an index", "[marketdata][datum][reference]") {
    auto lg(make_logger(test_suite));
    const auto reference = market_reference::parse(euribor_6m);

    REQUIRE(reference);
    REQUIRE(reference->index() != nullptr);
    CHECK(reference->datum() == nullptr);
    CHECK(reference->index()->family() == index_family::ibor);
    CHECK(reference->index()->subject() == "EUR");
    CHECK(reference->uri() == euribor_6m);
}

TEST_CASE("a series address is a reference to a datum", "[marketdata][datum][reference]") {
    auto lg(make_logger(test_suite));
    const auto index = market_reference::parse(euribor_6m);
    REQUIRE(index);

    /*
     * The series is whatever the codec writes for a datum it reads back, so the
     * case takes one the grammar admits rather than spelling a curve by hand.
     */
    const auto series =
        market_reference::parse("oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD");
    if (!series)
        FAIL("the codec refused the address: " << series.error());
    REQUIRE(series->datum() != nullptr);
    CHECK(series->index() == nullptr);
    CHECK(market_reference::parse(series->uri()) == *series);
    CHECK(!(*series == *index));
}

TEST_CASE("an address the grammar does not admit is refused with the codec's reason",
          "[marketdata][datum][reference]") {
    auto lg(make_logger(test_suite));
    for (const auto* uri : {"", "EUR-EURIBOR-6M", "oresmd://ir/EUR?type=fixing&index=ibor",
                            "oresmd://ir/EUR?type=series&instrument=nonsense"}) {
        const auto reference = market_reference::parse(uri);
        INFO("address: " << uri);
        REQUIRE(!reference);
        CHECK(!reference.error().empty());
    }
    const auto codec = oresmd_uri_codec::read_index("oresmd://ir/EUR?type=fixing&index=ibor");
    REQUIRE(!codec);
    CHECK(market_reference::parse("oresmd://ir/EUR?type=fixing&index=ibor").error() ==
          codec.error());
}

TEST_CASE("a reference made from an index is the reference its address parses to",
          "[marketdata][datum][reference]") {
    auto lg(make_logger(test_suite));
    const auto parsed = market_reference::parse(euribor_6m);
    REQUIRE(parsed);

    const auto made = market_reference::of(*parsed->index());

    REQUIRE(made);
    CHECK(*made == *parsed);
}
