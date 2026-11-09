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
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/ore_key_codec.hpp"
#include "ores.marketdata.api/projection/ore_projection.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using namespace ores::logging;
using ores::marketdata::datum::market_reference;
using ores::marketdata::datum::ore_key_codec;
using ores::marketdata::projection::ore_projection;
using ores::marketdata::projection::ore_projection_error;
using ores::marketdata::projection::projection_refusal_kind;

market_reference parsed(const std::string& uri) {
    auto reference = market_reference::parse(uri);
    if (!reference)
        FAIL("the codec refused " << uri << ": " << reference.error());
    return *reference;
}

/// A curve series, taken from the series of a quote ORE can key.
market_reference curve_series() {
    auto quote = ore_key_codec::read("ZERO/RATE/EUR/EUR1D/A365/5Y");
    REQUIRE(quote);
    auto series = market_reference::of(ores::marketdata::datum::series_of(*quote));
    if (!series)
        FAIL("the series has no address: " << series.error());
    return *series;
}

/// An FX volatility surface: an oresmd datum ORE has no market datum for.
market_reference fx_vol_surface() {
    namespace d = ores::marketdata::datum;
    const auto expiry = d::term::parse("1Y");
    REQUIRE(expiry);
    const auto strike = d::strike::parse("0.1");
    REQUIRE(strike);
    auto datum = d::market_datum::make(d::instrument_type::fx_vol_surface,
                                       d::quote_type::price,
                                       {{d::field::unit_ccy, std::string("EUR")},
                                        {d::field::ccy, std::string("USD")},
                                        {d::field::surface_id, std::string("SURF")},
                                        {d::field::expiry, *expiry},
                                        {d::field::strike, *strike}});
    REQUIRE(datum);
    auto reference = market_reference::of(*datum);
    if (!reference)
        FAIL("the surface has no address: " << reference.error());
    return *reference;
}

}

TEST_CASE("an index is written by its ORE index name", "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const ore_projection projection;

    const auto name = projection.ore_name_of(
        parsed("oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M"));

    REQUIRE(name);
    CHECK(*name == "EUR-EURIBOR-6M");
}

TEST_CASE("a quote is written by its ORE market data key", "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const ore_projection projection;
    const auto quote = ore_key_codec::read("ZERO/RATE/EUR/EUR1D/A365/5Y");
    REQUIRE(quote);
    const auto reference = market_reference::of(*quote);
    REQUIRE(reference);

    const auto name = projection.ore_name_of(*reference);

    REQUIRE(name);
    CHECK(*name == "ZERO/RATE/EUR/EUR1D/A365/5Y");
}

TEST_CASE("a curve is written by the name the market configuration gives it",
          "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const auto curve = curve_series();
    const ore_projection named({{curve.uri(), "EUR-ESTER"}});
    const ore_projection unnamed;

    const auto name = named.ore_name_of(curve);
    REQUIRE(name);
    CHECK(*name == "EUR-ESTER");

    const auto missing = unnamed.ore_name_of(curve);
    REQUIRE(!missing);
    CHECK(missing.error().kind == projection_refusal_kind::configuration_names_no_curve);
    CHECK(missing.error().reference == curve.uri());
}

TEST_CASE("a curve the run does not name is written as the trade holds it",
          "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const auto curve = curve_series();
    const ore_projection other({{curve.uri() + "x", "EUR-ESTER"}});

    CHECK(other.ore_name_or_stored(curve.uri()) == curve.uri());
}

TEST_CASE("a stored column is projected when it is an identifier and kept otherwise",
          "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const auto curve = curve_series();
    const ore_projection projection({{curve.uri(), "EUR-ESTER"}});

    CHECK(projection.ore_name_or_stored(curve.uri()) == "EUR-ESTER");
    CHECK(projection.ore_name_or_stored(
              "oresmd://ir/EUR?type=fixing&index=ibor&name=EURIBOR&tenor=6M") == "EUR-EURIBOR-6M");
    CHECK(projection.ore_name_or_stored("EUR-EURIBOR-6M") == "EUR-EURIBOR-6M");
    CHECK(ore_projection{}.ore_name_or_stored(curve.uri()) == curve.uri());
}

TEST_CASE("a stored address the grammar refuses is thrown, not written",
          "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const ore_projection projection;
    const std::string reordered = "oresmd://ir/EUR?index=ibor&name=EURIBOR&type=fixing&tenor=6M";

    try {
        const auto written = projection.ore_name_or_stored(reordered);
        FAIL("the projection wrote the refused address as " << written);
    } catch (const ore_projection_error& e) {
        CHECK(e.kind() == projection_refusal_kind::address_the_grammar_refuses);
        CHECK(e.reference() == reordered);
        CHECK(std::string(e.what()).find("cannot write") != std::string::npos);
    }
}

TEST_CASE("a datum ORE has no name for is thrown, and the reason is its own",
          "[marketdata][projection]") {
    auto lg(make_logger(test_suite));
    const ore_projection projection;
    const auto surface = fx_vol_surface();

    const auto name = projection.ore_name_of(surface);
    REQUIRE(!name);
    CHECK(name.error().kind == projection_refusal_kind::ore_has_no_name);
    CHECK(name.error().reference == surface.uri());
    CHECK(name.error().reason.find("ORE has no market datum") != std::string::npos);

    try {
        const auto written = projection.ore_name_or_stored(surface.uri());
        FAIL("the projection wrote the datum as " << written);
    } catch (const ore_projection_error& e) {
        CHECK(e.kind() == projection_refusal_kind::ore_has_no_name);
        CHECK(e.reference() == surface.uri());
    }
}

TEST_CASE("an_ore_index_name_is_stored_as_its_address_and_written_back", "[projection]") {
    const auto address = ore_projection::address_of_index_or_stored("EUR-EURIBOR-6M");
    CHECK(address.starts_with("oresmd://"));
    CHECK(ore_projection{}.ore_name_or_stored(address) == "EUR-EURIBOR-6M");
}

TEST_CASE("a_name_the_address_could_not_write_back_stays_as_it_stands", "[projection]") {
    CHECK(ore_projection::address_of_index_or_stored("UK RPI") == "UK RPI");
    CHECK(ore_projection::address_of_index_or_stored("EQ-") == "EQ-");
    CHECK(ore_projection::address_of_index_or_stored("").empty());
    CHECK(ore_projection{}.ore_name_or_stored("").empty());
}
