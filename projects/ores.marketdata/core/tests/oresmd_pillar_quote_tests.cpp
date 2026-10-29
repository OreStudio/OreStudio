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
/**
 * @file oresmd_pillar_quote_tests.cpp
 * @brief What a curve pillar's quote is: its term, the datum it builds, and the
 * series that datum belongs to.
 *
 * The expectations are the URI text a reader compares against, written out, so a
 * change to the identity of a pillar's quote is a change to this file and not a
 * silent one. No case here reads a database, NATS or a feed.
 */
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/oresmd/pillar_quote.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

namespace {

using ores::marketdata::core::pillar_quote;
using ores::marketdata::core::pillar_quote_datum;
using ores::marketdata::core::pillar_term_text;
using ores::marketdata::datum::market_datum;
using ores::marketdata::datum::oresmd_uri_codec;

const auto first_day = std::chrono::year{2026} / 1 / 1;
const auto later_day = std::chrono::year{2026} / 3 / 2;

/// The quote of a USD SOFR pillar: overnight index, 1D, spot-starting unless
/// @p start says otherwise.
pillar_quote sofr(const std::string& start, const std::string& end) {
    return {"USD", "1D", start, end, first_day, later_day};
}

market_datum datum_of(const pillar_quote& quote) {
    const auto made = pillar_quote_datum(quote);
    REQUIRE(made.has_value());
    return *made;
}

std::string uri_of(const market_datum& d) {
    const auto uri = oresmd_uri_codec::write(d);
    REQUIRE(uri.has_value());
    return *uri;
}

std::string series_uri_of(const market_datum& d) {
    return uri_of(ores::marketdata::datum::series_of(d));
}

}

TEST_CASE("a_pillars_quote_is_a_swap_rate_quoted_at_its_own_term", "[pillar_quote]") {
    const auto d = datum_of(sofr("SPOT", "3M"));

    CHECK(uri_of(d) ==
          "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"
          "&term=3M");
    CHECK(series_uri_of(d) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D");
}

TEST_CASE("the_term_of_a_pillar_that_names_a_period_is_that_period", "[pillar_quote]") {
    // A spot-starting pillar and one that starts at a period both take the tenor
    // their template names, so the quote is the same on every day it is read.
    CHECK(pillar_term_text("SPOT", "1M", first_day, later_day) == "1M");
    CHECK(pillar_term_text("3M", "6M", first_day, later_day) == "6M");
}

TEST_CASE("the_term_of_a_meeting_dated_pillar_is_the_date_its_end_resolves_to",
          "[pillar_quote]") {
    // A meeting code names no period, so the term is the date the conventions
    // resolved the pillar's end to.
    const auto meeting = pillar_quote{"USD", "1D", "1F", "2F", first_day, later_day};
    CHECK(pillar_term_text("1F", "2F", first_day, later_day) == "20260302");
    CHECK(uri_of(datum_of(meeting)) ==
          "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=20260101&tenor=1D"
          "&term=20260302");
}

TEST_CASE("a_spot_pillar_whose_template_names_no_end_counts_the_days_it_spans",
          "[pillar_quote]") {
    const auto end =
        std::chrono::year_month_day{std::chrono::sys_days{first_day} + std::chrono::days{50}};
    CHECK(pillar_term_text("SPOT", "", first_day, end) == "50D");
    CHECK_THROWS_AS(pillar_term_text("SPOT", "", later_day, first_day), std::invalid_argument);
}

TEST_CASE("the_index_tenor_separates_one_ladders_quotes_from_anothers", "[pillar_quote]") {
    // The same currency, the same pillar and two index tenors are two series: a
    // EURIBOR 6M curve and a EURIBOR 3M curve read different quotes.
    auto six = sofr("SPOT", "1Y");
    six.currency_code = "EUR";
    six.index_tenor = "6M";
    auto three = six;
    three.index_tenor = "3M";

    CHECK(uri_of(datum_of(six)) ==
          "oresmd://ir/EUR?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M"
          "&term=1Y");
    CHECK(series_uri_of(datum_of(six)) != series_uri_of(datum_of(three)));
}

TEST_CASE("a_pillar_the_schema_refuses_is_refused_where_the_datum_is_built", "[pillar_quote]") {
    // An index tenor that is not a period names no index, and the schema says so.
    auto bad = sofr("SPOT", "1Y");
    bad.index_tenor = "FOMC";
    CHECK_FALSE(pillar_quote_datum(bad).has_value());
}
