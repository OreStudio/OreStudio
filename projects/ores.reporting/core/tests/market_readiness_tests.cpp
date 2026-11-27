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
#include "ores.reporting.core/service/market_readiness.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using namespace ores::reporting::service;

address_answer answered(const std::string& uri) {
    return address_answer{.oresmd_uri = uri, .series_id = "series-" + uri, .cause = ""};
}

address_answer missing(const std::string& uri) {
    return address_answer{.oresmd_uri = uri, .series_id = std::nullopt, .cause = "no_series"};
}

address_answer no_point(const std::string& uri) {
    return address_answer{.oresmd_uri = uri, .series_id = "series-" + uri, .cause = "no_point"};
}

trade_requirement needs(const std::string& trade_id, std::vector<std::string> uris) {
    return trade_requirement{.trade_id = trade_id, .oresmd_uris = std::move(uris)};
}

}

TEST_CASE("a_trade_the_market_answers_in_full_is_complete", "[market_readiness]") {
    const auto readiness = decide_market_readiness(
        {"trade-1"}, {needs("trade-1", {"EUR-ESTR-6M"})}, {answered("EUR-ESTR-6M")});

    REQUIRE(readiness.booked == 1);
    REQUIRE(readiness.complete == 1);
    REQUIRE(readiness.excluded == 0);
    REQUIRE(readiness.survivors == std::vector<std::string>{"trade-1"});
    REQUIRE(readiness.trades.at(0).complete);
    REQUIRE(readiness.trades.at(0).gap_count == 0);
    REQUIRE(readiness.addresses.at(0).oresmd_uri == "EUR-ESTR-6M");
    REQUIRE(readiness.addresses.at(0).series_id ==
            std::optional<std::string>("series-EUR-ESTR-6M"));
    REQUIRE(readiness.addresses.at(0).gap_cause.empty());
}

TEST_CASE("a_trade_whose_series_is_absent_is_left_out", "[market_readiness]") {
    const auto readiness = decide_market_readiness(
        {"trade-1"}, {needs("trade-1", {"EUR-ESTR-6M"})}, {missing("EUR-ESTR-6M")});

    REQUIRE(readiness.complete == 0);
    REQUIRE(readiness.excluded == 1);
    REQUIRE(readiness.survivors.empty());
    REQUIRE_FALSE(readiness.has_a_valuation());
    REQUIRE(readiness.trades.at(0).gap_count == 1);
    REQUIRE(readiness.addresses.at(0).gap_cause == "no_series");
    REQUIRE_FALSE(readiness.addresses.at(0).series_id.has_value());
}

TEST_CASE("a_series_with_no_point_at_the_as_of_is_a_gap_of_its_own", "[market_readiness]") {
    const auto readiness = decide_market_readiness(
        {"trade-1"}, {needs("trade-1", {"EUR-ESTR-6M"})}, {no_point("EUR-ESTR-6M")});

    REQUIRE(readiness.excluded == 1);
    REQUIRE(readiness.addresses.at(0).gap_cause == "no_point");
    REQUIRE(readiness.addresses.at(0).series_id ==
            std::optional<std::string>("series-EUR-ESTR-6M"));
}

TEST_CASE("a_trade_that_names_no_address_is_complete", "[market_readiness]") {
    const auto readiness = decide_market_readiness({"trade-1"}, {needs("trade-1", {})}, {});

    REQUIRE(readiness.complete == 1);
    REQUIRE(readiness.excluded == 0);
    REQUIRE(readiness.survivors == std::vector<std::string>{"trade-1"});
    REQUIRE(readiness.addresses.empty());
}

TEST_CASE("an_incomplete_trade_keeps_the_addresses_its_book_needs", "[market_readiness]") {
    const auto readiness = decide_market_readiness(
        {"trade-1", "trade-2"},
        {needs("trade-1", {"EUR-ESTR-6M", "USD-SOFR-3M"}), needs("trade-2", {"EUR-ESTR-6M"})},
        {answered("EUR-ESTR-6M"), missing("USD-SOFR-3M")});

    REQUIRE(readiness.booked == 2);
    REQUIRE(readiness.complete == 1);
    REQUIRE(readiness.excluded == 1);
    REQUIRE(readiness.survivors == std::vector<std::string>{"trade-2"});
    REQUIRE(readiness.trades.at(0).trade_id == "trade-1");
    REQUIRE(readiness.trades.at(0).gap_count == 1);
    REQUIRE(readiness.trades.at(1).complete);

    // The address two trades name is one row, stated once.
    REQUIRE(readiness.addresses.size() == 2);
    REQUIRE(readiness.addresses.at(0).oresmd_uri == "EUR-ESTR-6M");
    REQUIRE(readiness.addresses.at(1).oresmd_uri == "USD-SOFR-3M");
    REQUIRE(readiness.addresses.at(1).gap_cause == "no_series");
}

TEST_CASE("a_trade_named_twice_is_counted_once", "[market_readiness]") {
    const auto readiness = decide_market_readiness(
        {"trade-1", "trade-1"}, {needs("trade-1", {"EUR-ESTR-6M"})}, {answered("EUR-ESTR-6M")});

    REQUIRE(readiness.booked == 1);
    REQUIRE(readiness.complete == 1);
    REQUIRE(readiness.survivors.size() == 1);
}

TEST_CASE("a_trade_the_requirements_do_not_cover_is_refused", "[market_readiness]") {
    REQUIRE_THROWS_AS(decide_market_readiness({"trade-1"}, {}, {answered("EUR-ESTR-6M")}),
                      std::invalid_argument);
}

TEST_CASE("an_address_the_market_read_does_not_answer_is_refused", "[market_readiness]") {
    REQUIRE_THROWS_AS(decide_market_readiness({"trade-1"}, {needs("trade-1", {"EUR-ESTR-6M"})}, {}),
                      std::invalid_argument);
}
