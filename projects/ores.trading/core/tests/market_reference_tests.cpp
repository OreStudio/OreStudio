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
#include "ores.trading.core/repository/market_reference_repository.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::trading::repository;

namespace {

constexpr auto first_trade = "11111111-1111-1111-1111-111111111111";
constexpr auto second_trade = "22222222-2222-2222-2222-222222222222";
constexpr auto third_trade = "33333333-3333-3333-3333-333333333333";

}

TEST_CASE("a-trade-answers-the-addresses-its-own-rows-name", "[market_reference]") {
    const std::vector<market_reference_row> rows{
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"},
        {.trade_id = second_trade, .oresmd_uri = "oresmd://index/USD-SOFR"},
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-EURIBOR-6M"}};

    const auto answer = trade_requirements_from(rows, {first_trade, second_trade});

    REQUIRE(answer.size() == 2);
    CHECK(answer[0].trade_id == first_trade);
    // Sorted, so ESTR comes before EURIBOR: 'S' sorts before 'U'.
    CHECK(answer[0].oresmd_uris ==
          std::vector<std::string>{"oresmd://index/EUR-ESTR", "oresmd://index/EUR-EURIBOR-6M"});
    CHECK(answer[1].trade_id == second_trade);
    CHECK(answer[1].oresmd_uris == std::vector<std::string>{"oresmd://index/USD-SOFR"});
}

TEST_CASE("a-trade-that-names-nothing-answers-an-empty-list-and-not-an-absence",
          "[market_reference]") {
    // The third trade has no rows: it is present and empty, which is not the same as absent.
    const std::vector<market_reference_row> rows{
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"}};

    const auto answer = trade_requirements_from(rows, {first_trade, third_trade});

    REQUIRE(answer.size() == 2);
    CHECK(answer[1].trade_id == third_trade);
    CHECK(answer[1].oresmd_uris.empty());
}

TEST_CASE("a-trade-the-request-names-twice-answers-once-in-its-first-place", "[market_reference]") {
    const std::vector<market_reference_row> rows{
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"}};

    const auto answer = trade_requirements_from(rows, {third_trade, first_trade, third_trade});

    REQUIRE(answer.size() == 2);
    CHECK(answer[0].trade_id == third_trade);
    CHECK(answer[1].trade_id == first_trade);
}

TEST_CASE("a-trade-stated-in-another-spelling-finds-its-addresses", "[market_reference]") {
    // The store writes the identifier its own way; the caller may state any spelling of it.
    const auto upper = std::string{"11111111-1111-1111-1111-111111111111"};
    const auto braced = "{" + upper + "}";

    const std::vector<market_reference_row> rows{
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"}};

    for (const auto& spelling : {upper, braced}) {
        const auto answer = trade_requirements_from(rows, {spelling});
        REQUIRE(answer.size() == 1);
        // The answer echoes the caller's spelling and still carries the addresses.
        CHECK(answer[0].trade_id == spelling);
        CHECK(answer[0].oresmd_uris == std::vector<std::string>{"oresmd://index/EUR-ESTR"});
    }
}

TEST_CASE("one-address-on-two-trades-is-stated-on-both", "[market_reference]") {
    const std::vector<market_reference_row> rows{
        {.trade_id = first_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"},
        {.trade_id = second_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"},
        {.trade_id = second_trade, .oresmd_uri = "oresmd://index/EUR-ESTR"}};

    const auto answer = trade_requirements_from(rows, {first_trade, second_trade});

    REQUIRE(answer.size() == 2);
    CHECK(answer[0].oresmd_uris == std::vector<std::string>{"oresmd://index/EUR-ESTR"});
    // A row the function repeats is one address, not two.
    CHECK(answer[1].oresmd_uris == std::vector<std::string>{"oresmd://index/EUR-ESTR"});
}
