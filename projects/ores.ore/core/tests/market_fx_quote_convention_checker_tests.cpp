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
#include "ores.ore.core/market/fx_quote_convention_checker.hpp"
#include <catch2/catch_test_macros.hpp>
#include <set>

namespace {

const std::string tags("[ore][market][fx_quote_convention_checker]");

}

using ores::ore::market::fx_quote_convention_checker;
using ores::ore::market::fx_quote_status;

TEST_CASE("pair already in canonical order is unchanged", tags) {
    fx_quote_convention_checker checker({{"EUR", "USD"}, {"EUR", "GBP"}});

    const auto result = checker.check("EUR", "USD");
    REQUIRE(result.status == fx_quote_status::unchanged);
    CHECK(result.base_currency == "EUR");
    CHECK(result.quote_currency == "USD");
}

TEST_CASE("reversed pair is swapped back to canonical order", tags) {
    // The real-world case this class exists for: Legacy/Example_56's
    // market.txt stores FX/RATE/USD/GBP, but refdata's canonical pair is
    // (GBP, USD).
    fx_quote_convention_checker checker({{"GBP", "USD"}});

    const auto result = checker.check("USD", "GBP");
    REQUIRE(result.status == fx_quote_status::key_swapped);
    CHECK(result.base_currency == "GBP");
    CHECK(result.quote_currency == "USD");
}

TEST_CASE("unrecognised pair is left unchanged and reported unknown", tags) {
    fx_quote_convention_checker checker({{"EUR", "USD"}});

    const auto result = checker.check("CHF", "JPY");
    REQUIRE(result.status == fx_quote_status::unknown_pair);
    CHECK(result.base_currency == "CHF");
    CHECK(result.quote_currency == "JPY");
}

TEST_CASE("empty known-pairs set never corrects anything", tags) {
    fx_quote_convention_checker checker({});

    const auto result = checker.check("GBP", "USD");
    REQUIRE(result.status == fx_quote_status::unknown_pair);
    CHECK(result.base_currency == "GBP");
    CHECK(result.quote_currency == "USD");
}

TEST_CASE("multiple known pairs: each checked independently", tags) {
    fx_quote_convention_checker checker(
        {{"EUR", "USD"}, {"GBP", "USD"}, {"EUR", "CHF"}, {"USD", "JPY"}});

    CHECK(checker.check("EUR", "USD").status == fx_quote_status::unchanged);
    CHECK(checker.check("USD", "GBP").status == fx_quote_status::key_swapped);
    CHECK(checker.check("CHF", "EUR").status == fx_quote_status::key_swapped);
    CHECK(checker.check("USD", "JPY").status == fx_quote_status::unchanged);
    CHECK(checker.check("JPY", "USD").status == fx_quote_status::key_swapped);
}

TEST_CASE("degenerate same-currency pair does not crash", tags) {
    fx_quote_convention_checker checker({{"EUR", "USD"}});

    const auto result = checker.check("EUR", "EUR");
    REQUIRE(result.status == fx_quote_status::unknown_pair);
    CHECK(result.base_currency == "EUR");
    CHECK(result.quote_currency == "EUR");
}

TEST_CASE("currency codes are matched exactly (case-sensitive)", tags) {
    // Currency codes are always uppercase ISO in practice; the checker
    // performs no case normalisation, so a mismatched case is simply an
    // unknown pair rather than a false match.
    fx_quote_convention_checker checker({{"EUR", "USD"}});

    const auto result = checker.check("eur", "usd");
    REQUIRE(result.status == fx_quote_status::unknown_pair);
}

TEST_CASE("both orders present in known set: first-listed order wins as canonical", tags) {
    // Pathological input (both orders "known") — shouldn't happen with real
    // refdata (pair_code is 1:1 canonical), but the checker must still
    // behave deterministically rather than throwing or looping.
    fx_quote_convention_checker checker({{"EUR", "USD"}, {"USD", "EUR"}});

    const auto result = checker.check("EUR", "USD");
    CHECK(result.status == fx_quote_status::unchanged);
}
