/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <set>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.risk/domain/currency.hpp" // IWYU pragma: keep.
#include "ores.risk/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.risk/generators/currency_generator.hpp"

namespace {

const std::string_view test_suite("ores.risk.tests");
const std::string tags("[generators]");

}

using namespace ores::risk::generators;
using namespace ores::utility::log;

TEST_CASE("generate_single_currency", tags) {
    auto lg(make_logger(test_suite));

    auto currency = generate_synthetic_currency();
    BOOST_LOG_SEV(lg, debug) << "Generated currency: " << currency;

    CHECK(!currency.iso_code.empty());
    CHECK(!currency.name.empty());
    CHECK(!currency.symbol.empty());
    CHECK(!currency.recorded_by.empty());
    CHECK(currency.recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_synthetic_currencies(3);
    BOOST_LOG_SEV(lg, debug) << "Generated currencies: " << currencies;

    CHECK(currencies.size() == 3);
}

TEST_CASE("generate_unique_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_unique_synthetic_currencies(3);
    BOOST_LOG_SEV(lg, debug) << "Generated unique currencies: " << currencies;

    CHECK(currencies.size() == 3);

    // Verify all iso_codes are unique
    std::set<std::string> codes;
    for (const auto& c : currencies)
        codes.insert(c.iso_code);

    CHECK(codes.size() == 3);
}

TEST_CASE("generate_unicode_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_synthetic_unicode_currencies();
    BOOST_LOG_SEV(lg, debug) << "Generated unicode currencies: " << currencies;

    CHECK(currencies.size() == 7);

    // Check for known currencies
    std::set<std::string> codes;
    for (const auto& c : currencies)
        codes.insert(c.iso_code);

    CHECK(codes.count("USD") == 1);
    CHECK(codes.count("EUR") == 1);
    CHECK(codes.count("GBP") == 1);
    CHECK(codes.count("JPY") == 1);
    CHECK(codes.count("BTC") == 1);
}

