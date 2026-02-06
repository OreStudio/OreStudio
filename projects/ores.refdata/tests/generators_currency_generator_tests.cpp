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
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/currency.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/currency_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[generators]");

}

using namespace ores::refdata::generators;
using namespace ores::logging;

TEST_CASE("generate_single_currency", tags) {
    auto lg(make_logger(test_suite));

    auto currency = generate_synthetic_currency(ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated currency: " << currency;

    CHECK(!currency.iso_code.empty());
    CHECK(!currency.name.empty());
    CHECK(!currency.symbol.empty());
    CHECK(!currency.recorded_by.empty());
    CHECK(currency.recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_synthetic_currencies(3, ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated currencies: " << currencies;

    CHECK(currencies.size() == 3);
}

TEST_CASE("generate_unique_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_unique_synthetic_currencies(3, ores::utility::uuid::tenant_id::system());
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

    auto currencies = generate_synthetic_unicode_currencies(ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated unicode currencies: " << currencies;

    CHECK(currencies.size() == 7);

    // Check for expected unicode symbols
    std::set<std::string> symbols;
    for (const auto& c : currencies)
        symbols.insert(c.symbol);

    CHECK(symbols.count("$") == 1);   // USD-like
    CHECK(symbols.count("€") == 1);   // EUR-like
    CHECK(symbols.count("£") == 1);   // GBP-like
    CHECK(symbols.count("¥") == 1);   // JPY-like
    CHECK(symbols.count("₿") == 1);   // BTC-like
}

TEST_CASE("generate_fictional_currencies_returns_all_when_no_count", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(0, ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated fictional currencies: " << currencies;

    CHECK(currencies.size() == 50);
}

TEST_CASE("generate_fictional_currencies_returns_requested_count", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(5, ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated 5 fictional currencies: " << currencies;

    CHECK(currencies.size() == 5);
}

TEST_CASE("generate_fictional_currencies_returns_all_when_count_exceeds_available", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(100, ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Generated fictional currencies with large count: " << currencies;

    CHECK(currencies.size() == 50);
}

TEST_CASE("generate_fictional_currencies_has_unique_iso_codes", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(0, ores::utility::uuid::tenant_id::system());

    std::set<std::string> codes;
    for (const auto& c : currencies)
        codes.insert(c.iso_code);

    CHECK(codes.size() == 50);
}

TEST_CASE("generate_fictional_currencies_has_unique_numeric_codes", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(0, ores::utility::uuid::tenant_id::system());

    std::set<std::string> codes;
    for (const auto& c : currencies)
        codes.insert(c.numeric_code);

    CHECK(codes.size() == 50);
}

TEST_CASE("generate_fictional_currencies_has_expected_first_currency", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(1, ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "First fictional currency: " << currencies;

    REQUIRE(currencies.size() == 1);
    CHECK(currencies[0].iso_code == "XAE");
    CHECK(currencies[0].name == "Aerilonian Dollar");
    CHECK(!currencies[0].recorded_by.empty());
    CHECK(currencies[0].recorded_at != std::chrono::system_clock::time_point{});
}

TEST_CASE("generate_fictional_currencies_contains_known_currencies", tags) {
    auto lg(make_logger(test_suite));

    auto currencies = generate_fictional_currencies(0, ores::utility::uuid::tenant_id::system());

    std::set<std::string> codes;
    for (const auto& c : currencies)
        codes.insert(c.iso_code);

    CHECK(codes.count("XAE") == 1);  // Aerilonian Dollar
    CHECK(codes.count("XZE") == 1);  // Zephyrian Zephyr
    CHECK(codes.count("XER") == 1);  // Eriadoran Euro
    CHECK(codes.count("XKR") == 1);  // Krynnish Krynn
}

