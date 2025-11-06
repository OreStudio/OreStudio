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
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.risk/domain/currency.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.risk.tests/repository_helper.hpp"

namespace {

std::string test_suite("ores.risk.tests");

}

using ores::risk::domain::currency;
using ores::risk::repository::currency_repository;
using ores::risk::tests::repository_helper;

TEST_CASE("write_single_currency", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;
    auto ccy = helper.create_test_currency("USD");
    std::vector<currency> currencies = {ccy};
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    CHECK_NOTHROW(repo.write(helper.get_context(), currencies));
}

TEST_CASE("write_multiple_currencies", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;
    std::vector<currency> currencies;
    const std::vector<std::string> iso_codes = {"USD", "EUR", "GBP", "JPY", "CHF"};
    for (const auto& iso_code : iso_codes) {
        currencies.push_back(helper.create_test_currency(iso_code));
    }
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    CHECK_NOTHROW(repo.write(helper.get_context(), currencies));
}

TEST_CASE("read_latest_currencies", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;
    std::vector<currency> written_currencies;
    const std::vector<std::string> iso_codes = {"AUD", "CAD", "NZD"};
    for (const auto& iso_code : iso_codes) {
        written_currencies.push_back(helper.create_test_currency(iso_code));
    }
    BOOST_LOG_SEV(lg, debug) << "Written currencies: " << written_currencies;

    repo.write(helper.get_context(), written_currencies);

    auto read_currencies = repo.read_latest(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(!read_currencies.empty());
    CHECK(read_currencies.size() >= written_currencies.size());
}

TEST_CASE("read_latest_currency_by_iso_code", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    auto ccy = helper.create_test_currency("EUR");
    const auto test_iso_code = ccy.iso_code;

    std::vector<currency> currencies = {ccy};
    BOOST_LOG_SEV(lg, debug) << "Write currencies: " << currencies;

    repo.write(helper.get_context(), currencies);

    auto read_currencies = repo.read_latest(helper.get_context(), test_iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    REQUIRE(read_currencies.size() == 1);
    CHECK(read_currencies[0].iso_code == test_iso_code);
    CHECK(read_currencies[0].iso_code == "EUR");
}

TEST_CASE("read_all_currencies", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    std::vector<currency> written_currencies;
    const std::vector<std::string> iso_codes = {"BRL", "MXN", "INR"};
    for (const auto& iso_code : iso_codes) {
        written_currencies.push_back(helper.create_test_currency(iso_code));
    }
    BOOST_LOG_SEV(lg, debug) << "Written currencies: " << written_currencies;

    repo.write(helper.get_context(), written_currencies);

    auto read_currencies = repo.read_all(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(!read_currencies.empty());
    CHECK(read_currencies.size() >= written_currencies.size());
}

TEST_CASE("read_all_currencies_by_iso_code", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    // Write multiple versions of the same currency
    auto ccy1 = helper.create_test_currency("GBP");
    const auto test_iso_code = ccy1.iso_code;
    ccy1.name = "British Pound v1";
    BOOST_LOG_SEV(lg, debug) << "Currency 1: " << ccy1;

    auto ccy2 = ccy1;
    ccy2.name = "British Pound v2";
    ccy2.symbol = "£";
    BOOST_LOG_SEV(lg, debug) << "Currency 2: " << ccy2;

    repo.write(helper.get_context(), {ccy1});
    repo.write(helper.get_context(), {ccy2});

    // Read all versions
    auto read_currencies = repo.read_all(helper.get_context(), test_iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() >= 2);

    // Verify different versions exist
    bool found_v1 = false, found_v2 = false;
    for (const auto& ccy : read_currencies) {
        if (ccy.iso_code == test_iso_code && ccy.name == "British Pound v1") found_v1 = true;
        if (ccy.iso_code == test_iso_code && ccy.name == "British Pound v2") found_v2 = true;
    }

    CHECK(found_v1);
    CHECK(found_v2);
}

TEST_CASE("read_nonexistent_iso_code", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    const std::string nonexistent_iso = "XXX";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ISO code: " << nonexistent_iso;

    auto read_currencies = repo.read_latest(helper.get_context(), nonexistent_iso);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() == 0);
}

TEST_CASE("write_and_read_currency_with_unicode_symbols", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    // Create currencies with special Unicode symbols
    std::vector<std::pair<std::string, std::string>> currency_data = {
        {"EUR", "€"},
        {"GBP", "£"},
        {"JPY", "¥"},
        {"INR", "₹"},
        {"BTC", "₿"},
        {"RUB", "₽"}
    };

    std::vector<currency> currencies;
    for (const auto& [iso, symbol] : currency_data) {
        auto ccy = helper.create_test_currency(iso);
        ccy.symbol = symbol;
        currencies.push_back(ccy);
        BOOST_LOG_SEV(lg, debug) << "Currency: " << iso << " = " << symbol;
    }

    repo.write(helper.get_context(), currencies);

    // Read back and verify symbols
    auto read_currencies = repo.read_latest(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() >= currency_data.size());

    for (const auto& [iso, expected_symbol] : currency_data) {
        auto it = std::ranges::find_if(read_currencies,
            [&iso](const currency& c) { return c.iso_code == iso; });

        REQUIRE(it != read_currencies.end());
        CHECK(it->symbol == expected_symbol);
        BOOST_LOG_SEV(lg, debug) << "Verified: " << iso << " = " << it->symbol;
    }
}

TEST_CASE("write_and_read_cryptocurrency", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    // Create cryptocurrency with high precision
    currency btc;
    btc.iso_code = "BTC";
    btc.name = "Bitcoin";
    btc.numeric_code = "0";
    btc.symbol = "₿";
    btc.fraction_symbol = "sat";
    btc.fractions_per_unit = 100000000;
    btc.rounding_type = "Closest";
    btc.rounding_precision = 8;
    btc.format = "%3% %1$.8f";
    btc.currency_type = "Cryptocurrency";
    btc.modified_by = "system";
    btc.valid_from = "";
    btc.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "Cryptocurrency: " << btc;

    repo.write(helper.get_context(), {btc});

    // Read back and verify precision
    auto read_currencies = repo.read_latest(helper.get_context(), "BTC");
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    REQUIRE(read_currencies.size() == 1);
    CHECK(read_currencies[0].iso_code == "BTC");
    CHECK(read_currencies[0].fractions_per_unit == 100000000);
    CHECK(read_currencies[0].rounding_precision == 8);
    CHECK(read_currencies[0].symbol == "₿");
}

TEST_CASE("write_and_read_currency_with_no_fractions", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    // Create currency with no fractions (like Japanese Yen)
    currency jpy;
    jpy.iso_code = "JPY";
    jpy.name = "Japanese Yen";
    jpy.numeric_code = "392";
    jpy.symbol = "¥";
    jpy.fraction_symbol = "";
    jpy.fractions_per_unit = 1;
    jpy.rounding_type = "Closest";
    jpy.rounding_precision = 0;
    jpy.format = "%3% %1$.0f";
    jpy.currency_type = "Fiat";
    jpy.modified_by = "admin";
    jpy.valid_from = "";
    jpy.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "Currency with no fractions: " << jpy;

    repo.write(helper.get_context(), {jpy});

    // Read back and verify
    auto read_currencies = repo.read_latest(helper.get_context(), "JPY");
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    REQUIRE(read_currencies.size() == 1);
    CHECK(read_currencies[0].iso_code == "JPY");
    CHECK(read_currencies[0].fractions_per_unit == 1);
    CHECK(read_currencies[0].rounding_precision == 0);
}

TEST_CASE("write_currencies_with_faker", "[repository_currency_repository_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    currency_repository repo;

    // Create random currencies with faker
    std::vector<currency> currencies;
    for (int i = 0; i < 10; ++i) {
        currency ccy;
        ccy.iso_code = std::string(faker::finance::currencyCode()) + std::to_string(i);
        ccy.name = std::string(faker::finance::currencyName());
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = std::string(faker::finance::currencySymbol());
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = faker::number::integer(1, 10000);
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = faker::number::integer(0, 5);
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "Fiat";
        ccy.modified_by = std::string(faker::internet::username());
        ccy.valid_from = "";
        ccy.valid_to = "";

        currencies.push_back(ccy);
        BOOST_LOG_SEV(lg, debug) << "Currency " << i << ": " << ccy;
    }

    repo.write(helper.get_context(), currencies);

    // Read back and verify count
    auto read_currencies = repo.read_latest(helper.get_context());
    BOOST_LOG_SEV(lg, debug) << "Read " << read_currencies.size() << " currencies";

    CHECK(read_currencies.size() >= 10);
}
