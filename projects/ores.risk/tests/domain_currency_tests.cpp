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
#include "ores.risk/domain/currency.hpp"

#include <array>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include <faker-cxx/faker.h> // IWYU pragma: keep.

namespace {

const std::string test_suite("ores.risk.tests.");
const std::string tags("[domain_currency_tests]");

}

using ores::risk::domain::currency;
using namespace ores::utility::log;

TEST_CASE("create_currency_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "USD";
    ccy.name = "United States Dollar";
    ccy.numeric_code = "840";
    ccy.symbol = "$";
    ccy.fraction_symbol = "¢";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Fiat";
    ccy.modified_by = "admin";
    ccy.valid_from = "2025-01-01";
    ccy.valid_to = "2099-12-31";

    BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy;

    CHECK(ccy.iso_code == "USD");
    CHECK(ccy.name == "United States Dollar");
    CHECK(ccy.numeric_code == "840");
    CHECK(ccy.symbol == "$");
    CHECK(ccy.fractions_per_unit == 100);
    CHECK(ccy.rounding_precision == 2);
}

TEST_CASE("create_currency_with_faker", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    auto fakerCcy = faker::finance::currency();

    ccy.iso_code = fakerCcy.code;
    ccy.name = fakerCcy.name;
    ccy.symbol = fakerCcy.symbol;
    ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = faker::number::integer(1, 10000);
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = faker::number::integer(0, 5);
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "";
    ccy.modified_by = std::string(faker::internet::username());
    ccy.valid_from = "";
    ccy.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy;

    CHECK(!ccy.iso_code.empty());
    CHECK(!ccy.name.empty());
    CHECK(!ccy.numeric_code.empty());
    // CHECK(!ccy.symbol.empty()); FIXME: empty sometimes
    CHECK(ccy.fractions_per_unit > 0);
    CHECK(ccy.rounding_precision >= 0);
}

TEST_CASE("create_multiple_random_currencies", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 5; ++i) {
        currency ccy;

        auto fakerCcy = faker::finance::currency();
        ccy.iso_code = fakerCcy.code;
        ccy.name = fakerCcy.name;
        ccy.symbol = fakerCcy.symbol;

        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = faker::number::integer(1, 10000);
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = faker::number::integer(0, 5);
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "";
        ccy.modified_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        ccy.valid_from = "";
        ccy.valid_to = "";

        BOOST_LOG_SEV(lg, debug) << "Currency " << i << ": " << ccy;

        CHECK(!ccy.iso_code.empty());
        CHECK(!ccy.name.empty());
        CHECK(ccy.fractions_per_unit > 0);
    }
}

TEST_CASE("create_currency_with_high_precision", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "BTC";
    ccy.name = "Bitcoin";
    ccy.numeric_code = "0";
    ccy.symbol = "₿";
    ccy.fraction_symbol = "sat";
    ccy.fractions_per_unit = 100000000;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 8;
    ccy.format = "%3% %1$.8f";
    ccy.currency_type = "Cryptocurrency";
    ccy.modified_by = "system";
    ccy.valid_from = "";
    ccy.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "High precision currency: " << ccy;

    CHECK(ccy.iso_code == "BTC");
    CHECK(ccy.fractions_per_unit == 100000000);
    CHECK(ccy.rounding_precision == 8);
}

TEST_CASE("create_currency_with_no_fractions", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "JPY";
    ccy.name = "Japanese Yen";
    ccy.numeric_code = "392";
    ccy.symbol = "¥";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 1;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 0;
    ccy.format = "%3% %1$.0f";
    ccy.currency_type = "Fiat";
    ccy.modified_by = "admin";
    ccy.valid_from = "";
    ccy.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "Currency with no fractions: " << ccy;

    CHECK(ccy.iso_code == "JPY");
    CHECK(ccy.fractions_per_unit == 1);
    CHECK(ccy.rounding_precision == 0);
}

TEST_CASE("create_currency_with_three_decimal_places", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "BHD";
    ccy.name = "Bahraini Dinar";
    ccy.numeric_code = "48";
    ccy.symbol = "BD";
    ccy.fraction_symbol = "fils";
    ccy.fractions_per_unit = 1000;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 3;
    ccy.format = "%3% %1$.3f";
    ccy.currency_type = "Fiat";
    ccy.modified_by = "admin";
    ccy.valid_from = "";
    ccy.valid_to = "";

    BOOST_LOG_SEV(lg, debug) << "Currency with three decimal places: " << ccy;

    CHECK(ccy.iso_code == "BHD");
    CHECK(ccy.fractions_per_unit == 1000);
    CHECK(ccy.rounding_precision == 3);
}

TEST_CASE("create_currencies_with_different_symbols", tags) {
    auto lg(make_logger(test_suite));

    using Currency = std::tuple<std::string, std::string, std::string>;
    const std::array<Currency, 7> currencies = {
        Currency{"USD", "$", "United States Dollar"},
        Currency{"EUR", "€", "Euro"},
        Currency{"GBP", "£", "British Pound Sterling"},
        Currency{"JPY", "¥", "Japanese Yen"},
        Currency{"INR", "₹", "Indian Rupee"},
        Currency{"BTC", "₿", "Bitcoin"},
        Currency{"RUB", "₽", "Russian Rubble"}
    };

    for (const auto& [code, symbol, name] : currencies) {
        currency ccy;
        ccy.iso_code = code;
        ccy.name = name;
        ccy.symbol = symbol;

        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = 100;
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = 2;
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "Fiat";
        ccy.modified_by = "system";
        ccy.valid_from = "";
        ccy.valid_to = "";

        BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy;

        CHECK(ccy.iso_code == code);
        CHECK(ccy.symbol == symbol);
    }
}
