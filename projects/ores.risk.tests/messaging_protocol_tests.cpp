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
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.risk/messaging/protocol.hpp"

namespace {

std::string test_suite("ores.risk.tests.");

}

using namespace ores::risk::messaging;
using ores::risk::domain::currency;

TEST_CASE("get_currencies_request_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_currencies_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << deserialized;

    // Request has no fields, so just verify it deserialized successfully
    CHECK(result.has_value());
}

TEST_CASE("get_currencies_response_empty", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;
    BOOST_LOG_SEV(lg, debug) << "Empty response: " << resp;

    CHECK(resp.currencies.empty());
}

TEST_CASE("get_currencies_response_with_single_currency", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

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
    ccy.valid_from = "";
    ccy.valid_to = "";

    resp.currencies.push_back(ccy);

    BOOST_LOG_SEV(lg, debug) << "Response with 1 currency: " << resp;

    CHECK(resp.currencies.size() == 1);
    CHECK(resp.currencies[0].iso_code == "USD");
}

TEST_CASE("get_currencies_response_serialize_deserialize", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response original;

    // Add a few currencies
    for (int i = 0; i < 3; ++i) {
        currency ccy;
        ccy.iso_code = std::string(faker::finance::currencyCode());
        ccy.name = std::string(faker::finance::currencyName());
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = std::string(faker::finance::currencySymbol());
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = faker::number::integer(1, 10000);
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = faker::number::integer(0, 5);
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "";
        ccy.modified_by = std::string(faker::internet::username());
        ccy.valid_from = "";
        ccy.valid_to = "";

        original.currencies.push_back(ccy);
    }

    BOOST_LOG_SEV(lg, debug) << "Original response with " << original.currencies.size()
                            << " currencies";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size() << " bytes";

    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                            << deserialized.currencies.size() << " currencies";

    REQUIRE(deserialized.currencies.size() == original.currencies.size());

    for (size_t i = 0; i < original.currencies.size(); ++i) {
        BOOST_LOG_SEV(lg, debug) << "Comparing currency " << i << ": "
                                << original.currencies[i].iso_code;

        CHECK(deserialized.currencies[i].iso_code == original.currencies[i].iso_code);
        CHECK(deserialized.currencies[i].name == original.currencies[i].name);
        CHECK(deserialized.currencies[i].numeric_code == original.currencies[i].numeric_code);
        CHECK(deserialized.currencies[i].symbol == original.currencies[i].symbol);
        CHECK(deserialized.currencies[i].fractions_per_unit == original.currencies[i].fractions_per_unit);
        CHECK(deserialized.currencies[i].rounding_precision == original.currencies[i].rounding_precision);
    }
}

TEST_CASE("get_currencies_response_with_multiple_currencies", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    // Add multiple standard currencies
    std::vector<std::string> iso_codes = {"USD", "EUR", "GBP", "JPY", "CHF"};

    for (const auto& iso : iso_codes) {
        currency ccy;
        ccy.iso_code = iso;
        ccy.name = std::string(faker::finance::currencyName());
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = std::string(faker::finance::currencySymbol());
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = 100;
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = 2;
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "Fiat";
        ccy.modified_by = "system";
        ccy.valid_from = "";
        ccy.valid_to = "";

        resp.currencies.push_back(ccy);
    }

    BOOST_LOG_SEV(lg, debug) << "Response with " << resp.currencies.size()
                            << " currencies";

    CHECK(resp.currencies.size() == 5);

    for (const auto& ccy : resp.currencies) {
        BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code;
        CHECK(!ccy.iso_code.empty());
    }
}

TEST_CASE("get_currencies_response_large_dataset", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response original;

    // Create a larger dataset
    const int count = 50;
    for (int i = 0; i < count; ++i) {
        currency ccy;
        ccy.iso_code = std::string(faker::finance::currencyCode());
        ccy.name = std::string(faker::finance::currencyName());
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = std::string(faker::finance::currencySymbol());
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = faker::number::integer(1, 10000);
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = faker::number::integer(0, 8);
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "";
        ccy.modified_by = std::string(faker::internet::username());
        ccy.valid_from = "";
        ccy.valid_to = "";

        original.currencies.push_back(ccy);
    }

    BOOST_LOG_SEV(lg, debug) << "Created response with " << original.currencies.size()
                            << " currencies";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size() << " bytes";

    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    BOOST_LOG_SEV(lg, debug) << "Successfully deserialized "
                            << deserialized.currencies.size() << " currencies";

    CHECK(deserialized.currencies.size() == count);
}

TEST_CASE("get_currencies_response_with_special_characters", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    // Add currencies with special Unicode symbols
    std::vector<std::pair<std::string, std::string>> currency_data = {
        {"EUR", "€"},
        {"GBP", "£"},
        {"JPY", "¥"},
        {"INR", "₹"},
        {"BTC", "₿"},
        {"RUB", "₽"}
    };

    for (const auto& [iso, symbol] : currency_data) {
        currency ccy;
        ccy.iso_code = iso;
        ccy.name = std::string(faker::finance::currencyName());
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = symbol;
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = 100;
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = 2;
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "";
        ccy.modified_by = "system";
        ccy.valid_from = "";
        ccy.valid_to = "";

        resp.currencies.push_back(ccy);
    }

    BOOST_LOG_SEV(lg, debug) << "Response with special character symbols";

    const auto serialized = resp.serialize();
    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    for (size_t i = 0; i < resp.currencies.size(); ++i) {
        BOOST_LOG_SEV(lg, debug) << "Currency: " << resp.currencies[i].iso_code
                                << " = " << resp.currencies[i].symbol;

        CHECK(deserialized.currencies[i].iso_code == resp.currencies[i].iso_code);
        CHECK(deserialized.currencies[i].symbol == resp.currencies[i].symbol);
    }
}

TEST_CASE("get_currencies_response_with_empty_fields", "[messaging_protocol_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    currency ccy;
    ccy.iso_code = "XXX";
    ccy.name = "Test Currency";
    ccy.numeric_code = "999";
    ccy.symbol = "";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 1;
    ccy.rounding_type = "";
    ccy.rounding_precision = 0;
    ccy.format = "";
    ccy.currency_type = "";
    ccy.modified_by = "";
    ccy.valid_from = "";
    ccy.valid_to = "";

    resp.currencies.push_back(ccy);

    BOOST_LOG_SEV(lg, debug) << "Response with empty optional fields";

    const auto serialized = resp.serialize();
    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized currency with empty fields";

    CHECK(deserialized.currencies[0].iso_code == "XXX");
    CHECK(deserialized.currencies[0].symbol.empty());
    CHECK(deserialized.currencies[0].currency_type.empty());
}
