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
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.risk/messaging/risk_message_handler.hpp"
#include "ores.risk/messaging/protocol.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.risk.tests/repository_helper.hpp"

namespace {

std::string test_suite("ores.risk.tests");

}

using namespace ores;
using namespace ores::risk::messaging;
using namespace ores::risk::tests;

TEST_CASE("handle_get_currencies_request_empty", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        CHECK(response.currencies.empty());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_single_currency", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    // Create a test currency
    auto ccy = helper.create_test_currency("USD");
    risk::repository::currency_repository repo;
    repo.write(helper.get_context(), {ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << ccy;

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        CHECK(response.currencies.size() == 1);
        CHECK(response.currencies[0].iso_code == "USD");
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_multiple_currencies", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    // Create multiple test currencies
    const std::vector<std::string> iso_codes = {"USD", "EUR", "GBP", "JPY", "CHF"};
    risk::repository::currency_repository repo;

    std::vector<risk::domain::currency> currencies;
    for (const auto& iso_code : iso_codes) {
        auto ccy = helper.create_test_currency(iso_code);
        currencies.push_back(ccy);
        BOOST_LOG_SEV(lg, debug) << "Created test currency: " << ccy;
    }
    repo.write(helper.get_context(), currencies);

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response with " << response.currencies.size()
                                << " currencies";

        CHECK(response.currencies.size() == iso_codes.size());

        for (const auto& ccy : response.currencies) {
            BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code;
            CHECK(!ccy.iso_code.empty());
        }

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_faker", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    // Create random currencies
    const int currency_count = 10;
    risk::repository::currency_repository repo;

    std::vector<risk::domain::currency> currencies;
    for (int i = 0; i < currency_count; ++i) {
        risk::domain::currency ccy;
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
        BOOST_LOG_SEV(lg, debug) << "Created test currency " << i << ": " << ccy;
    }
    repo.write(helper.get_context(), currencies);

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response with " << response.currencies.size()
                                << " currencies";

        CHECK(response.currencies.size() == currency_count);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_verify_serialization_roundtrip", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    // Create a currency with specific values
    risk::domain::currency original_ccy;
    original_ccy.iso_code = "BTC";
    original_ccy.name = "Bitcoin";
    original_ccy.numeric_code = "0";
    original_ccy.symbol = "₿";
    original_ccy.fraction_symbol = "sat";
    original_ccy.fractions_per_unit = 100000000;
    original_ccy.rounding_type = "Closest";
    original_ccy.rounding_precision = 8;
    original_ccy.format = "%3% %1$.8f";
    original_ccy.currency_type = "Cryptocurrency";
    original_ccy.modified_by = "system";
    original_ccy.valid_from = "";
    original_ccy.valid_to = "";

    risk::repository::currency_repository repo;
    repo.write(helper.get_context(), {original_ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << original_ccy;

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        REQUIRE(response.currencies.size() == 1);
        const auto& retrieved_ccy = response.currencies[0];

        CHECK(retrieved_ccy.iso_code == original_ccy.iso_code);
        CHECK(retrieved_ccy.name == original_ccy.name);
        CHECK(retrieved_ccy.numeric_code == original_ccy.numeric_code);
        CHECK(retrieved_ccy.symbol == original_ccy.symbol);
        CHECK(retrieved_ccy.fraction_symbol == original_ccy.fraction_symbol);
        CHECK(retrieved_ccy.fractions_per_unit == original_ccy.fractions_per_unit);
        CHECK(retrieved_ccy.rounding_type == original_ccy.rounding_type);
        CHECK(retrieved_ccy.rounding_precision == original_ccy.rounding_precision);
        CHECK(retrieved_ccy.format == original_ccy.format);
        CHECK(retrieved_ccy.currency_type == original_ccy.currency_type);
        // Note: modified_by, valid_from, and valid_to are set by the repository
        CHECK(!retrieved_ccy.modified_by.empty());
        CHECK(!retrieved_ccy.valid_from.empty());
        CHECK(!retrieved_ccy.valid_to.empty());

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_unicode_symbols", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    helper.cleanup_database();

    // Create currencies with special Unicode symbols
    std::vector<std::pair<std::string, std::string>> currency_data = {
        {"EUR", "€"},
        {"GBP", "£"},
        {"JPY", "¥"},
        {"INR", "₹"},
        {"BTC", "₿"},
        {"RUB", "₽"}
    };

    risk::repository::currency_repository repo;
    std::vector<risk::domain::currency> currencies;
    for (const auto& [iso, symbol] : currency_data) {
        auto ccy = helper.create_test_currency(iso);
        ccy.symbol = symbol;
        currencies.push_back(ccy);
        BOOST_LOG_SEV(lg, debug) << "Created currency: " << iso << " = " << symbol;
    }
    repo.write(helper.get_context(), currencies);

    risk_message_handler handler(helper.get_context());

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::protocol::message_type::get_currencies_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();

        CHECK(response.currencies.size() == currency_data.size());

        for (const auto& ccy : response.currencies) {
            BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code << " = " << ccy.symbol;
            CHECK(!ccy.symbol.empty());
        }

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_invalid_message_type", "[messaging_risk_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    repository_helper helper;
    risk_message_handler handler(helper.get_context());

    std::vector<std::uint8_t> empty_payload;

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<comms::protocol::message_type>(0xFFFF),
            empty_payload, "127.0.0.1:12345");

        CHECK(!result.has_value());
        CHECK(result.error() == comms::protocol::error_code::invalid_message_type);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}
