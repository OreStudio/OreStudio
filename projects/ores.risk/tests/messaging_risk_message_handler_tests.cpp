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
#include "ores.risk/messaging/risk_message_handler.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.risk/messaging/protocol.hpp"
#include "ores.risk/repository/currency_repository.hpp"
#include "ores.risk/generators/currency_generator.hpp"

namespace {

const std::string test_suite("ores.risk.tests");
const std::string database_table("oresdb.currencies");
const std::string tags("[messaging_risk_message_handler_tests]");

}

using namespace ores;
using namespace ores::utility::log;
using ores::risk::domain::currency;
using namespace ores::risk::messaging;
using namespace ores::risk::generators;
using ores::testing::database_helper;

TEST_CASE("handle_get_currencies_request_empty", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    risk_message_handler handler(h.get_context());

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

TEST_CASE("handle_get_currencies_request_with_single_currency", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto ccy = generate_synthetic_currency();
    risk::repository::currency_repository repo;
    repo.write(h.get_context(), {ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << ccy;

    risk_message_handler handler(h.get_context());

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
        CHECK(response.currencies[0].iso_code == ccy.iso_code);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    // Create multiple test currencies
    risk::repository::currency_repository repo;

    auto currencies = generate_unique_synthetic_currencies(5);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;
    repo.write(h.get_context(), currencies);

    risk_message_handler handler(h.get_context());

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

        CHECK(response.currencies.size() == currencies.size());

        for (const auto& ccy : response.currencies) {
            BOOST_LOG_SEV(lg, debug) << "Currency: " << ccy.iso_code;
            CHECK(!ccy.iso_code.empty());
        }

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_faker", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    // Create random currencies
    const int currency_count = 10;
    auto currencies = generate_unique_synthetic_currencies(currency_count);

    risk::repository::currency_repository repo;
    repo.write(h.get_context(), currencies);

    risk_message_handler handler(h.get_context());

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

TEST_CASE("handle_get_currencies_request_verify_serialization_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

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
    repo.write(h.get_context(), {original_ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << original_ccy;

    risk_message_handler handler(h.get_context());

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

TEST_CASE("handle_get_currencies_request_with_unicode_symbols", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    // Create currencies with special Unicode symbols
    std::vector<std::pair<std::string, std::string>> currency_data = {
        {"EUR", "€"},
        {"USD", "$"},
        {"GBP", "£"},
        {"JPY", "¥"},
        {"INR", "₹"},
        {"BTC", "₿"},
        {"RUB", "₽"}
    };

    auto currencies = generate_synthetic_unicode_currencies();
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    risk::repository::currency_repository repo;
    repo.write(h.get_context(), currencies);
    BOOST_LOG_SEV(lg, debug) << "Currencies written to db.";

    risk_message_handler handler(h.get_context());

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

TEST_CASE("handle_invalid_message_type",
    tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    risk_message_handler handler(h.get_context());

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
