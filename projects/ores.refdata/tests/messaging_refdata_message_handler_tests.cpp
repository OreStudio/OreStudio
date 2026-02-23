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
#include "ores.refdata/messaging/refdata_message_handler.hpp"

#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.refdata/domain/currency_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.refdata/repository/currency_repository.hpp"
#include "ores.refdata/generators/currency_generator.hpp"
#include "ores.variability/service/system_flags_service.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[messaging][handler]");

std::shared_ptr<ores::variability::service::system_flags_service>
make_system_flags(ores::database::context& ctx, const std::string& tenant_id) {
    auto flags = std::make_shared<ores::variability::service::system_flags_service>(
        ctx, tenant_id);
    // Disable bootstrap mode so tests can proceed
    flags->set_bootstrap_mode(false, "test", "system.new_record", "Test setup");
    return flags;
}

const std::string test_remote_address = "127.0.0.1:12345";
const std::string test_username = "test_user";

std::shared_ptr<ores::comms::service::auth_session_service>
make_sessions(const ores::utility::uuid::tenant_id& tenant_id) {
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    // Create a test session with a known username
    auto session = std::make_shared<ores::comms::service::session_data>();
    session->id = boost::uuids::random_generator()();
    session->account_id = boost::uuids::random_generator()();
    session->tenant_id = tenant_id;
    session->username = test_username;
    session->start_time = std::chrono::system_clock::now();
    sessions->store_session_data(test_remote_address, session);
    return sessions;
}

}

using namespace ores;
using namespace ores::logging;
using ores::refdata::domain::currency;
using namespace ores::refdata::messaging;
using namespace ores::refdata::generators;
using ores::testing::scoped_database_helper;

TEST_CASE("handle_get_currencies_request_returns_currencies", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        // Test database may have currencies from previous runs
        INFO("Currency count: " << response.currencies.size());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_single_currency", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ccy = generate_synthetic_currency(ctx);
    refdata::repository::currency_repository repo;
    repo.write(h.context(), {ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << ccy;

    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        // Find our specific currency in the response
        auto it = std::ranges::find_if(response.currencies,
            [&ccy](const auto& c) { return c.iso_code == ccy.iso_code; });
        REQUIRE(it != response.currencies.end());
        CHECK(it->iso_code == ccy.iso_code);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    // Create multiple test currencies
    refdata::repository::currency_repository repo;

    auto currencies = generate_unique_synthetic_currencies(5, ctx);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;
    repo.write(h.context(), currencies);

    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response with " << response.currencies.size()
                                << " currencies";

        // Handler may limit results; verify our currencies are in the response
        CHECK(response.currencies.size() >= currencies.size());

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

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    refdata::repository::currency_repository repo;

    // Create random currencies
    const int new_currencies = 10;
    auto currencies = generate_unique_synthetic_currencies(new_currencies, ctx);

    repo.write(h.context(), currencies);

    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response with " << response.currencies.size()
                                 << " currencies";

        // Handler may limit results; just verify we got some currencies
        CHECK(response.currencies.size() >= new_currencies);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_verify_serialization_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    // Create a currency with specific values using unique code
    static std::atomic<int> counter{0};
    const auto unique_code = "XRT" + std::to_string(++counter);

    refdata::domain::currency original_ccy;
    original_ccy.iso_code = unique_code;
    original_ccy.name = "Test Crypto " + unique_code;
    original_ccy.numeric_code = "99999";
    original_ccy.symbol = "₿";
    original_ccy.fraction_symbol = "sat";
    original_ccy.fractions_per_unit = 100000000;
    original_ccy.rounding_type = "Closest";
    original_ccy.rounding_precision = 8;
    original_ccy.format = "%3% %1$.8f";
    original_ccy.asset_class = "crypto";
    original_ccy.market_tier = "emerging";
    original_ccy.modified_by = "system";
    original_ccy.change_reason_code = "system.test";
    original_ccy.change_commentary = "Test data";
    original_ccy.recorded_at = {};

    refdata::repository::currency_repository repo;
    repo.write(h.context(), {original_ccy});
    BOOST_LOG_SEV(lg, debug) << "Created test currency: " << original_ccy;

    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << response;

        // Find our specific currency
        auto it = std::ranges::find_if(response.currencies,
            [&unique_code](const auto& c) { return c.iso_code == unique_code; });
        REQUIRE(it != response.currencies.end());
        const auto& retrieved_ccy = *it;

        CHECK(retrieved_ccy.iso_code == original_ccy.iso_code);
        CHECK(retrieved_ccy.name == original_ccy.name);
        CHECK(retrieved_ccy.numeric_code == original_ccy.numeric_code);
        CHECK(retrieved_ccy.symbol == original_ccy.symbol);
        CHECK(retrieved_ccy.fraction_symbol == original_ccy.fraction_symbol);
        CHECK(retrieved_ccy.fractions_per_unit == original_ccy.fractions_per_unit);
        CHECK(retrieved_ccy.rounding_type == original_ccy.rounding_type);
        CHECK(retrieved_ccy.rounding_precision == original_ccy.rounding_precision);
        CHECK(retrieved_ccy.format == original_ccy.format);
        CHECK(retrieved_ccy.asset_class == original_ccy.asset_class);
        CHECK(retrieved_ccy.market_tier == original_ccy.market_tier);
        CHECK(retrieved_ccy.modified_by == original_ccy.modified_by);
        CHECK(retrieved_ccy.recorded_at != std::chrono::system_clock::time_point{});

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_get_currencies_request_with_unicode_symbols", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto currencies = generate_synthetic_unicode_currencies(ctx);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    refdata::repository::currency_repository repo;
    repo.write(h.context(), currencies);
    BOOST_LOG_SEV(lg, debug) << "Currencies written to db.";

    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    get_currencies_request req;
    BOOST_LOG_SEV(lg, debug) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            comms::messaging::message_type::get_currencies_request,
            payload, test_remote_address);

        REQUIRE(result.has_value());
        const auto response_result = get_currencies_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();

        // Verify all written currencies are present in response
        CHECK(response.currencies.size() >= currencies.size());

        for (const auto& written : currencies) {
            auto it = std::ranges::find_if(response.currencies,
                [&written](const auto& c) { return c.iso_code == written.iso_code; });
            REQUIRE(it != response.currencies.end());
            BOOST_LOG_SEV(lg, debug) << "Currency: " << it->iso_code << " = " << it->symbol;
            CHECK(it->symbol == written.symbol);
        }

        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_invalid_message_type",
    tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string());
    refdata_message_handler handler(h.context(), system_flags, make_sessions(h.tenant_id()));

    std::vector<std::byte> empty_payload;

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<comms::messaging::message_type>(0xFFFF),
            empty_payload, test_remote_address);

        CHECK(!result.has_value());
        CHECK(result.error() == ores::utility::serialization::error_code::invalid_message_type);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}
