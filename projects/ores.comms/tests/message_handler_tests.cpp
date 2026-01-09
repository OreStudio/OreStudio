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
#include <span>
#include <vector>
#include <string>
#include <cstdint>
#include <expected>
#include <catch2/catch_test_macros.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/messaging/message_dispatcher.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace {

const std::string_view test_suite("ores.comms.tests");
const std::string tags("[messaging][handler]");

// Test subsystem range: 0xF000 - 0xFFFF (reserved for testing)
constexpr std::uint16_t TEST_SUBSYSTEM_MIN = 0xF000;
constexpr std::uint16_t TEST_SUBSYSTEM_MAX = 0xFFFF;

// Test message types within the test subsystem range
enum class test_message_type : std::uint16_t {
    test_request = 0xF001,
    test_response = 0xF002,
    echo_request = 0xF003,
    echo_response = 0xF004
};

/**
 * @brief Test request message for handler tests.
 *
 * Contains a simple numeric value and a string field.
 */
struct test_request final {
    std::uint32_t value;
    std::string message;

    static std::vector<std::byte> serialize(const test_request& r) {
        std::vector<std::byte> buffer;
        ores::utility::serialization::writer::write_uint32(buffer, r.value);
        ores::utility::serialization::writer::write_string(buffer, r.message);
        return buffer;
    }

    static std::expected<test_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data) {
        auto value_result = ores::utility::serialization::reader::read_uint32(data);
        if (!value_result) {
            return std::unexpected(value_result.error());
        }

        auto message_result = ores::utility::serialization::reader::read_string(data);
        if (!message_result) {
            return std::unexpected(message_result.error());
        }

        return test_request{*value_result, *message_result};
    }
};

/**
 * @brief Test response message for handler tests.
 *
 * Contains a success flag, a processed value, and a response message.
 */
struct test_response final {
    bool success;
    std::uint32_t processed_value;
    std::string response_message;

    static std::vector<std::byte> serialize(const test_response& r) {
        std::vector<std::byte> buffer;
        ores::utility::serialization::writer::write_bool(buffer, r.success);
        ores::utility::serialization::writer::write_uint32(buffer, r.processed_value);
        ores::utility::serialization::writer::write_string(buffer, r.response_message);
        return buffer;
    }

    static std::expected<test_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data) {
        auto success_result = ores::utility::serialization::reader::read_bool(data);
        if (!success_result) {
            return std::unexpected(success_result.error());
        }

        auto value_result = ores::utility::serialization::reader::read_uint32(data);
        if (!value_result) {
            return std::unexpected(value_result.error());
        }

        auto message_result = ores::utility::serialization::reader::read_string(data);
        if (!message_result) {
            return std::unexpected(message_result.error());
        }

        return test_response{*success_result, *value_result, *message_result};
    }
};

/**
 * @brief Echo request message - simply echoes back the payload.
 */
struct echo_request final {
    std::string data;

    static std::vector<std::byte> serialize(const echo_request& r) {
        std::vector<std::byte> buffer;
        ores::utility::serialization::writer::write_string(buffer, r.data);
        return buffer;
    }

    static std::expected<echo_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data) {
        auto data_result = ores::utility::serialization::reader::read_string(data);
        if (!data_result) {
            return std::unexpected(data_result.error());
        }
        return echo_request{*data_result};
    }
};

/**
 * @brief Echo response message - contains the echoed data.
 */
struct echo_response final {
    std::string echoed_data;

    static std::vector<std::byte> serialize(const echo_response& r) {
        std::vector<std::byte> buffer;
        ores::utility::serialization::writer::write_string(buffer, r.echoed_data);
        return buffer;
    }

    static std::expected<echo_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte>& data) {
        auto data_result = ores::utility::serialization::reader::read_string(data);
        if (!data_result) {
            return std::unexpected(data_result.error());
        }
        return echo_response{*data_result};
    }
};

/**
 * @brief Test message handler implementation.
 *
 * Handles test_request and echo_request messages within the test subsystem.
 */
class test_message_handler final : public ores::comms::messaging::message_handler {
private:
    inline static std::string_view logger_name = "ores.comms.tests.test_message_handler";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    boost::asio::awaitable<std::expected<std::vector<std::byte>,
        ores::utility::serialization::error_code>>
    handle_message(ores::comms::messaging::message_type type,
                   std::span<const std::byte> payload,
                   const std::string& remote_address) override {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), debug) << "Handling message type "
                                   << static_cast<std::uint16_t>(type)
                                   << " from " << remote_address;

        switch (static_cast<test_message_type>(type)) {
            case test_message_type::test_request:
                co_return handle_test_request(payload);
            case test_message_type::echo_request:
                co_return handle_echo_request(payload);
            default:
                BOOST_LOG_SEV(lg(), error) << "Unknown message type: " << static_cast<std::uint16_t>(type);
                co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
        }
    }

private:
    std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
    handle_test_request(std::span<const std::byte> payload) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), debug) << "Processing test_request";

        auto request_result = test_request::deserialize(payload);
        if (!request_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize test_request";
            return std::unexpected(request_result.error());
        }

        const auto& request = *request_result;
        BOOST_LOG_SEV(lg(), debug) << "Received value=" << request.value
                                   << ", message=" << request.message;

        test_response response;
        response.success = true;
        response.processed_value = request.value * 2;
        response.response_message = "Processed: " + request.message;

        return test_response::serialize(response);
    }

    std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>
    handle_echo_request(std::span<const std::byte> payload) {
        using namespace ores::logging;
        BOOST_LOG_SEV(lg(), debug) << "Processing echo_request";

        auto request_result = echo_request::deserialize(payload);
        if (!request_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize echo_request";
            return std::unexpected(request_result.error());
        }

        echo_response response;
        response.echoed_data = request_result->data;

        return echo_response::serialize(response);
    }
};

}

using namespace ores::logging;
using namespace ores::comms::messaging;
using ores::testing::run_coroutine_test;
using ores::utility::serialization::error_code;

TEST_CASE("test_request_serialization_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    test_request original;
    original.value = 42;
    original.message = "Hello, handler!";

    BOOST_LOG_SEV(lg, debug) << "Original request: value=" << original.value
                             << ", message=" << original.message;

    auto serialized = test_request::serialize(original);
    REQUIRE(!serialized.empty());

    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    std::span<const std::byte> data(serialized);
    auto deserialized_result = test_request::deserialize(data);

    REQUIRE(deserialized_result.has_value());
    const auto& deserialized = *deserialized_result;

    CHECK(deserialized.value == original.value);
    CHECK(deserialized.message == original.message);

    BOOST_LOG_SEV(lg, debug) << "Roundtrip successful";
}

TEST_CASE("test_response_serialization_roundtrip", tags) {
    auto lg(make_logger(test_suite));

    test_response original;
    original.success = true;
    original.processed_value = 84;
    original.response_message = "Response message";

    BOOST_LOG_SEV(lg, debug) << "Original response: success=" << original.success
                             << ", processed_value=" << original.processed_value
                             << ", response_message=" << original.response_message;

    auto serialized = test_response::serialize(original);
    REQUIRE(!serialized.empty());

    std::span<const std::byte> data(serialized);
    auto deserialized_result = test_response::deserialize(data);

    REQUIRE(deserialized_result.has_value());
    const auto& deserialized = *deserialized_result;

    CHECK(deserialized.success == original.success);
    CHECK(deserialized.processed_value == original.processed_value);
    CHECK(deserialized.response_message == original.response_message);

    BOOST_LOG_SEV(lg, debug) << "Roundtrip successful";
}

TEST_CASE("test_handler_processes_test_request", tags) {
    auto lg(make_logger(test_suite));

    test_message_handler handler;

    test_request request;
    request.value = 21;
    request.message = "Test message";

    BOOST_LOG_SEV(lg, debug) << "Request: value=" << request.value
                             << ", message=" << request.message;

    auto payload = test_request::serialize(request);

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<message_type>(test_message_type::test_request),
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());

        std::span<const std::byte> response_data(*result);
        auto response_result = test_response::deserialize(response_data);

        REQUIRE(response_result.has_value());
        const auto& response = *response_result;

        BOOST_LOG_SEV(lg, debug) << "Response: success=" << response.success
                                 << ", processed_value=" << response.processed_value
                                 << ", response_message=" << response.response_message;

        CHECK(response.success == true);
        CHECK(response.processed_value == 42); // 21 * 2
        CHECK(response.response_message == "Processed: Test message");
    });
}

TEST_CASE("test_handler_processes_echo_request", tags) {
    auto lg(make_logger(test_suite));

    test_message_handler handler;

    echo_request request;
    request.data = "Echo this data!";

    BOOST_LOG_SEV(lg, debug) << "Echo request: data=" << request.data;

    auto payload = echo_request::serialize(request);

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<message_type>(test_message_type::echo_request),
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());

        std::span<const std::byte> response_data(*result);
        auto response_result = echo_response::deserialize(response_data);

        REQUIRE(response_result.has_value());
        const auto& response = *response_result;

        BOOST_LOG_SEV(lg, debug) << "Echo response: echoed_data=" << response.echoed_data;

        CHECK(response.echoed_data == request.data);
    });
}

TEST_CASE("test_handler_rejects_invalid_message_type", tags) {
    auto lg(make_logger(test_suite));

    test_message_handler handler;

    std::vector<std::byte> empty_payload;

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<message_type>(0xFFFF),
            empty_payload, "127.0.0.1:12345");

        CHECK(!result.has_value());
        CHECK(result.error() == error_code::invalid_message_type);
    });
}

TEST_CASE("test_dispatcher_with_registered_handler", tags) {
    auto lg(make_logger(test_suite));

    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    // Store a test session to pass authorization
    const std::string test_address = "127.0.0.1:54321";
    sessions->store_session(test_address, {boost::uuids::random_generator()()});

    message_dispatcher dispatcher(sessions);

    auto handler = std::make_shared<test_message_handler>();
    message_type_range test_range{TEST_SUBSYSTEM_MIN, TEST_SUBSYSTEM_MAX};
    dispatcher.register_handler(test_range, handler);

    BOOST_LOG_SEV(lg, debug) << "Registered handler for range ["
                             << std::hex << TEST_SUBSYSTEM_MIN << ", "
                             << TEST_SUBSYSTEM_MAX << "]";

    test_request request;
    request.value = 100;
    request.message = "Dispatcher test";

    auto payload = test_request::serialize(request);

    frame request_frame(
        static_cast<message_type>(test_message_type::test_request),
        1, // sequence
        payload);

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await dispatcher.dispatch(
            request_frame, 2, test_address);

        REQUIRE(result.has_value());

        const auto& response_frame = *result;
        BOOST_LOG_SEV(lg, debug) << "Response frame type: "
                                 << static_cast<std::uint16_t>(response_frame.header().type);

        // Response type should be request type + 1
        CHECK(static_cast<std::uint16_t>(response_frame.header().type) ==
              static_cast<std::uint16_t>(test_message_type::test_response));

        std::span<const std::byte> response_data(response_frame.payload());
        auto response_result = test_response::deserialize(response_data);

        REQUIRE(response_result.has_value());
        const auto& response = *response_result;

        CHECK(response.success == true);
        CHECK(response.processed_value == 200); // 100 * 2
        CHECK(response.response_message == "Processed: Dispatcher test");
    });
}

TEST_CASE("test_dispatcher_without_handler_returns_error", tags) {
    auto lg(make_logger(test_suite));

    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    // Store a test session to pass authorization
    const std::string test_address = "127.0.0.1:54321";
    sessions->store_session(test_address, {boost::uuids::random_generator()()});

    message_dispatcher dispatcher(sessions);
    // No handler registered

    test_request request;
    request.value = 50;
    request.message = "No handler test";

    auto payload = test_request::serialize(request);

    frame request_frame(
        static_cast<message_type>(test_message_type::test_request),
        1, // sequence
        payload);

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await dispatcher.dispatch(
            request_frame, 2, test_address);

        CHECK(!result.has_value());
        CHECK(result.error() == error_code::invalid_message_type);
    });
}

TEST_CASE("test_dispatcher_preserves_correlation_id", tags) {
    auto lg(make_logger(test_suite));

    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    // Store a test session to pass authorization
    const std::string test_address = "127.0.0.1:54321";
    sessions->store_session(test_address, {boost::uuids::random_generator()()});

    message_dispatcher dispatcher(sessions);

    auto handler = std::make_shared<test_message_handler>();
    message_type_range test_range{TEST_SUBSYSTEM_MIN, TEST_SUBSYSTEM_MAX};
    dispatcher.register_handler(test_range, handler);

    test_request request;
    request.value = 10;
    request.message = "Correlation test";

    auto payload = test_request::serialize(request);

    std::uint32_t expected_correlation_id = 0xDEADBEEF;
    frame request_frame(
        static_cast<message_type>(test_message_type::test_request),
        1, // sequence
        expected_correlation_id,
        payload);

    BOOST_LOG_SEV(lg, debug) << "Request correlation_id: " << expected_correlation_id;

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await dispatcher.dispatch(
            request_frame, 2, test_address);

        REQUIRE(result.has_value());

        const auto& response_frame = *result;
        BOOST_LOG_SEV(lg, debug) << "Response correlation_id: "
                                 << response_frame.correlation_id();

        CHECK(response_frame.correlation_id() == expected_correlation_id);
    });
}

TEST_CASE("test_handler_table_driven_values", tags) {
    auto lg(make_logger(test_suite));

    test_message_handler handler;

    struct test_case {
        std::uint32_t input_value;
        std::string input_message;
        std::uint32_t expected_processed_value;
        std::string expected_response_prefix;
    };

    std::vector<test_case> test_cases = {
        {0, "zero", 0, "Processed: zero"},
        {1, "one", 2, "Processed: one"},
        {100, "hundred", 200, "Processed: hundred"},
        {1000000, "large number", 2000000, "Processed: large number"},
        {42, "", 84, "Processed: "},
    };

    boost::asio::io_context io_ctx;

    for (const auto& tc : test_cases) {
        BOOST_LOG_SEV(lg, debug) << "Testing input_value=" << tc.input_value
                                 << ", input_message=" << tc.input_message;

        test_request request;
        request.value = tc.input_value;
        request.message = tc.input_message;

        auto payload = test_request::serialize(request);

        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto result = co_await handler.handle_message(
                static_cast<message_type>(test_message_type::test_request),
                payload, "127.0.0.1:12345");

            REQUIRE(result.has_value());

            std::span<const std::byte> response_data(*result);
            auto response_result = test_response::deserialize(response_data);

            REQUIRE(response_result.has_value());
            const auto& response = *response_result;

            CHECK(response.success == true);
            CHECK(response.processed_value == tc.expected_processed_value);
            CHECK(response.response_message == tc.expected_response_prefix);
        });
    }
}

TEST_CASE("test_echo_request_various_payloads", tags) {
    auto lg(make_logger(test_suite));

    test_message_handler handler;

    std::vector<std::string> test_data = {
        "",
        "a",
        "Hello, World!",
        "This is a longer test string with various characters: !@#$%^&*()",
        std::string(1000, 'x'),  // 1000 x characters
    };

    boost::asio::io_context io_ctx;

    for (const auto& data : test_data) {
        BOOST_LOG_SEV(lg, debug) << "Testing echo with data length: " << data.size();

        echo_request request;
        request.data = data;

        auto payload = echo_request::serialize(request);

        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto result = co_await handler.handle_message(
                static_cast<message_type>(test_message_type::echo_request),
                payload, "127.0.0.1:12345");

            REQUIRE(result.has_value());

            std::span<const std::byte> response_data(*result);
            auto response_result = echo_response::deserialize(response_data);

            REQUIRE(response_result.has_value());
            CHECK(response_result->echoed_data == data);
        });
    }
}
