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
#include <boost/uuid/uuid_generators.hpp>
#include "ores.testing/database_helper.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "faker-cxx/faker.h" // IWYU pragma: keep.
#include "ores.accounts/generators/account_generator.hpp"
#include "ores.accounts/messaging/accounts_message_handler.hpp"
#include "ores.accounts/messaging/protocol.hpp"

using namespace ores::accounts;
using namespace ores::accounts::messaging;

namespace {

const std::string test_suite("ores.accounts.tests");
const std::string database_table("oresdb.accounts");

create_account_request to_create_account_request(const domain::account& a) {
    create_account_request r;
    r.username = a.username;
    r.password = faker::internet::password();
    r.totp_secret = a.totp_secret;
    r.email = a.email;
    r.modified_by = a.modified_by;
    r.is_admin = a.is_admin;
    return r;
}

}

using namespace ores::utility::log;
using ores::comms::protocol::message_type;
using ores::comms::protocol::error_code;
using ores::testing::database_helper;
using ores::accounts::generators::generate_fake_accounts;

TEST_CASE("handle_single_create_account_request",
    "[messaging_accounts_message_handler_tests]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());

    const auto account = *generate_fake_accounts().begin();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request req(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::create_account_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = create_account_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(!response.account_id.is_nil());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_many_create_account_requests",
    "[messaging_accounts_message_handler_tests]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());
    auto accounts =
        (generate_fake_accounts() | std::views::take(5)) |
        std::ranges::to<std::vector>();

    for (const auto& acc : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << acc;

        create_account_request req(to_create_account_request(acc));
        BOOST_LOG_SEV(lg, info) << "Request: " << req;

        const auto payload = req.serialize();

        boost::asio::io_context io_context;
        bool test_completed = false;

        boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
            auto result = co_await handler.handle_message(
                message_type::create_account_request,
                payload, "127.0.0.1:12345");

            REQUIRE(result.has_value());
            const auto response_result =
                create_account_response::deserialize(result.value());
            REQUIRE(response_result.has_value());
            const auto& response = response_result.value();
            BOOST_LOG_SEV(lg, info) << "Response " << ": " << response;

            CHECK(!response.account_id.is_nil());
            test_completed = true;
        }, boost::asio::detached);

        io_context.run();
        REQUIRE(test_completed);
    }
}

TEST_CASE("handle_list_accounts_request_empty",
    "[messaging_accounts_message_handler_tests]") {

    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);
    accounts_message_handler handler(h.get_context());

    list_accounts_request req;
    BOOST_LOG_SEV(lg, info) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::list_accounts_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = list_accounts_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.accounts.empty());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_list_accounts_request_with_accounts",
    "[messaging_accounts_message_handler_tests]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());

    const int account_count = 5;
    auto accounts =
        (generate_fake_accounts() | std::views::take(account_count)) |
        std::ranges::to<std::vector>();

    for (const auto& acc : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << acc;

        create_account_request create_req(to_create_account_request(acc));
        BOOST_LOG_SEV(lg, info) << "Create request: " << create_req;
        const auto create_payload = create_req.serialize();

        boost::asio::io_context create_io_context;
        boost::asio::co_spawn(create_io_context, [&]() -> boost::asio::awaitable<void> {
            auto result = co_await handler.handle_message(
                message_type::create_account_request,
                create_payload, "127.0.0.1:12345");
            REQUIRE(result.has_value());
        }, boost::asio::detached);
        create_io_context.run();
    }

    // Now list the accounts
    list_accounts_request req;
    BOOST_LOG_SEV(lg, info) << "Request: " << req;

    const auto payload = req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::list_accounts_request,
            payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = list_accounts_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.accounts.size() == account_count);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_login_request_with_valid_credentials",
    "[messaging_accounts_message_handler_tests]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());

    // Create an account first
    const auto account = *generate_fake_accounts().begin();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request create_req(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << create_req;

    const auto create_payload = create_req.serialize();

    boost::asio::io_context create_io_context;
    boost::asio::co_spawn(create_io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::create_account_request,
            create_payload, "127.0.0.1:12345");
        REQUIRE(result.has_value());
    }, boost::asio::detached);
    create_io_context.run();

    // Now attempt login
    login_request login_req;
    login_req.username = create_req.username;
    login_req.password = create_req.password;
    BOOST_LOG_SEV(lg, info) << "Request: " << login_req;

    const auto login_payload = login_req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::login_request,
            login_payload, "192.168.1.100:54321");

        REQUIRE(result.has_value());
        const auto response_result = login_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.success == true);
        CHECK(response.error_message.empty());
        CHECK(response.username == create_req.username);
        CHECK(!response.account_id.is_nil());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_login_request_with_invalid_credentials",
    "[messaging_accounts_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());

    // Create an account first
    const auto account = *generate_fake_accounts().begin();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request create_req(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << create_req;

    const auto create_payload = create_req.serialize();

    boost::asio::io_context create_io_context;
    boost::asio::co_spawn(create_io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::create_account_request,
            create_payload, "127.0.0.1:12345");
        REQUIRE(result.has_value());
    }, boost::asio::detached);
    create_io_context.run();

    // Attempt login with wrong password
    login_request login_req;
    login_req.username = create_req.username;
    login_req.password = "wrong_password";
    BOOST_LOG_SEV(lg, info) << "Request: " << login_req;

    const auto login_payload = login_req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::login_request,
            login_payload, "192.168.1.100:54321");

        REQUIRE(result.has_value());
        const auto response_result = login_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.success == false);
        CHECK(!response.error_message.empty());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_unlock_account_request",
    "[messaging_accounts_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    accounts_message_handler handler(h.get_context());

    // Create an account first
    const auto account = *generate_fake_accounts().begin();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request create_req(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << create_req;

    const auto create_payload = create_req.serialize();

    boost::uuids::uuid account_id;

    boost::asio::io_context create_io_context;
    boost::asio::co_spawn(create_io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::create_account_request,
            create_payload, "127.0.0.1:12345");
        REQUIRE(result.has_value());

        const auto response_result = create_account_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        account_id = response_result.value().account_id;
    }, boost::asio::detached);
    create_io_context.run();

    // Attempt to unlock the account
    unlock_account_request unlock_req;
    unlock_req.account_id = account_id;
    BOOST_LOG_SEV(lg, info) << "Request: " << unlock_req;

    const auto unlock_payload = unlock_req.serialize();

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            message_type::unlock_account_request,
            unlock_payload, "127.0.0.1:12345");

        REQUIRE(result.has_value());
        const auto response_result = unlock_account_response::deserialize(result.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.success == true);
        CHECK(response.error_message.empty());
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}

TEST_CASE("handle_invalid_message_type",
    "[messaging_accounts_message_handler_tests]") {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    database_helper h;
    accounts_message_handler handler(h.get_context());

    std::vector<std::uint8_t> empty_payload;

    boost::asio::io_context io_context;
    bool test_completed = false;

    boost::asio::co_spawn(io_context, [&]() -> boost::asio::awaitable<void> {
        auto result = co_await handler.handle_message(
            static_cast<message_type>(0xFFFF),
            empty_payload, "127.0.0.1:12345");

        CHECK(!result.has_value());
        CHECK(result.error() == error_code::invalid_message_type);
        test_completed = true;
    }, boost::asio::detached);

    io_context.run();
    REQUIRE(test_completed);
}
