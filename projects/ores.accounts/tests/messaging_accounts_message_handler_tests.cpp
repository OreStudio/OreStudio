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
#include "ores.accounts/messaging/accounts_message_handler.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/faker/internet.hpp"
#include "ores.accounts/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.accounts/generators/account_generator.hpp"
#include "ores.accounts/messaging/protocol.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"

using namespace ores::accounts;
using namespace ores::accounts::messaging;
using ores::testing::run_coroutine_test;
using ores::utility::faker::internet;

namespace {

const std::string_view test_suite("ores.accounts.tests");
const std::string database_table("oresdb.accounts");
const std::string tags("[messaging][handler]");

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

std::shared_ptr<ores::variability::service::system_flags_service>
make_system_flags(ores::utility::database::context& ctx) {
    auto flags = std::make_shared<ores::variability::service::system_flags_service>(ctx);
    // Disable bootstrap mode so tests can proceed
    flags->set_bootstrap_mode(false, "test");
    return flags;
}

}

using namespace ores::utility::log;
using namespace ores::accounts::generators;

using ores::comms::messaging::message_type;
using ores::comms::messaging::error_code;
using ores::testing::scoped_database_helper;

TEST_CASE("handle_single_create_account_request", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            create_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(!rp.account_id.is_nil());
    });
}

TEST_CASE("handle_many_create_account_requests", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);
    auto accounts = generate_synthetic_accounts(5);

    boost::asio::io_context io_ctx;
    for (const auto& a : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << a;

        create_account_request rq(to_create_account_request(a));
        BOOST_LOG_SEV(lg, info) << "Request: " << rq;

        const auto payload = rq.serialize();
        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::create_account_request,
                payload, internet::endpoint());

            REQUIRE(r.has_value());
            const auto response_result =
                create_account_response::deserialize(r.value());
            REQUIRE(response_result.has_value());
            const auto& rp = response_result.value();
            BOOST_LOG_SEV(lg, info) << "Response " << ": " << rp;

            CHECK(!rp.account_id.is_nil());
        });
    }
}

TEST_CASE("handle_list_accounts_request_empty", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    list_accounts_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_accounts_request,
            payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            list_accounts_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.accounts.empty());
    });
}

TEST_CASE("handle_list_accounts_request_with_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    const int account_count = 5;
    auto accounts =
        generate_synthetic_accounts(account_count);

    boost::asio::io_context io_ctx;
    for (const auto& a : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << a;

        create_account_request ca_rq(to_create_account_request(a));
        BOOST_LOG_SEV(lg, info) << "Create request: " << ca_rq;
        const auto create_payload = ca_rq.serialize();

        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::create_account_request,
                create_payload, internet::endpoint());
            REQUIRE(r.has_value());
        });
    }

    list_accounts_request la_rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << la_rq;

    const auto payload = la_rq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::list_accounts_request,
            payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            list_accounts_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.accounts.size() == account_count);
    });
}

TEST_CASE("handle_login_request_with_valid_password", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, internet::endpoint());
        REQUIRE(r.has_value());
    });

    login_request lrq;
    lrq.username = ca_rq.username;
    lrq.password = ca_rq.password;
    BOOST_LOG_SEV(lg, info) << "Login request: " << lrq;

    const auto login_payload = lrq.serialize();

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result = login_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == true);
        CHECK(rp.error_message.empty());
        CHECK(rp.username == ca_rq.username);
        CHECK(!rp.account_id.is_nil());
    });
}

TEST_CASE("handle_login_request_with_invalid_password", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, internet::endpoint());
        REQUIRE(r.has_value());
    });

    // Attempt login with wrong password
    login_request lrq;
    lrq.username = ca_rq.username;
    lrq.password = "wrong_password";
    BOOST_LOG_SEV(lg, info) << "Login request: " << lrq;

    const auto login_payload = lrq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            login_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.success == false);
        CHECK(!response.error_message.empty());
    });
}

TEST_CASE("handle_unlock_account_request", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Create an account first
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, internet::endpoint());
        REQUIRE(r.has_value());

        const auto response_result =
            create_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        account_id = response_result.value().account_id;
    });

    // Attempt to unlock the account
    unlock_account_request urq;
    urq.account_id = account_id;
    BOOST_LOG_SEV(lg, info) << "Unlock request: " << urq;

    const auto unlock_payload = urq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::unlock_account_request,
            unlock_payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            unlock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == true);
        CHECK(rp.error_message.empty());
    });
}

TEST_CASE("handle_invalid_message_type", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    std::vector<std::byte> empty_payload;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            static_cast<message_type>(0xFFFF),
            empty_payload, internet::endpoint());

        CHECK(!r.has_value());
        CHECK(r.error() == error_code::invalid_message_type);
    });
}

TEST_CASE("handle_login_request_non_existent_user", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    login_request lrq;
    lrq.username = "non_existent_user_" +
        std::string(faker::number::hexadecimal(8));
    lrq.password = faker::internet::password();
    BOOST_LOG_SEV(lg, info) << "Login request: " << lrq;

    const auto payload = lrq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            payload, internet::endpoint());

        // The handler should return a valid response object indicating failure,
        // not a transport error (unless the message was malformed).
        REQUIRE(r.has_value());
        const auto response_result = login_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, debug) << "Response: " << rp;

        CHECK(rp.success == false);
        CHECK(rp.error_message.find("user") != std::string::npos);
    });
}

TEST_CASE("handle_delete_account_request_success", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Create an account first
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, internet::endpoint());
        REQUIRE(r.has_value());

        const auto response_result =
            create_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        account_id = response_result.value().account_id;
    });

    // Delete the account
    delete_account_request drq;
    drq.account_id = account_id;
    BOOST_LOG_SEV(lg, info) << "Delete request: " << drq;

    const auto delete_payload = drq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::delete_account_request,
            delete_payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            delete_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == true);
        CHECK(rp.message == "Account deleted successfully");
    });
}

TEST_CASE("handle_delete_account_request_non_existent_account", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Try to delete a non-existent account
    delete_account_request drq;
    drq.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Delete request: " << drq;

    const auto payload = drq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::delete_account_request,
            payload, internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            delete_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == false);
        CHECK(rp.message.find("does not exist") != std::string::npos);
    });
}
