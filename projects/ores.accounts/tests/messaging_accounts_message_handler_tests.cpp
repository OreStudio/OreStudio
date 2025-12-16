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
    r.recorded_by = a.recorded_by;
    r.is_admin = a.is_admin;
    return r;
}

std::shared_ptr<ores::variability::service::system_flags_service>
make_system_flags(ores::database::context& ctx) {
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

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = "192.168.1.100:54321";

    // Create an admin account first (to be the requester)
    auto admin_account = generate_synthetic_account();
    admin_account.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Admin account: " << admin_account;

    create_account_request admin_rq(to_create_account_request(admin_account));
    admin_rq.is_admin = true;
    BOOST_LOG_SEV(lg, info) << "Admin request: " << admin_rq;

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            admin_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());

        const auto response_result =
            create_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        admin_id = response_result.value().account_id;
    });

    // Login as admin to establish session
    login_request admin_login_rq;
    admin_login_rq.username = admin_rq.username;
    admin_login_rq.password = admin_rq.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            admin_login_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // Create a regular account
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
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

    // Attempt to unlock the account (admin is logged in from admin_endpoint)
    unlock_account_request urq;
    urq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Unlock request: " << urq;

    const auto unlock_payload = urq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::unlock_account_request,
            unlock_payload, admin_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            unlock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.results.size() == 1);
        CHECK(rp.results[0].success == true);
        CHECK(rp.results[0].message.empty());
    });
}

TEST_CASE("handle_unlock_account_request_non_admin", tags) {
    using namespace ores::utility::log;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Use a consistent remote address for non-admin session
    const std::string user_endpoint = "192.168.1.100:54321";

    // Create two regular (non-admin) accounts
    const auto account1 = generate_synthetic_account();
    create_account_request ca_rq1(to_create_account_request(account1));

    boost::uuids::uuid account1_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq1.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        account1_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    const auto account2 = generate_synthetic_account();
    create_account_request ca_rq2(to_create_account_request(account2));

    boost::uuids::uuid account2_id;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq2.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        account2_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Login as non-admin account1 to establish session
    login_request login_rq;
    login_rq.username = ca_rq1.username;
    login_rq.password = ca_rq1.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_rq.serialize(), user_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // Try to unlock account2 using non-admin account1's session
    unlock_account_request urq;
    urq.account_ids = {account2_id};
    BOOST_LOG_SEV(lg, info) << "Unlock request (non-admin): " << urq;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::unlock_account_request,
            urq.serialize(), user_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            unlock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.results.size() == 1);
        CHECK(rp.results[0].success == false);
        CHECK(rp.results[0].message.find("Admin") != std::string::npos);
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

TEST_CASE("handle_lock_account_request", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = "192.168.1.100:54321";

    // Create an admin account first (to be the requester)
    auto admin_account = generate_synthetic_account();
    admin_account.is_admin = true;

    create_account_request admin_rq(to_create_account_request(admin_account));
    admin_rq.is_admin = true;

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            admin_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        admin_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Login as admin to establish session
    login_request admin_login_rq;
    admin_login_rq.username = admin_rq.username;
    admin_login_rq.password = admin_rq.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            admin_login_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // Create a regular account to lock
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
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

    // Lock the account (admin is logged in from admin_endpoint)
    lock_account_request lrq;
    lrq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Lock request: " << lrq;

    const auto lock_payload = lrq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::lock_account_request,
            lock_payload, admin_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            lock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.results.size() == 1);
        CHECK(rp.results[0].success == true);
        CHECK(rp.results[0].message.empty());
    });
}

TEST_CASE("handle_login_request_locked_account", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = "192.168.1.100:54321";
    const std::string user_endpoint = "192.168.1.200:12345";

    // 1. Create an admin account (to be the requester for locking)
    auto admin_account = generate_synthetic_account();
    admin_account.is_admin = true;

    create_account_request admin_rq(to_create_account_request(admin_account));
    admin_rq.is_admin = true;

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            admin_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        admin_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Login as admin to establish session
    login_request admin_login_rq;
    admin_login_rq.username = admin_rq.username;
    admin_login_rq.password = admin_rq.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            admin_login_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // 2. Create a regular account
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
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

    // 3. Lock the account using lock_account_request (admin is logged in)
    lock_account_request lock_rq;
    lock_rq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Lock account request: " << lock_rq;

    const auto lock_payload = lock_rq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::lock_account_request,
            lock_payload, admin_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            lock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        REQUIRE(response_result.value().results.size() == 1);
        CHECK(response_result.value().results[0].success == true);
    });

    // 4. Attempt login with valid credentials for the now-locked account
    login_request login_rq;
    login_rq.username = ca_rq.username;
    login_rq.password = ca_rq.password;
    BOOST_LOG_SEV(lg, info) << "Attempting login for locked user: "
                            << login_rq.username;

    const auto login_payload = login_rq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_payload, user_endpoint);

        REQUIRE(r.has_value());
        const auto response_result = login_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& response = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << response;

        CHECK(response.success == false);
        CHECK(response.error_message.find("locked") != std::string::npos);
    });
}

TEST_CASE("handle_lock_account_request_unauthenticated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Create an account to try to lock
    const auto account = generate_synthetic_account();
    create_account_request ca_rq(to_create_account_request(account));

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        account_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Try to lock without being logged in (no session established)
    lock_account_request lrq;
    lrq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Lock request (unauthenticated): " << lrq;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::lock_account_request,
            lrq.serialize(), "192.168.1.50:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            lock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.results.size() == 1);
        CHECK(rp.results[0].success == false);
        CHECK(rp.results[0].message.find("Authentication") != std::string::npos);
    });
}

TEST_CASE("handle_unlock_account_request_unauthenticated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    accounts_message_handler sut(h.context(), system_flags, sessions);

    // Create an account to try to unlock
    const auto account = generate_synthetic_account();
    create_account_request ca_rq(to_create_account_request(account));

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        account_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Try to unlock without being logged in (no session established)
    unlock_account_request urq;
    urq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Unlock request (unauthenticated): " << urq;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::unlock_account_request,
            urq.serialize(), "192.168.1.50:12345");

        REQUIRE(r.has_value());
        const auto response_result =
            unlock_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        REQUIRE(rp.results.size() == 1);
        CHECK(rp.results[0].success == false);
        CHECK(rp.results[0].message.find("Authentication") != std::string::npos);
    });
}
