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
#include "ores.iam/messaging/accounts_message_handler.hpp"

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
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_generator.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/rbac_seeder.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"

using namespace ores::iam;
using namespace ores::iam::messaging;
using ores::testing::run_coroutine_test;
using ores::utility::faker::internet;

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string database_table("ores.accounts");
const std::string tags("[messaging][handler][auth]");

create_account_request to_create_account_request(const domain::account& a) {
    create_account_request r;
    r.username = a.username;
    r.password = faker::internet::password();
    r.totp_secret = a.totp_secret;
    r.email = a.email;
    r.recorded_by = a.recorded_by;
    return r;
}

std::shared_ptr<ores::variability::service::system_flags_service>
make_system_flags(ores::database::context& ctx) {
    auto flags = std::make_shared<ores::variability::service::system_flags_service>(ctx);
    flags->set_bootstrap_mode(false, "test");
    return flags;
}

std::shared_ptr<service::authorization_service>
make_auth_service(ores::database::context& ctx) {
    auto auth = std::make_shared<service::authorization_service>(ctx);
    service::rbac_seeder seeder(*auth);
    seeder.seed("test");
    return auth;
}

void assign_admin_role(std::shared_ptr<service::authorization_service> auth,
                       boost::uuids::uuid account_id) {
    auto admin_role = auth->find_role_by_name(domain::roles::admin);
    REQUIRE(admin_role.has_value());
    auth->assign_role(account_id, admin_role->id, "test");
}

/**
 * @brief Sets up an authenticated admin session for testing.
 *
 * Creates a session with admin permissions and stores it in the session service.
 * This is used to bootstrap tests that need authentication before creating accounts.
 *
 * @param sessions The session service to store the session in
 * @param auth_service The auth service to look up the admin role
 * @param endpoint The remote address to associate with the session
 * @return The account ID of the test admin user
 */
boost::uuids::uuid setup_admin_session(
    std::shared_ptr<ores::comms::service::auth_session_service>& sessions,
    std::shared_ptr<service::authorization_service>& auth_service,
    const std::string& endpoint) {
    // Create a test admin account ID
    auto account_id = boost::uuids::random_generator()();

    // Store session info for this endpoint
    ores::comms::service::session_info info{.account_id = account_id};
    sessions->store_session(endpoint, info);

    // Assign admin role to the account
    auto admin_role = auth_service->find_role_by_name(domain::roles::admin);
    if (admin_role) {
        auth_service->assign_role(account_id, admin_role->id, "test");
    }

    return account_id;
}

}

using namespace ores::utility::log;
using namespace ores::iam::generators;

using ores::comms::messaging::message_type;
using ores::comms::messaging::error_code;
using ores::testing::scoped_database_helper;

TEST_CASE("handle_login_request_with_valid_password", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Set up authenticated admin session for account creation
    const auto admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, admin_endpoint);
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
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Set up authenticated admin session for account creation
    const auto admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            create_payload, admin_endpoint);
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

TEST_CASE("handle_login_request_non_existent_user", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

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

TEST_CASE("handle_login_request_locked_account", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = internet::endpoint();
    const std::string user_endpoint = internet::endpoint();

    // Set up authenticated admin session for account creation
    setup_admin_session(sessions, auth_service, admin_endpoint);

    // 1. Create an admin account (to be the requester for locking)
    auto admin_account = generate_synthetic_account();

    create_account_request admin_rq(to_create_account_request(admin_account));

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            admin_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        admin_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Assign admin role to the newly created account
    assign_admin_role(auth_service, admin_id);

    // Login as admin to establish proper session with correct account_id
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
            create_payload, admin_endpoint);
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

TEST_CASE("handle_change_password_request_success", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Set up authenticated admin session for account creation
    const std::string admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint);

    // Use a different endpoint for user session
    const std::string user_endpoint = internet::endpoint();

    // Create a user account
    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    create_account_request ca_rq(to_create_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Create account request: " << ca_rq;

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        account_id = create_account_response::deserialize(r.value()).value().account_id;
    });

    // Login to establish session
    login_request login_rq;
    login_rq.username = ca_rq.username;
    login_rq.password = ca_rq.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_rq.serialize(), user_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // Change password
    change_password_request cp_rq;
    cp_rq.new_password = "NewSecurePassword123!";
    BOOST_LOG_SEV(lg, info) << "Change password request";

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::change_password_request,
            cp_rq.serialize(), user_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            change_password_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;
        INFO("Change password error: " << rp.message);
        REQUIRE(rp.success == true);
    });

    // Verify the new password works by logging in again
    login_request new_login_rq;
    new_login_rq.username = ca_rq.username;
    new_login_rq.password = "NewSecurePassword123!";

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            new_login_rq.serialize(), internet::endpoint());
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });
}

TEST_CASE("handle_change_password_request_unauthenticated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Try to change password without being logged in (no session established)
    change_password_request cp_rq;
    cp_rq.new_password = "NewSecurePassword123!";
    BOOST_LOG_SEV(lg, info) << "Change password request (unauthenticated)";

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::change_password_request,
            cp_rq.serialize(), internet::endpoint());

        REQUIRE(r.has_value());
        const auto response_result =
            change_password_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == false);
        CHECK(rp.message.find("Authentication") != std::string::npos);
    });
}

TEST_CASE("handle_change_password_request_weak_password", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service);

    // Set up authenticated admin session for account creation
    const std::string admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint);

    // Use a different endpoint for user session
    const std::string user_endpoint = internet::endpoint();

    // Create a user account
    const auto account = generate_synthetic_account();
    create_account_request ca_rq(to_create_account_request(account));

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::create_account_request,
            ca_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
    });

    // Login to establish session
    login_request login_rq;
    login_rq.username = ca_rq.username;
    login_rq.password = ca_rq.password;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::login_request,
            login_rq.serialize(), user_endpoint);
        REQUIRE(r.has_value());
        auto login_resp = login_response::deserialize(r.value());
        REQUIRE(login_resp.has_value());
        CHECK(login_resp->success == true);
    });

    // Try to change password to a weak password (less than 12 characters)
    change_password_request cp_rq;
    cp_rq.new_password = "short";
    BOOST_LOG_SEV(lg, info) << "Change password request (weak password)";

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::change_password_request,
            cp_rq.serialize(), user_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            change_password_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.success == false);
        CHECK(rp.message.find("12") != std::string::npos);
    });
}
