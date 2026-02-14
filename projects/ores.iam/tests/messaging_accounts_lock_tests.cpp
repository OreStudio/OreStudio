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
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/internet.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_generator.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.variability/service/system_flags_service.hpp"

using namespace ores::iam;
using namespace ores::iam::messaging;
using ores::testing::run_coroutine_test;
using ores::utility::faker::internet;

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[messaging][handler][lock]");

save_account_request to_save_account_request(const domain::account& a) {
    save_account_request r;
    r.principal = a.username;
    r.password = faker::internet::password();
    r.totp_secret = a.totp_secret;
    r.email = a.email;
    return r;
}

std::shared_ptr<ores::variability::service::system_flags_service>
make_system_flags(ores::database::context& ctx, const std::string& tenant_id,
    const std::string& db_user) {
    auto flags = std::make_shared<ores::variability::service::system_flags_service>(
        ctx, tenant_id);
    flags->set_bootstrap_mode(false, db_user, "system.new_record", "Test setup");
    return flags;
}

std::shared_ptr<service::authorization_service>
make_auth_service(ores::database::context& ctx) {
    // RBAC permissions and roles are seeded via SQL scripts in the database template
    return std::make_shared<service::authorization_service>(ctx);
}

void assign_admin_role(std::shared_ptr<service::authorization_service> auth,
                       boost::uuids::uuid account_id,
                       const std::string& db_user) {
    auto admin_role = auth->find_role_by_name(domain::roles::super_admin);
    REQUIRE(admin_role.has_value());
    auth->assign_role(account_id, admin_role->id, db_user);
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
 * @param tenant_id The tenant ID for the session
 * @return The account ID of the test admin user
 */
boost::uuids::uuid setup_admin_session(
    std::shared_ptr<ores::comms::service::auth_session_service>& sessions,
    std::shared_ptr<service::authorization_service>& auth_service,
    const std::string& endpoint,
    const ores::utility::uuid::tenant_id& tenant_id) {
    // Create a test admin account ID
    auto account_id = boost::uuids::random_generator()();

    // Store session info for this endpoint
    ores::comms::service::session_info info{
        .account_id = account_id,
        .tenant_id = tenant_id,
        .username = "test_admin"
    };
    sessions->store_session(endpoint, info);

    // Assign admin role to the account
    auto admin_role = auth_service->find_role_by_name(domain::roles::super_admin);
    if (admin_role) {
        auth_service->assign_role(account_id, admin_role->id, "test_admin");
    }

    return account_id;
}

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::comms::messaging::message_type;
using ores::utility::serialization::error_code;
using ores::testing::scoped_database_helper;

TEST_CASE("handle_unlock_account_request", tags) {
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(true);
    auto ctx = ores::testing::make_generation_context(h);
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string(), h.db_user());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = internet::endpoint();

    // Set up authenticated admin session for account creation
    setup_admin_session(sessions, auth_service, admin_endpoint, h.tenant_id());

    // Create an admin account (to be the requester)
    auto admin_account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Admin account: " << admin_account;

    save_account_request admin_rq(to_save_account_request(admin_account));
    BOOST_LOG_SEV(lg, info) << "Admin request: " << admin_rq;

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            admin_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());

        const auto response_result =
            save_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        admin_id = response_result.value().account_id;
    });

    // Assign admin role to the newly created account
    assign_admin_role(auth_service, admin_id, h.db_user());

    // Login as admin to establish proper session with correct account_id
    login_request admin_login_rq;
    admin_login_rq.principal = admin_rq.principal;
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
    const auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    save_account_request ca_rq(to_save_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            create_payload, admin_endpoint);
        REQUIRE(r.has_value());

        const auto response_result =
            save_account_response::deserialize(r.value());
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
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(true);
    auto ctx = ores::testing::make_generation_context(h);
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string(), h.db_user());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Use a consistent remote address for admin session (to create accounts)
    const std::string admin_endpoint = internet::endpoint();
    // Use a different endpoint for non-admin session
    const std::string user_endpoint = internet::endpoint();

    // Set up authenticated admin session for account creation
    setup_admin_session(sessions, auth_service, admin_endpoint, h.tenant_id());

    // Create two regular (non-admin) accounts
    const auto account1 = generate_synthetic_account(ctx);
    save_account_request ca_rq1(to_save_account_request(account1));

    boost::uuids::uuid account1_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            ca_rq1.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        account1_id = save_account_response::deserialize(r.value()).value().account_id;
    });

    const auto account2 = generate_synthetic_account(ctx);
    save_account_request ca_rq2(to_save_account_request(account2));

    boost::uuids::uuid account2_id;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            ca_rq2.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        account2_id = save_account_response::deserialize(r.value()).value().account_id;
    });

    // Login as non-admin account1 to establish session
    login_request login_rq;
    login_rq.principal = ca_rq1.principal;
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
        CHECK(rp.results[0].message.find("Permission") != std::string::npos);
    });
}

TEST_CASE("handle_lock_account_request", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(true);
    auto ctx = ores::testing::make_generation_context(h);
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string(), h.db_user());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Use a consistent remote address for admin session
    const std::string admin_endpoint = internet::endpoint();

    // Set up authenticated admin session for account creation
    setup_admin_session(sessions, auth_service, admin_endpoint, h.tenant_id());

    // Create an admin account (to be the requester)
    auto admin_account = generate_synthetic_account(ctx);

    save_account_request admin_rq(to_save_account_request(admin_account));

    boost::uuids::uuid admin_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            admin_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        admin_id = save_account_response::deserialize(r.value()).value().account_id;
    });

    // Assign admin role to the newly created account
    assign_admin_role(auth_service, admin_id, h.db_user());

    // Login as admin to establish proper session with correct account_id
    login_request admin_login_rq;
    admin_login_rq.principal = admin_rq.principal;
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
    const auto account = generate_synthetic_account(ctx);
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    save_account_request ca_rq(to_save_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            create_payload, admin_endpoint);
        REQUIRE(r.has_value());

        const auto response_result =
            save_account_response::deserialize(r.value());
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

TEST_CASE("handle_lock_account_request_unauthenticated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(true);
    auto ctx = ores::testing::make_generation_context(h);
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string(), h.db_user());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Use admin endpoint to create the account
    const std::string admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint, h.tenant_id());

    // Create an account to try to lock
    const auto account = generate_synthetic_account(ctx);
    save_account_request ca_rq(to_save_account_request(account));

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            ca_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        account_id = save_account_response::deserialize(r.value()).value().account_id;
    });

    // Try to lock from a different endpoint without being logged in (no session)
    const std::string unauthenticated_endpoint = internet::endpoint();
    lock_account_request lrq;
    lrq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Lock request (unauthenticated): " << lrq;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::lock_account_request,
            lrq.serialize(), unauthenticated_endpoint);

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

    scoped_database_helper h(true);
    auto ctx = ores::testing::make_generation_context(h);
    auto system_flags = make_system_flags(h.context(), h.tenant_id().to_string(), h.db_user());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Use admin endpoint to create the account
    const std::string admin_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, admin_endpoint, h.tenant_id());

    // Create an account to try to unlock
    const auto account = generate_synthetic_account(ctx);
    save_account_request ca_rq(to_save_account_request(account));

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            ca_rq.serialize(), admin_endpoint);
        REQUIRE(r.has_value());
        account_id = save_account_response::deserialize(r.value()).value().account_id;
    });

    // Try to unlock from a different endpoint without being logged in (no session)
    const std::string unauthenticated_endpoint = internet::endpoint();
    unlock_account_request urq;
    urq.account_ids = {account_id};
    BOOST_LOG_SEV(lg, info) << "Unlock request (unauthenticated): " << urq;

    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::unlock_account_request,
            urq.serialize(), unauthenticated_endpoint);

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
