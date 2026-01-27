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
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/internet.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_generator.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/service/account_service.hpp"
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
const std::string database_table("production.iam_accounts_tbl");
const std::string tags("[messaging][handler][crud]");

save_account_request to_save_account_request(const domain::account& a) {
    save_account_request r;
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
    flags->set_bootstrap_mode(false, "test", "system.new_record", "Test setup");
    return flags;
}

std::shared_ptr<service::authorization_service>
make_auth_service(ores::database::context& ctx) {
    // RBAC permissions and roles are seeded via SQL scripts in the database template
    return std::make_shared<service::authorization_service>(ctx);
}

/**
 * @brief Sets up an authenticated admin session for testing.
 *
 * Creates a session with admin permissions and stores it in the session service.
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

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::comms::messaging::message_type;
using ores::utility::serialization::error_code;
using ores::testing::scoped_database_helper;

TEST_CASE("handle_single_save_account_request", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Original account: " << account;

    save_account_request rq(to_save_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            payload, test_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            save_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(!rp.account_id.is_nil());
    });
}

TEST_CASE("handle_many_save_account_requests", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    auto accounts = generate_synthetic_accounts(5);

    boost::asio::io_context io_ctx;
    for (const auto& a : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << a;

        save_account_request rq(to_save_account_request(a));
        BOOST_LOG_SEV(lg, info) << "Request: " << rq;

        const auto payload = rq.serialize();
        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::save_account_request,
                payload, test_endpoint);

            REQUIRE(r.has_value());
            const auto response_result =
                save_account_response::deserialize(r.value());
            REQUIRE(response_result.has_value());
            const auto& rp = response_result.value();
            BOOST_LOG_SEV(lg, info) << "Response " << ": " << rp;

            CHECK(!rp.account_id.is_nil());
        });
    }
}

TEST_CASE("handle_get_accounts_request_returns_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    get_accounts_request rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << rq;

    const auto payload = rq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::get_accounts_request,
            payload, test_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            get_accounts_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        // Test database may have accounts from previous runs; just verify
        // the handler returns successfully
        INFO("Number of accounts in response: " << rp.accounts.size());
    });
}

TEST_CASE("handle_get_accounts_request_with_accounts", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    // Get initial count before adding new accounts
    service::account_service account_svc(h.context());
    const auto initial_count = account_svc.list_accounts().size();
    BOOST_LOG_SEV(lg, info) << "Initial account count: " << initial_count;

    boost::asio::io_context io_ctx;

    const int new_accounts = 5;
    auto accounts = generate_synthetic_accounts(new_accounts);

    for (const auto& a : accounts) {
        BOOST_LOG_SEV(lg, info) << "Original account: " << a;

        save_account_request ca_rq(to_save_account_request(a));
        BOOST_LOG_SEV(lg, info) << "Create request: " << ca_rq;
        const auto create_payload = ca_rq.serialize();

        run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
            auto r = co_await sut.handle_message(
                message_type::save_account_request,
                create_payload, test_endpoint);
            REQUIRE(r.has_value());
        });
    }

    get_accounts_request la_rq;
    BOOST_LOG_SEV(lg, info) << "Request: " << la_rq;

    const auto payload = la_rq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::get_accounts_request,
            payload, test_endpoint);

        REQUIRE(r.has_value());
        const auto response_result =
            get_accounts_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        const auto& rp = response_result.value();
        BOOST_LOG_SEV(lg, info) << "Response: " << rp;

        CHECK(rp.accounts.size() == initial_count + new_accounts);
    });
}

TEST_CASE("handle_delete_account_request_success", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    const auto account = generate_synthetic_account();
    BOOST_LOG_SEV(lg, info) << "Account: " << account;

    save_account_request ca_rq(to_save_account_request(account));
    BOOST_LOG_SEV(lg, info) << "Request: " << ca_rq;

    const auto create_payload = ca_rq.serialize();

    boost::uuids::uuid account_id;
    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::save_account_request,
            create_payload, test_endpoint);
        REQUIRE(r.has_value());

        const auto response_result =
            save_account_response::deserialize(r.value());
        REQUIRE(response_result.has_value());
        account_id = response_result.value().account_id;
    });

    delete_account_request drq;
    drq.account_id = account_id;
    BOOST_LOG_SEV(lg, info) << "Delete request: " << drq;

    const auto delete_payload = drq.serialize();
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::delete_account_request,
            delete_payload, test_endpoint);

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

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

    // Set up authenticated admin session
    const auto test_endpoint = internet::endpoint();
    setup_admin_session(sessions, auth_service, test_endpoint);

    delete_account_request drq;
    drq.account_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Delete request: " << drq;

    const auto payload = drq.serialize();

    boost::asio::io_context io_ctx;
    run_coroutine_test(io_ctx, [&]() -> boost::asio::awaitable<void> {
        auto r = co_await sut.handle_message(
            message_type::delete_account_request,
            payload, test_endpoint);

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

TEST_CASE("handle_invalid_message_type", tags) {
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    scoped_database_helper h(database_table, true);
    auto system_flags = make_system_flags(h.context());
    auto sessions = std::make_shared<ores::comms::service::auth_session_service>();
    auto auth_service = make_auth_service(h.context());
    accounts_message_handler sut(h.context(), system_flags, sessions, auth_service, nullptr);

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
