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
#include "ores.comms/service/auth_session_service.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace {

const std::string_view test_suite("ores.comms.tests");
const std::string tags("[service][auth_session]");

}

using namespace ores::logging;
using ores::comms::service::auth_session_service;
using ores::comms::service::session_info;
using ores::comms::service::session_data;
using ores::comms::service::client_info;
using ores::comms::messaging::message_type;
using ores::utility::serialization::error_code;

TEST_CASE("test_store_and_get_session_roundtrip", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:5000");
    const auto account_id = boost::uuids::random_generator()();

    session_info info{.account_id = account_id, .username = "alice"};
    svc.store_session(addr, info);

    auto result = svc.get_session(addr);
    REQUIRE(result.has_value());
    CHECK(result->account_id == account_id);
    CHECK(result->username == "alice");

    BOOST_LOG_SEV(lg, debug) << "Session roundtrip verified for " << addr;
}

TEST_CASE("test_get_session_returns_nullopt_for_unknown", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    auto result = svc.get_session("unknown:1234");

    CHECK(!result.has_value());
    BOOST_LOG_SEV(lg, debug) << "get_session correctly returned nullopt";
}

TEST_CASE("test_is_authenticated_returns_false_for_unknown", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    CHECK(!svc.is_authenticated("unknown:1234"));
}

TEST_CASE("test_is_authenticated_returns_true_after_store", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:5001");
    const auto account_id = boost::uuids::random_generator()();

    CHECK(!svc.is_authenticated(addr));
    svc.store_session(addr, {.account_id = account_id});
    CHECK(svc.is_authenticated(addr));
}

TEST_CASE("test_store_and_get_session_data_roundtrip", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:5002");

    auto session = std::make_shared<session_data>();
    session->account_id = boost::uuids::random_generator()();
    session->username = "bob";
    session->start_time = std::chrono::system_clock::now();

    svc.store_session_data(addr, session);

    auto result = svc.get_session_data(addr);
    REQUIRE(result != nullptr);
    CHECK(result->account_id == session->account_id);
    CHECK(result->username == "bob");
}

TEST_CASE("test_get_session_data_returns_nullptr_for_unknown", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    auto result = svc.get_session_data("unknown:1234");
    CHECK(result == nullptr);
}

TEST_CASE("test_update_session_bytes", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:5003");

    auto session = std::make_shared<session_data>();
    session->account_id = boost::uuids::random_generator()();
    session->start_time = std::chrono::system_clock::now();
    svc.store_session_data(addr, session);

    svc.update_session_bytes(addr, 1024, 2048);

    auto result = svc.get_session_data(addr);
    REQUIRE(result != nullptr);
    CHECK(result->bytes_sent == 1024);
    CHECK(result->bytes_received == 2048);
}

TEST_CASE("test_remove_session_returns_and_removes", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:5004");
    const auto account_id = boost::uuids::random_generator()();

    svc.store_session(addr, {.account_id = account_id, .username = "carol"});
    CHECK(svc.is_authenticated(addr));

    auto removed = svc.remove_session(addr);
    REQUIRE(removed != nullptr);
    CHECK(removed->account_id == account_id);
    CHECK(!svc.is_authenticated(addr));
}

TEST_CASE("test_remove_session_returns_nullptr_for_unknown", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    auto removed = svc.remove_session("unknown:1234");
    CHECK(removed == nullptr);
}

TEST_CASE("test_clear_all_sessions_returns_all_and_empties", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const auto id1 = boost::uuids::random_generator()();
    const auto id2 = boost::uuids::random_generator()();

    svc.store_session("addr1:5000", {.account_id = id1});
    svc.store_session("addr2:5000", {.account_id = id2});

    auto all = svc.clear_all_sessions();
    CHECK(all.size() == 2);
    CHECK(!svc.is_authenticated("addr1:5000"));
    CHECK(!svc.is_authenticated("addr2:5000"));
}

TEST_CASE("test_get_all_sessions_returns_stored", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;

    svc.store_session("a:1", {.account_id = boost::uuids::random_generator()()});
    svc.store_session("b:2", {.account_id = boost::uuids::random_generator()()});
    svc.store_session("c:3", {.account_id = boost::uuids::random_generator()()});

    auto all = svc.get_all_sessions();
    CHECK(all.size() == 3);
}

TEST_CASE("test_store_and_get_client_info_roundtrip", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:6000");

    client_info ci{
        .client_identifier = "OreStudio-Qt",
        .client_version_major = 1,
        .client_version_minor = 2
    };
    svc.store_client_info(addr, ci);

    auto result = svc.get_client_info(addr);
    REQUIRE(result.has_value());
    CHECK(result->client_identifier == "OreStudio-Qt");
    CHECK(result->client_version_major == 1);
    CHECK(result->client_version_minor == 2);
}

TEST_CASE("test_get_client_info_returns_nullopt_for_unknown", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    auto result = svc.get_client_info("unknown:1234");
    CHECK(!result.has_value());
}

TEST_CASE("test_remove_client_info_cleans_up", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:6001");

    svc.store_client_info(addr, {.client_identifier = "test"});
    REQUIRE(svc.get_client_info(addr).has_value());

    svc.remove_client_info(addr);
    CHECK(!svc.get_client_info(addr).has_value());
}

TEST_CASE("test_authorize_request_allows_unauthenticated_types", tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:7000");
    // No session stored - not authenticated

    // These message types should be allowed without authentication
    CHECK(svc.authorize_request(message_type::ping, addr).has_value());
    CHECK(svc.authorize_request(message_type::pong, addr).has_value());
    CHECK(svc.authorize_request(message_type::handshake_request, addr).has_value());
    CHECK(svc.authorize_request(message_type::handshake_ack, addr).has_value());
    CHECK(svc.authorize_request(message_type::login_request, addr).has_value());
    CHECK(svc.authorize_request(message_type::signup_request, addr).has_value());
    CHECK(svc.authorize_request(
        message_type::create_initial_admin_request, addr).has_value());
    CHECK(svc.authorize_request(
        message_type::bootstrap_status_request, addr).has_value());

    BOOST_LOG_SEV(lg, debug) << "All unauthenticated types allowed";
}

TEST_CASE("test_authorize_request_rejects_authenticated_types_without_session",
    tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:7001");
    // No session stored

    auto result = svc.authorize_request(
        message_type::get_currencies_request, addr);
    CHECK(!result.has_value());
    CHECK(result.error() == error_code::authentication_failed);

    result = svc.authorize_request(
        message_type::get_accounts_request, addr);
    CHECK(!result.has_value());
    CHECK(result.error() == error_code::authentication_failed);
}

TEST_CASE("test_authorize_request_allows_authenticated_types_with_session",
    tags) {
    auto lg = make_logger(test_suite);

    auth_session_service svc;
    const std::string addr("127.0.0.1:7002");

    svc.store_session(addr,
        {.account_id = boost::uuids::random_generator()(), .username = "admin"});

    auto result = svc.authorize_request(
        message_type::get_currencies_request, addr);
    CHECK(result.has_value());

    result = svc.authorize_request(
        message_type::get_accounts_request, addr);
    CHECK(result.has_value());
}
