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
#include <boost/test/unit_test.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.utility/test/logging.hpp"
#include "ores.accounts/domain/login_info.hpp"

namespace {

const std::string test_module("ores.accounts.tests");
const std::string test_suite("domain_login_info_tests");

}

using ores::accounts::domain::login_info;

BOOST_AUTO_TEST_SUITE(domain_login_info_tests)

BOOST_AUTO_TEST_CASE(create_login_info_with_ipv4_addresses) {
    SETUP_TEST_LOG_SOURCE_DEBUG("create_login_info_with_ipv4_addresses");

    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("192.168.1.100");
    login_info.last_attempt_ip = boost::asio::ip::make_address("192.168.1.101");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = true;

    BOOST_LOG_SEV(lg, debug) << "Created login_info with IPv4: " << login_info;

    BOOST_CHECK_EQUAL(login_info.failed_logins, 0);
    BOOST_CHECK_EQUAL(login_info.locked, false);
    BOOST_CHECK_EQUAL(login_info.online, true);
    BOOST_CHECK_EQUAL(login_info.last_ip.to_string(), "192.168.1.100");
    BOOST_CHECK_EQUAL(login_info.last_attempt_ip.to_string(), "192.168.1.101");
}

BOOST_AUTO_TEST_CASE(create_login_info_with_ipv6_addresses) {
    SETUP_TEST_LOG_SOURCE_DEBUG("create_login_info_with_ipv6_addresses");

    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("2001:0db8:85a3:0000:0000:8a2e:0370:7334");
    login_info.last_attempt_ip = boost::asio::ip::make_address("::1");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = false;

    BOOST_LOG_SEV(lg, debug) << "Created login_info with IPv6: " << login_info;

    BOOST_CHECK_EQUAL(login_info.failed_logins, 0);
    BOOST_CHECK_EQUAL(login_info.locked, false);
    BOOST_CHECK_EQUAL(login_info.online, false);
    BOOST_CHECK_EQUAL(login_info.last_attempt_ip.to_string(), "::1");
}

BOOST_AUTO_TEST_CASE(create_locked_account_login_info) {
    SETUP_TEST_LOG_SOURCE_DEBUG("create_locked_account_login_info");

    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("10.0.0.50");
    login_info.last_attempt_ip = boost::asio::ip::make_address("203.0.113.42");
    login_info.failed_logins = 5;
    login_info.locked = true;
    login_info.last_login = std::chrono::system_clock::now() - std::chrono::hours(24);
    login_info.online = false;

    BOOST_LOG_SEV(lg, debug) << "Created locked account login_info: " << login_info;

    BOOST_CHECK_EQUAL(login_info.failed_logins, 5);
    BOOST_CHECK_EQUAL(login_info.locked, true);
    BOOST_CHECK_EQUAL(login_info.online, false);
    BOOST_CHECK_EQUAL(login_info.last_attempt_ip.to_string(), "203.0.113.42");
}

BOOST_AUTO_TEST_CASE(login_info_with_failed_attempts) {
    SETUP_TEST_LOG_SOURCE_DEBUG("login_info_with_failed_attempts");

    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("172.16.0.1");
    login_info.last_attempt_ip = boost::asio::ip::make_address("172.16.0.1");
    login_info.failed_logins = 3;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now() - std::chrono::minutes(30);
    login_info.online = false;

    BOOST_LOG_SEV(lg, debug) << "Login_Info with failed attempts: " << login_info;

    BOOST_CHECK_EQUAL(login_info.failed_logins, 3);
    BOOST_CHECK_EQUAL(login_info.locked, false);
    BOOST_CHECK_EQUAL(login_info.last_ip.to_string(), "172.16.0.1");
}

BOOST_AUTO_TEST_CASE(login_info_serialization_to_json) {
    SETUP_TEST_LOG_SOURCE_DEBUG("login_info_serialization_to_json");

    boost::uuids::string_generator uuid_gen;
    const auto account_uuid = uuid_gen("123e4567-e89b-12d3-a456-426614174000");

    login_info login_info;
    login_info.account_id = account_uuid;
    login_info.last_ip = boost::asio::ip::make_address("198.51.100.42");
    login_info.last_attempt_ip = boost::asio::ip::make_address("198.51.100.43");
    login_info.failed_logins = 1;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = true;

    BOOST_LOG_SEV(lg, debug) << "Login_Info before serialization: " << login_info;

    std::ostringstream oss;
    oss << login_info;
    const std::string json_output = oss.str();

    BOOST_LOG_SEV(lg, debug) << "Serialized JSON: " << json_output;

    BOOST_CHECK(!json_output.empty());
    BOOST_CHECK(json_output.find("123e4567-e89b-12d3-a456-426614174000") != std::string::npos);
    BOOST_CHECK(json_output.find("198.51.100.42") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(login_info_with_different_ip_versions) {
    SETUP_TEST_LOG_SOURCE_DEBUG("login_info_with_different_ip_versions");

    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("192.0.2.1");
    login_info.last_attempt_ip = boost::asio::ip::make_address("2001:db8::1");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = true;

    BOOST_LOG_SEV(lg, debug) << "Login_Info with mixed IP versions: " << login_info;

    BOOST_CHECK(login_info.last_ip.is_v4());
    BOOST_CHECK(login_info.last_attempt_ip.is_v6());
    BOOST_CHECK_EQUAL(login_info.last_ip.to_string(), "192.0.2.1");
    BOOST_CHECK_EQUAL(login_info.last_attempt_ip.to_string(), "2001:db8::1");
}

BOOST_AUTO_TEST_SUITE_END()
