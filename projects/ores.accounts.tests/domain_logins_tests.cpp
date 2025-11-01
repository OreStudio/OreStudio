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
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.accounts/domain/login_info.hpp"

using ores::accounts::domain::login_info;

TEST_CASE("create_login_info_with_ipv4_addresses", "[domain_login_info_tests]") {
    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("192.168.1.100");
    login_info.last_attempt_ip = boost::asio::ip::make_address("192.168.1.101");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = true;

    CHECK(login_info.failed_logins == 0);
    CHECK(login_info.locked == false);
    CHECK(login_info.online == true);
    CHECK(login_info.last_ip.to_string() == "192.168.1.100");
    CHECK(login_info.last_attempt_ip.to_string() == "192.168.1.101");
}

TEST_CASE("create_login_info_with_ipv6_addresses", "[domain_login_info_tests]") {
    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("2001:0db8:85a3:0000:0000:8a2e:0370:7334");
    login_info.last_attempt_ip = boost::asio::ip::make_address("::1");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = false;

    CHECK(login_info.failed_logins == 0);
    CHECK(login_info.locked == false);
    CHECK(login_info.online == false);
    CHECK(login_info.last_attempt_ip.to_string() == "::1");
}

TEST_CASE("create_locked_account_login_info", "[domain_login_info_tests]") {
    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("10.0.0.50");
    login_info.last_attempt_ip = boost::asio::ip::make_address("203.0.113.42");
    login_info.failed_logins = 5;
    login_info.locked = true;
    login_info.last_login = std::chrono::system_clock::now() - std::chrono::hours(24);
    login_info.online = false;

    CHECK(login_info.failed_logins == 5);
    CHECK(login_info.locked == true);
    CHECK(login_info.online == false);
    CHECK(login_info.last_attempt_ip.to_string() == "203.0.113.42");
}

TEST_CASE("login_info_with_failed_attempts", "[domain_login_info_tests]") {
    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("172.16.0.1");
    login_info.last_attempt_ip = boost::asio::ip::make_address("172.16.0.1");
    login_info.failed_logins = 3;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now() - std::chrono::minutes(30);
    login_info.online = false;

    CHECK(login_info.failed_logins == 3);
    CHECK(login_info.locked == false);
    CHECK(login_info.last_ip.to_string() == "172.16.0.1");
}

TEST_CASE("login_info_serialization_to_json", "[domain_login_info_tests]") {
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

    std::ostringstream oss;
    oss << login_info;
    const std::string json_output = oss.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("123e4567-e89b-12d3-a456-426614174000") != std::string::npos);
    CHECK(json_output.find("198.51.100.42") != std::string::npos);
}

TEST_CASE("login_info_with_different_ip_versions", "[domain_login_info_tests]") {
    login_info login_info;
    login_info.account_id = boost::uuids::random_generator()();
    login_info.last_ip = boost::asio::ip::make_address("192.0.2.1");
    login_info.last_attempt_ip = boost::asio::ip::make_address("2001:db8::1");
    login_info.failed_logins = 0;
    login_info.locked = false;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.online = true;

    CHECK(login_info.last_ip.is_v4());
    CHECK(login_info.last_attempt_ip.is_v6());
    CHECK(login_info.last_ip.to_string() == "192.0.2.1");
    CHECK(login_info.last_attempt_ip.to_string() == "2001:db8::1");
}
