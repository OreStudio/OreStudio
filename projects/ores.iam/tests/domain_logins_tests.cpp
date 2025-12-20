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
#include "ores.iam/domain/login_info.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/domain/login_info_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/login_info_table.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::login_info;
using namespace ores::utility::log;

TEST_CASE("create_login_info_with_ipv4_addresses", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address("192.168.1.100");
    sut.last_attempt_ip = boost::asio::ip::make_address("192.168.1.101");
    sut.failed_logins = 0;
    sut.locked = false;
    sut.last_login = std::chrono::system_clock::now();
    sut.online = true;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.failed_logins == 0);
    CHECK(sut.locked == false);
    CHECK(sut.online == true);
    CHECK(sut.last_ip.to_string() == "192.168.1.100");
    CHECK(sut.last_attempt_ip.to_string() == "192.168.1.101");
}

TEST_CASE("create_login_info_with_ipv6_addresses", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address("2001:0db8:85a3:0000:0000:8a2e:0370:7334");
    sut.last_attempt_ip = boost::asio::ip::make_address("::1");
    sut.failed_logins = 0;
    sut.locked = false;
    sut.last_login = std::chrono::system_clock::now();
    sut.online = false;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.failed_logins == 0);
    CHECK(sut.locked == false);
    CHECK(sut.online == false);
    CHECK(sut.last_attempt_ip.to_string() == "::1");
}

TEST_CASE("create_locked_account_login_info", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address("10.0.0.50");
    sut.last_attempt_ip = boost::asio::ip::make_address("203.0.113.42");
    sut.failed_logins = 5;
    sut.locked = true;
    sut.last_login = std::chrono::system_clock::now() - std::chrono::hours(24);
    sut.online = false;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.failed_logins == 5);
    CHECK(sut.locked == true);
    CHECK(sut.online == false);
    CHECK(sut.last_attempt_ip.to_string() == "203.0.113.42");
}

TEST_CASE("login_info_with_failed_attempts", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address("172.16.0.1");
    sut.last_attempt_ip = boost::asio::ip::make_address("172.16.0.1");
    sut.failed_logins = 3;
    sut.locked = false;
    sut.last_login = std::chrono::system_clock::now() - std::chrono::minutes(30);
    sut.online = false;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.failed_logins == 3);
    CHECK(sut.locked == false);
    CHECK(sut.last_ip.to_string() == "172.16.0.1");
}

TEST_CASE("login_info_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto account_uuid = uuid_gen("123e4567-e89b-12d3-a456-426614174000");

    login_info sut;
    sut.account_id = account_uuid;
    sut.last_ip = boost::asio::ip::make_address("198.51.100.42");
    sut.last_attempt_ip = boost::asio::ip::make_address("198.51.100.43");
    sut.failed_logins = 1;
    sut.locked = false;
    sut.last_login = std::chrono::system_clock::now();
    sut.online = true;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("123e4567-e89b-12d3-a456-426614174000") != std::string::npos);
    CHECK(json_output.find("198.51.100.42") != std::string::npos);
}

TEST_CASE("login_info_with_different_ip_versions", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address("192.0.2.1");
    sut.last_attempt_ip = boost::asio::ip::make_address("2001:db8::1");
    sut.failed_logins = 0;
    sut.locked = false;
    sut.last_login = std::chrono::system_clock::now();
    sut.online = true;
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.last_ip.is_v4());
    CHECK(sut.last_attempt_ip.is_v6());
    CHECK(sut.last_ip.to_string() == "192.0.2.1");
    CHECK(sut.last_attempt_ip.to_string() == "2001:db8::1");
}

TEST_CASE("create_login_info_with_faker", tags) {
    auto lg(make_logger(test_suite));

    login_info sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.last_ip = boost::asio::ip::make_address(faker::internet::ipv4());
    sut.last_attempt_ip = boost::asio::ip::make_address(faker::internet::ipv4());
    sut.failed_logins = faker::number::integer(0, 10);
    sut.locked = faker::datatype::boolean();
    sut.last_login = std::chrono::system_clock::now() -
        std::chrono::hours(faker::number::integer(0, 168));
    sut.online = faker::datatype::boolean();
    BOOST_LOG_SEV(lg, info) << "Login info: " << sut;

    CHECK(sut.failed_logins >= 0);
    CHECK(sut.failed_logins <= 10);
}

TEST_CASE("create_multiple_random_login_infos", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        login_info sut;
        sut.account_id = boost::uuids::random_generator()();

        const bool use_ipv6 = faker::datatype::boolean();
        if (use_ipv6) {
            sut.last_ip = boost::asio::ip::make_address(faker::internet::ipv6());
            sut.last_attempt_ip = boost::asio::ip::make_address(faker::internet::ipv6());
        } else {
            sut.last_ip = boost::asio::ip::make_address(faker::internet::ipv4());
            sut.last_attempt_ip = boost::asio::ip::make_address(faker::internet::ipv4());
        }

        sut.failed_logins = faker::number::integer(0, 5);
        sut.locked = sut.failed_logins >= 3;
        sut.last_login = std::chrono::system_clock::now() -
            std::chrono::minutes(faker::number::integer(0, 1440));
        sut.online = faker::datatype::boolean();
        BOOST_LOG_SEV(lg, info) << "Login info " << i << ":" << sut;

        CHECK(sut.failed_logins >= 0);
        if (sut.locked) {
            CHECK(sut.failed_logins >= 3);
        }
    }
}

TEST_CASE("login_info_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    login_info li;
    li.account_id = boost::uuids::random_generator()();
    li.last_ip = boost::asio::ip::make_address("192.168.1.100");
    li.last_attempt_ip = boost::asio::ip::make_address("192.168.1.101");
    li.failed_logins = 2;
    li.locked = false;
    li.last_login = std::chrono::system_clock::now();
    li.online = true;

    std::vector<login_info> logins = {li};
    auto table = convert_to_table(logins);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("192.168.1.100") != std::string::npos);
    CHECK(table.find("192.168.1.101") != std::string::npos);
}

TEST_CASE("login_info_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<login_info> logins;
    for (int i = 0; i < 3; ++i) {
        login_info li;
        li.account_id = boost::uuids::random_generator()();
        li.last_ip = boost::asio::ip::make_address("10.0.0." + std::to_string(i + 1));
        li.last_attempt_ip = boost::asio::ip::make_address("10.0.0." + std::to_string(i + 10));
        li.failed_logins = i;
        li.locked = (i >= 2);
        li.last_login = std::chrono::system_clock::now();
        li.online = (i == 0);
        logins.push_back(li);
    }

    auto table = convert_to_table(logins);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("10.0.0.1") != std::string::npos);
    CHECK(table.find("10.0.0.2") != std::string::npos);
    CHECK(table.find("10.0.0.3") != std::string::npos);
}

TEST_CASE("login_info_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<login_info> logins;
    for (int i = 0; i < 5; ++i) {
        login_info li;
        li.account_id = boost::uuids::random_generator()();
        li.last_ip = boost::asio::ip::make_address(faker::internet::ipv4());
        li.last_attempt_ip = boost::asio::ip::make_address(faker::internet::ipv4());
        li.failed_logins = faker::number::integer(0, 5);
        li.locked = faker::datatype::boolean();
        li.last_login = std::chrono::system_clock::now();
        li.online = faker::datatype::boolean();
        logins.push_back(li);
    }

    auto table = convert_to_table(logins);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    // Verify all IP addresses appear in table
    for (const auto& li : logins) {
        CHECK(table.find(li.last_ip.to_string()) != std::string::npos);
    }
}
