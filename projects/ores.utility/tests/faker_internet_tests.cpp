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
#include "ores.utility/faker/internet.hpp"

#include <set>
#include <regex>
#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[faker]");

}

using ores::utility::faker::internet;
using namespace ores::telemetry::log;

TEST_CASE("internet_endpoint_format", tags) {
    auto lg(make_logger(test_suite));

    auto endpoint = internet::endpoint();

    BOOST_LOG_SEV(lg, info) << "Generated endpoint: " << endpoint;

    // Should contain a colon separating IP and port
    CHECK(endpoint.find(':') != std::string::npos);

    // Should match IPv4:port format
    std::regex endpoint_regex(R"(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}:\d+)");
    CHECK(std::regex_match(endpoint, endpoint_regex));
}

TEST_CASE("internet_endpoint_port_range", tags) {
    auto lg(make_logger(test_suite));

    auto endpoint = internet::endpoint();

    BOOST_LOG_SEV(lg, info) << "Checking port range for: " << endpoint;

    // Extract port from endpoint
    auto colon_pos = endpoint.find_last_of(':');
    REQUIRE(colon_pos != std::string::npos);

    std::string port_str = endpoint.substr(colon_pos + 1);
    int port = std::stoi(port_str);

    BOOST_LOG_SEV(lg, info) << "Port: " << port;

    // Port should be in range 1025-65535 (non-privileged ports)
    CHECK(port >= 1025);
    CHECK(port <= 65535);
}

TEST_CASE("internet_endpoint_unique_values", tags) {
    auto lg(make_logger(test_suite));

    std::set<std::string> endpoints;
    const size_t count = 50;

    for (size_t i = 0; i < count; ++i) {
        endpoints.insert(internet::endpoint());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " endpoints, "
                            << endpoints.size() << " unique";

    // Most should be unique (allowing for small collision probability)
    CHECK(endpoints.size() >= count - 5);
}

TEST_CASE("internet_ipv4_is_valid", tags) {
    auto lg(make_logger(test_suite));

    auto ip = internet::ipv4();

    BOOST_LOG_SEV(lg, info) << "Generated IPv4: " << ip.to_string();

    // Should be a valid IPv4 address
    CHECK(ip.is_v4());
    CHECK(!ip.is_v6());
}

TEST_CASE("internet_ipv4_to_string_format", tags) {
    auto lg(make_logger(test_suite));

    auto ip = internet::ipv4();
    auto ip_str = ip.to_string();

    BOOST_LOG_SEV(lg, info) << "IPv4 string: " << ip_str;

    // Should match IPv4 format
    std::regex ipv4_regex(R"(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})");
    CHECK(std::regex_match(ip_str, ipv4_regex));
}

TEST_CASE("internet_ipv4_octets_in_range", tags) {
    auto lg(make_logger(test_suite));

    auto ip = internet::ipv4();
    auto ip_str = ip.to_string();

    BOOST_LOG_SEV(lg, info) << "Validating octets for: " << ip_str;

    // Parse octets and verify range
    std::regex octet_regex(R"((\d+)\.(\d+)\.(\d+)\.(\d+))");
    std::smatch matches;

    REQUIRE(std::regex_match(ip_str, matches, octet_regex));
    REQUIRE(matches.size() == 5); // Full match + 4 octets

    for (size_t i = 1; i <= 4; ++i) {
        int octet = std::stoi(matches[i].str());
        BOOST_LOG_SEV(lg, info) << "Octet " << i << ": " << octet;
        CHECK(octet >= 0);
        CHECK(octet <= 255);
    }
}

TEST_CASE("internet_ipv4_unique_values", tags) {
    auto lg(make_logger(test_suite));

    std::set<std::string> ips;
    const size_t count = 50;

    for (size_t i = 0; i < count; ++i) {
        ips.insert(internet::ipv4().to_string());
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " IPs, "
                            << ips.size() << " unique";

    // Most should be unique (allowing for small collision probability)
    CHECK(ips.size() >= count - 5);
}

TEST_CASE("internet_ipv4_can_create_address", tags) {
    auto lg(make_logger(test_suite));

    auto ip = internet::ipv4();

    BOOST_LOG_SEV(lg, info) << "IPv4 address: " << ip;

    // Should be able to use as boost::asio address
    CHECK_NOTHROW(ip.to_string());
    CHECK(ip.is_v4());
}

TEST_CASE("internet_endpoint_multiple_calls", tags) {
    auto lg(make_logger(test_suite));

    auto endpoint1 = internet::endpoint();
    auto endpoint2 = internet::endpoint();
    auto endpoint3 = internet::endpoint();

    BOOST_LOG_SEV(lg, info) << "Endpoint 1: " << endpoint1;
    BOOST_LOG_SEV(lg, info) << "Endpoint 2: " << endpoint2;
    BOOST_LOG_SEV(lg, info) << "Endpoint 3: " << endpoint3;

    // Very likely to be different (not guaranteed but highly probable)
    // At least one pair should be different
    CHECK((endpoint1 != endpoint2 || endpoint2 != endpoint3 || endpoint1 != endpoint3));
}

TEST_CASE("internet_ipv4_not_unspecified", tags) {
    auto lg(make_logger(test_suite));

    auto ip = internet::ipv4();

    BOOST_LOG_SEV(lg, info) << "Checking IP is not 0.0.0.0: " << ip;

    // Should not generate 0.0.0.0
    CHECK(!ip.is_unspecified());
}
