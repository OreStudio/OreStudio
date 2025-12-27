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
#include "ores.platform/net/network_info.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[net]");

}

using namespace ores::platform::net;
using namespace ores::telemetry::log;

TEST_CASE("get_hostname_returns_non_empty_string", tags) {
    auto lg(make_logger(test_suite));

    const auto hostname = get_hostname();

    BOOST_LOG_SEV(lg, info) << "Hostname: " << hostname;

    REQUIRE_FALSE(hostname.empty());
    REQUIRE(hostname != "unknown");
}

TEST_CASE("get_hostname_is_stable", tags) {
    const auto hostname1 = get_hostname();
    const auto hostname2 = get_hostname();

    REQUIRE(hostname1 == hostname2);
}

TEST_CASE("get_primary_mac_address_format", tags) {
    auto lg(make_logger(test_suite));

    const auto mac = get_primary_mac_address();

    if (mac.has_value()) {
        BOOST_LOG_SEV(lg, info) << "Primary MAC: " << mac.value();

        // MAC should be 17 chars: "xx:xx:xx:xx:xx:xx"
        REQUIRE(mac->size() == 17);

        // Should have colons at positions 2, 5, 8, 11, 14
        REQUIRE((*mac)[2] == ':');
        REQUIRE((*mac)[5] == ':');
        REQUIRE((*mac)[8] == ':');
        REQUIRE((*mac)[11] == ':');
        REQUIRE((*mac)[14] == ':');
    } else {
        BOOST_LOG_SEV(lg, warn) << "No MAC address available (may be in container)";
    }
}

TEST_CASE("get_primary_mac_address_is_stable", tags) {
    const auto mac1 = get_primary_mac_address();
    const auto mac2 = get_primary_mac_address();

    REQUIRE(mac1 == mac2);
}

TEST_CASE("get_primary_mac_address_bytes_length", tags) {
    const auto mac_bytes = get_primary_mac_address_bytes();

    if (mac_bytes.has_value()) {
        // Raw MAC should be 6 bytes
        REQUIRE(mac_bytes->size() == 6);
    }
}

TEST_CASE("derive_machine_id_is_non_empty", tags) {
    auto lg(make_logger(test_suite));

    const auto machine_id = derive_machine_id();

    BOOST_LOG_SEV(lg, info) << "Machine ID: " << machine_id;

    REQUIRE_FALSE(machine_id.empty());
}

TEST_CASE("derive_machine_id_is_stable", tags) {
    const auto id1 = derive_machine_id();
    const auto id2 = derive_machine_id();

    REQUIRE(id1 == id2);
}

TEST_CASE("derive_machine_id_is_hex", tags) {
    const auto machine_id = derive_machine_id();

    // Should be 16 hex chars
    REQUIRE(machine_id.size() == 16);

    for (const char c : machine_id) {
        const bool is_hex = (c >= '0' && c <= '9') ||
                            (c >= 'a' && c <= 'f') ||
                            (c >= 'A' && c <= 'F');
        REQUIRE(is_hex);
    }
}

TEST_CASE("derive_machine_id_hash_is_stable", tags) {
    const auto hash1 = derive_machine_id_hash();
    const auto hash2 = derive_machine_id_hash();

    REQUIRE(hash1 == hash2);
}

TEST_CASE("get_process_id_is_positive", tags) {
    auto lg(make_logger(test_suite));

    const auto pid = get_process_id();

    BOOST_LOG_SEV(lg, info) << "Process ID: " << pid;

    REQUIRE(pid > 0);
}

TEST_CASE("get_process_id_is_stable", tags) {
    const auto pid1 = get_process_id();
    const auto pid2 = get_process_id();

    REQUIRE(pid1 == pid2);
}
