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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/net/network_info.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iomanip>
#include <sstream>
#include <string>
#include <unistd.h>
#include <vector>

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[net]");

#if defined(__linux__)
/**
 * @brief Returns the non-loopback MAC addresses the kernel reports, sorted.
 *
 * This is the same adapter state the implementation reads, but through a
 * different API, so comparing the two is a cross-check rather than a
 * restatement of the subject.
 */
std::vector<std::string> kernel_mac_addresses() {
    std::vector<std::string> macs;

    const std::filesystem::path net_dir("/sys/class/net");
    if (!std::filesystem::exists(net_dir))
        return macs;

    for (const auto& entry : std::filesystem::directory_iterator(net_dir)) {
        const auto iface = entry.path().filename().string();
        if (iface == "lo")
            continue;

        std::ifstream address_file(entry.path() / "address");
        if (!address_file)
            continue;

        std::string address;
        std::getline(address_file, address);
        if (address.size() == 17 && address != "00:00:00:00:00:00")
            macs.push_back(address);
    }

    std::sort(macs.begin(), macs.end());
    return macs;
}

/** @brief Reverses the colon-separated hex formatting back to raw bytes. */
std::string mac_bytes_from_string(const std::string& formatted) {
    std::string bytes;
    bytes.reserve(6);
    for (std::size_t i = 0; i < 6; ++i)
        bytes.push_back(static_cast<char>(std::stoi(formatted.substr(i * 3, 2), nullptr, 16)));
    return bytes;
}
#endif

}

using namespace ores::platform::net;
using namespace ores::logging;

TEST_CASE("get_hostname_matches_the_operating_system", tags) {
    auto lg(make_logger(test_suite));

    char os_hostname[256] = {};
    REQUIRE(::gethostname(os_hostname, sizeof(os_hostname)) == 0);

    const auto hostname = get_hostname();
    BOOST_LOG_SEV(lg, info) << "Hostname: " << hostname;

    CHECK(hostname == os_hostname);
}

#if defined(__linux__)
TEST_CASE("get_primary_mac_address_reports_the_first_kernel_address", tags) {
    auto lg(make_logger(test_suite));

    const auto kernel_macs = kernel_mac_addresses();
    if (kernel_macs.empty()) {
        BOOST_LOG_SEV(lg, warn) << "No non-loopback interface available";
        return;
    }

    const auto mac = get_primary_mac_address();
    REQUIRE(mac.has_value());
    BOOST_LOG_SEV(lg, info) << "Primary MAC: " << *mac;

    CHECK(*mac == kernel_macs.front());
}

TEST_CASE("get_primary_mac_address_bytes_are_the_first_kernel_address", tags) {
    const auto kernel_macs = kernel_mac_addresses();
    if (kernel_macs.empty())
        return;

    const auto formatted = get_primary_mac_address();
    REQUIRE(formatted.has_value());

    const auto expected = mac_bytes_from_string(kernel_macs.front());
    const auto bytes = get_primary_mac_address_bytes();
    REQUIRE(bytes.has_value());

    CHECK(*bytes == expected);
    CHECK(bytes->size() == 6);
}
#endif

TEST_CASE("derive_machine_id_is_the_hex_hash_of_hostname_and_formatted_mac", tags) {
    auto lg(make_logger(test_suite));

    const auto machine_id = derive_machine_id();
    BOOST_LOG_SEV(lg, info) << "Machine ID: " << machine_id;

    const std::string combined = get_hostname() + ":" + get_primary_mac_address().value_or("");
    const std::size_t expected_hash = std::hash<std::string>{}(combined);
    std::ostringstream expected;
    expected << std::hex << std::setfill('0') << std::setw(16) << expected_hash;

    CHECK(machine_id == expected.str());
}

TEST_CASE("derive_machine_id_hash_is_the_low_sixteen_bits_of_hostname_and_raw_mac", tags) {
    const auto combined = get_hostname() + get_primary_mac_address_bytes().value_or("");
    const std::size_t expected_hash = std::hash<std::string>{}(combined);

    CHECK(derive_machine_id_hash() == static_cast<std::uint16_t>(expected_hash & 0xFFFF));
}
