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
#ifndef ORES_PLATFORM_NET_NETWORK_INFO_HPP
#define ORES_PLATFORM_NET_NETWORK_INFO_HPP

#include <cstdint>
#include <optional>
#include <string>

namespace ores::platform::net {

/**
 * @brief Gets the hostname of the current machine.
 *
 * @return The hostname, or "unknown" if it cannot be determined.
 */
std::string get_hostname();

/**
 * @brief Gets the primary MAC address of the machine.
 *
 * Returns the lexicographically first non-loopback MAC address to ensure
 * stable ordering across reboots. The MAC address is formatted as a
 * colon-separated hex string (e.g., "00:1a:2b:3c:4d:5e").
 *
 * @return The MAC address if available, empty optional otherwise.
 */
std::optional<std::string> get_primary_mac_address();

/**
 * @brief Gets the primary MAC address as raw bytes.
 *
 * Returns the lexicographically first non-loopback MAC address as a
 * 6-byte string. This is useful for hashing without formatting overhead.
 *
 * @return The MAC address bytes if available, empty optional otherwise.
 */
std::optional<std::string> get_primary_mac_address_bytes();

/**
 * @brief Derives a stable machine identifier from hardware properties.
 *
 * Combines hostname and MAC address to create a unique identifier that
 * remains stable across reboots. The result is a hex-encoded hash.
 *
 * @return A hex string representing the machine ID.
 */
std::string derive_machine_id();

/**
 * @brief Derives a 16-bit machine identifier suitable for embedding in IDs.
 *
 * @return A 16-bit hash of the machine identity.
 */
std::uint16_t derive_machine_id_hash();

/**
 * @brief Gets the current process ID.
 *
 * @return The process ID, or 0 if it cannot be determined.
 */
std::int64_t get_process_id();

}

#endif
