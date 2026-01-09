/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_UTILITY_VERSION_VERSION_HPP
#define ORES_UTILITY_VERSION_VERSION_HPP

#include <string>
#include <sstream>
#include <cstdint>

#define ORES_VERSION_MAJOR 0
#define ORES_VERSION_MINOR 0
#define ORES_VERSION_PATCH 8
#define ORES_VERSION "0.0.8"
#define ORES_BUILD_INFO "local d70ecd1b-dirty 2026/01/08 22:52:05"

namespace ores::utility::version {

/**
 * @brief Formats a startup message with version and build info.
 *
 * @param service_name Name of the service (e.g., "ORE Studio Web")
 * @return Formatted startup string
 */
inline std::string format_startup_message(const std::string& service_name) {
    std::ostringstream ss;
    ss << "Starting " << service_name << " v" << ORES_VERSION
       << " (" << ORES_BUILD_INFO << ")";
    return ss.str();
}

/**
 * @brief Formats a startup message with version, protocol, and build info.
 *
 * @param service_name Name of the service (e.g., "ORE Studio Service")
 * @param protocol_major Protocol major version
 * @param protocol_minor Protocol minor version
 * @return Formatted startup string
 */
inline std::string format_startup_message(const std::string& service_name,
    std::uint16_t protocol_major, std::uint16_t protocol_minor) {
    std::ostringstream ss;
    ss << "Starting " << service_name << " v" << ORES_VERSION
       << " Protocol " << protocol_major << "." << protocol_minor
       << " (" << ORES_BUILD_INFO << ")";
    return ss.str();
}

}

#endif
