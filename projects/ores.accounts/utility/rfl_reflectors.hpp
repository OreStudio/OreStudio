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
#ifndef ORES_ACCOUNTS_UTILITY_RFL_REFLECTORS_HPP
#define ORES_ACCOUNTS_UTILITY_RFL_REFLECTORS_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <chrono>
#include <sstream>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/asio/ip/address.hpp>
#include <rfl.hpp>

namespace rfl {

/**
 * @brief Custom reflector for boost::uuids::uuid.
 *
 * Serializes UUID as string representation.
 */
template <>
struct Reflector<boost::uuids::uuid> {
    using ReflType = std::string;

    static boost::uuids::uuid to(const ReflType& str) {
        return boost::lexical_cast<boost::uuids::uuid>(str);
    }

    static ReflType from(const boost::uuids::uuid& v) {
        return boost::lexical_cast<std::string>(v);
    }
};

/**
 * @brief Custom reflector for std::chrono::system_clock::time_point.
 *
 * @details Serializes to and from an ISO 8601 compliant UTC string.
 * This is the idiomatic C++23 approach, using std::format and
 * std::chrono::parse for type-safe and portable operations.
 *
 * Serialized format: "YYYY-MM-DD HH:MM:SS" (assumed to be UTC)
 * Example: "2023-10-27 14:45:30"
 */
template <>
struct Reflector<std::chrono::system_clock::time_point> {
    using ReflType = std::string;

    /**
     * @brief Parses a string into a time_point.
     * @details Uses std::chrono::from_stream (C++20) to safely parse the
     * string format into a std::chrono::sys_timepoint.
     * @param str The string to parse.
     * @return A system_clock::time_point representing the parsed time.
     * @throws std::runtime_error if the string cannot be parsed.
     */
    static std::chrono::system_clock::time_point to(const ReflType& str) {
        std::istringstream ss(str);
        std::chrono::sys_seconds tp; // Use sys_seconds for second precision

        // The format string must match the input string.
        // We parse into a sys_time, which is implicitly UTC.
        ss >> std::chrono::parse("%F %T", tp);

        if (ss.fail()) {
            throw std::runtime_error("Failed to parse time_point from string: '" + str + "'");
        }

        // Convert sys_seconds back to system_clock::time_point
        return {tp };
    }

    /**
     * @brief Formats a time_point into a string.
     * @details Uses std::format (C++23) to create a clean, unambiguous
     * UTC string representation. This avoids all C-style time functions.
     * @param v The time_point to format.
     * @return A string representation of the time_point.
     */
    static ReflType from(const std::chrono::system_clock::time_point& v) {
        // The time_point is already in the "system clock" which is UTC.
        // We format it directly. The format specifiers are:
        // %F -> YYYY-MM-DD
        // %T -> HH:MM:SS
        // We add 'Z' to explicitly mark it as UTC, which is best practice.
        return std::format("{:%F %T}Z", v);
    }
};
/**
 * @brief Custom reflector for boost::asio::ip::address.
 *
 * Serializes IP address as string representation (supports both IPv4 and IPv6).
 */
template <>
struct Reflector<boost::asio::ip::address> {
    using ReflType = std::string;

    static boost::asio::ip::address to(const ReflType& str) {
        return boost::asio::ip::make_address(str);
    }

    static ReflType from(const boost::asio::ip::address& v) {
        return v.to_string();
    }
};

}

#endif
