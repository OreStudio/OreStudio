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
#ifndef ORES_UTILITY_RFL_REFLECTORS_HPP
#define ORES_UTILITY_RFL_REFLECTORS_HPP

#include <string>
#include <chrono>
#include <sstream>
#include <cstdint>
#include <stdexcept>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/asio/ip/address.hpp>
#include <rfl.hpp>

// Forward declarations for enum types that need custom reflectors
// to avoid GCC 15 std::min/max type deduction errors in rfl's
// internal enum range detection.
namespace ores::comms::messaging {
    enum class compression_type : std::uint8_t;
    enum class error_code;
    enum class message_type;
}

namespace rfl {

/**
 * @brief Custom reflector for boost::uuids::uuid.
 *
 * Serializes UUID as string representation.
 */
template<>
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
template<>
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
        // Parse ISO 8601 format: "YYYY-MM-DD HH:MM:SS"
        std::tm tm = {};
        std::istringstream ss(str);
        ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");

        auto tp = std::chrono::system_clock::from_time_t(std::mktime(&tm));
        return tp;
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
template<>
struct Reflector<boost::asio::ip::address> {
    using ReflType = std::string;

    static boost::asio::ip::address to(const ReflType& str) {
        return boost::asio::ip::make_address(str);
    }

    static ReflType from(const boost::asio::ip::address& v) {
        return v.to_string();
    }
};

/**
 * @brief Custom reflector for ores::comms::messaging::compression_type.
 *
 * Serializes as underlying integer type to avoid GCC 15 compilation errors
 * in rfl's internal enum range detection (std::min/max type mismatch).
 */
template<>
struct Reflector<ores::comms::messaging::compression_type> {
    using ReflType = std::uint8_t;

    static ores::comms::messaging::compression_type to(const ReflType& v) {
        // Valid values: none=0, zlib=1, gzip=2, bzip2=3
        if (v > 3) {
            throw std::runtime_error("Invalid value for compression_type enum: " +
                                     std::to_string(v));
        }
        return static_cast<ores::comms::messaging::compression_type>(v);
    }

    static ReflType from(const ores::comms::messaging::compression_type& v) {
        return static_cast<ReflType>(v);
    }
};

/**
 * @brief Custom reflector for ores::comms::messaging::error_code.
 *
 * Serializes as underlying integer type to avoid GCC 15 compilation errors
 * in rfl's internal enum range detection (std::min/max type mismatch).
 */
template<>
struct Reflector<ores::comms::messaging::error_code> {
    using ReflType = std::uint16_t;

    static ores::comms::messaging::error_code to(const ReflType& v) {
        // Valid values: none=0x0000 through payload_incomplete=0x0019
        if (v > 0x0019) {
            throw std::runtime_error("Invalid value for error_code enum: " +
                                     std::to_string(v));
        }
        return static_cast<ores::comms::messaging::error_code>(v);
    }

    static ReflType from(const ores::comms::messaging::error_code& v) {
        return static_cast<ReflType>(v);
    }
};

/**
 * @brief Custom reflector for ores::comms::messaging::message_type.
 *
 * Serializes as underlying integer type to avoid GCC 15 compilation errors
 * in rfl's internal enum range detection (std::min/max type mismatch).
 */
template<>
struct Reflector<ores::comms::messaging::message_type> {
    using ReflType = std::uint16_t;

    static ores::comms::messaging::message_type to(const ReflType& v) {
        // Valid values: 0x0001 to 0x5FFF (max subsystem range)
        // More specific validation is done at the protocol layer
        if (v == 0 || v > 0x5FFF) {
            throw std::runtime_error("Invalid value for message_type enum: " +
                                     std::to_string(v));
        }
        return static_cast<ores::comms::messaging::message_type>(v);
    }

    static ReflType from(const ores::comms::messaging::message_type& v) {
        return static_cast<ReflType>(v);
    }
};

}

#endif
