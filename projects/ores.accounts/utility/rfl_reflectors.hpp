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
#include <iomanip>
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
 * Serializes time_point as ISO 8601 string representation.
 */
template <>
struct Reflector<std::chrono::system_clock::time_point> {
    using ReflType = std::string;

    static std::chrono::system_clock::time_point to(const ReflType& str) {
        // Parse ISO 8601 format: "YYYY-MM-DD HH:MM:SS"
        std::tm tm = {};
        std::istringstream ss(str);
        ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");

        auto tp = std::chrono::system_clock::from_time_t(std::mktime(&tm));
        return tp;
    }

    static ReflType from(const std::chrono::system_clock::time_point& v) {
        // Format as ISO 8601: "YYYY-MM-DD HH:MM:SS"
        auto tt = std::chrono::system_clock::to_time_t(v);
        std::tm tm = *std::localtime(&tt);

        std::ostringstream ss;
        ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S");
        return ss.str();
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
