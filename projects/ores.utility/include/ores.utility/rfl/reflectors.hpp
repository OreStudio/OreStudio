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

#include "ores.utility/rfl/time_point_parser.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <rfl.hpp>
#include <sstream>
#include <stdexcept>
#include <string>

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
 * @brief Custom reflector for ores::utility::uuid::tenant_id.
 *
 * Serializes tenant_id as UUID string representation.
 */
template <>
struct Reflector<ores::utility::uuid::tenant_id> {
    using ReflType = std::string;

    static ores::utility::uuid::tenant_id to(const ReflType& str) {
        auto result = ores::utility::uuid::tenant_id::from_string(str);
        if (!result) {
            throw std::runtime_error("Invalid tenant_id: " + result.error());
        }
        return *result;
    }

    static ReflType from(const ores::utility::uuid::tenant_id& v) {
        return v.to_string();
    }
};

/**
 * @brief Custom reflector for std::optional<boost::uuids::uuid>.
 *
 * Serializes optional UUID as nullable string representation.
 * An empty optional serializes to null, a present value serializes as UUID string.
 */
template <>
struct Reflector<std::optional<boost::uuids::uuid>> {
    using ReflType = std::optional<std::string>;

    static std::optional<boost::uuids::uuid> to(const ReflType& str) {
        if (!str.has_value()) {
            return std::nullopt;
        }
        return boost::lexical_cast<boost::uuids::uuid>(str.value());
    }

    static ReflType from(const std::optional<boost::uuids::uuid>& v) {
        if (!v.has_value()) {
            return std::nullopt;
        }
        return boost::lexical_cast<std::string>(v.value());
    }
};

/**
 * @brief Custom reflector for std::chrono::year_month_day.
 *
 * Serializes to and from an ISO 8601 date string ("YYYY-MM-DD").
 */
template <>
struct Reflector<std::chrono::year_month_day> {
    using ReflType = std::string;

    static std::chrono::year_month_day to(const ReflType& str) {
        int y{}, m{}, d{};
        char sep1{}, sep2{};
        std::istringstream ss(str);
        ss.imbue(std::locale::classic());
        if (!(ss >> y >> sep1 >> m >> sep2 >> d) || sep1 != '-' || sep2 != '-' || !ss.eof())
            throw std::runtime_error("Invalid date format: " + str);
        auto ymd = std::chrono::year{y} / std::chrono::month{static_cast<unsigned>(m)} /
                   std::chrono::day{static_cast<unsigned>(d)};
        if (!ymd.ok())
            throw std::runtime_error("Invalid date value: " + str);
        return ymd;
    }

    static ReflType from(const std::chrono::year_month_day& v) {
        return std::format("{:%Y-%m-%d}", v);
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

/**
 * @brief Custom reflector for ores::utility::serialization::error_code.
 *
 * Serializes as underlying integer type to avoid GCC 15 compilation errors
 * in rfl's internal enum range detection (std::min/max type mismatch).
 */
template <>
struct Reflector<ores::utility::serialization::error_code> {
    using ReflType = std::uint16_t;

    static ores::utility::serialization::error_code to(const ReflType& v) {
        using code = ores::utility::serialization::error_code;
        // last_value is a sentinel, so limit_exceeded is the highest wire value.
        if (v > static_cast<std::uint16_t>(code::limit_exceeded)) {
            throw std::runtime_error("Invalid value for error_code enum: " + std::to_string(v));
        }
        return static_cast<code>(v);
    }

    static ReflType from(const ores::utility::serialization::error_code& v) {
        return static_cast<ReflType>(v);
    }
};

}

#endif
