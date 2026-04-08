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
#include <optional>
#include <sstream>
#include <cstdint>
#include <stdexcept>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/asio/ip/address.hpp>
#include <rfl.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

// Forward declarations for enum types that need custom reflectors
// to avoid GCC 15 std::min/max type deduction errors in rfl's
// internal enum range detection.
namespace ores::utility::serialization {
    enum class error_code : std::uint16_t;
}
namespace ores::comms::messaging {
    enum class compression_type : std::uint8_t;
    enum class message_type : std::uint16_t;
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
 * @brief Custom reflector for ores::utility::uuid::tenant_id.
 *
 * Serializes tenant_id as UUID string representation.
 */
template<>
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
template<>
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
template<>
struct Reflector<std::chrono::year_month_day> {
    using ReflType = std::string;

    static std::chrono::year_month_day to(const ReflType& str) {
        int y{}, m{}, d{};
        char sep1{}, sep2{};
        std::istringstream ss(str);
        ss.imbue(std::locale::classic());
        if (!(ss >> y >> sep1 >> m >> sep2 >> d) || sep1 != '-' || sep2 != '-' || !ss.eof())
            throw std::runtime_error("Invalid date format: " + str);
        auto ymd = std::chrono::year{y} /
                   std::chrono::month{static_cast<unsigned>(m)} /
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
 * @brief Custom reflector for ores::utility::serialization::error_code.
 *
 * Serializes as underlying integer type to avoid GCC 15 compilation errors
 * in rfl's internal enum range detection (std::min/max type mismatch).
 */
template<>
struct Reflector<ores::utility::serialization::error_code> {
    using ReflType = std::uint16_t;

    static ores::utility::serialization::error_code to(const ReflType& v) {
        // Valid values: none=0x0000 through payload_incomplete=0x0019
        if (v > 0x0019) {
            throw std::runtime_error("Invalid value for error_code enum: " +
                                     std::to_string(v));
        }
        return static_cast<ores::utility::serialization::error_code>(v);
    }

    static ReflType from(const ores::utility::serialization::error_code& v) {
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
