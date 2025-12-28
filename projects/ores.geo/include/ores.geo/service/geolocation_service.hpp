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
#ifndef ORES_GEO_SERVICE_GEOLOCATION_SERVICE_HPP
#define ORES_GEO_SERVICE_GEOLOCATION_SERVICE_HPP

#include <string>
#include <optional>
#include <expected>
#include <boost/asio/ip/address.hpp>
#include "ores.database/domain/context.hpp"

namespace ores::geo::service {

/**
 * @brief Result of a geolocation lookup.
 */
struct geolocation_result {
    std::string country_code;
    std::string city;
    std::optional<double> latitude;
    std::optional<double> longitude;
};

/**
 * @brief Error codes for geolocation lookup failures.
 */
enum class geolocation_error {
    database_not_available,
    address_not_found,
    lookup_failed,
    invalid_address
};

/**
 * @brief Service for looking up geographic location from IP addresses.
 *
 * Uses PostgreSQL geoip tables for lookups. The tables must be populated
 * with MaxMind GeoLite2-City CSV data using the geolocation_import.sql script.
 *
 * Thread-safety: All public methods are thread-safe.
 */
class geolocation_service {
public:
    /**
     * @brief Construct a geolocation service with database context.
     *
     * @param ctx Database context for PostgreSQL queries
     */
    explicit geolocation_service(database::context ctx);

    /**
     * @brief Look up geolocation for an IP address.
     *
     * @param ip The IP address to look up
     * @return Geolocation result or error
     */
    [[nodiscard]] std::expected<geolocation_result, geolocation_error>
    lookup(const boost::asio::ip::address& ip) const;

    /**
     * @brief Look up geolocation for an IP address string.
     *
     * @param ip_string The IP address string to look up
     * @return Geolocation result or error
     */
    [[nodiscard]] std::expected<geolocation_result, geolocation_error>
    lookup(const std::string& ip_string) const;

private:
    database::context ctx_;
};

}

#endif
