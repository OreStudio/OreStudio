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
#ifndef ORES_UTILITY_GEO_GEOLOCATION_SERVICE_HPP
#define ORES_UTILITY_GEO_GEOLOCATION_SERVICE_HPP

#include <string>
#include <memory>
#include <optional>
#include <expected>
#include <boost/asio/ip/address.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace ores::utility::geo {

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
    database_not_loaded,
    address_not_found,
    lookup_failed,
    invalid_address
};

/**
 * @brief Service for looking up geographic location from IP addresses.
 *
 * Uses MaxMind GeoLite2-City database for lookups. The database file
 * must be provided at construction time and can be updated at runtime.
 *
 * Thread-safety: All public methods are thread-safe for read operations.
 * Database loading/reloading should be done before concurrent lookups.
 */
class geolocation_service {
private:
    inline static std::string_view logger_name =
        "ores.utility.geo.geolocation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a geolocation service without loading a database.
     *
     * Lookups will return database_not_loaded error until load() is called.
     */
    geolocation_service();

    /**
     * @brief Construct a geolocation service and load the database.
     *
     * @param database_path Path to MaxMind GeoLite2-City.mmdb file
     */
    explicit geolocation_service(const std::string& database_path);

    ~geolocation_service();

    // Non-copyable, movable
    geolocation_service(const geolocation_service&) = delete;
    geolocation_service& operator=(const geolocation_service&) = delete;
    geolocation_service(geolocation_service&&) noexcept;
    geolocation_service& operator=(geolocation_service&&) noexcept;

    /**
     * @brief Load or reload the MaxMind database.
     *
     * @param database_path Path to MaxMind GeoLite2-City.mmdb file
     * @return true if database was loaded successfully
     */
    bool load(const std::string& database_path);

    /**
     * @brief Check if the database is loaded.
     */
    [[nodiscard]] bool is_loaded() const;

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
    struct impl;
    std::unique_ptr<impl> pimpl_;
};

}

#endif
