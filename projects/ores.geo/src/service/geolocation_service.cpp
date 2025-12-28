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
#include "ores.geo/service/geolocation_service.hpp"

namespace ores::geo::service {

geolocation_service::geolocation_service(database::context ctx)
    : ctx_(std::move(ctx)) {}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const boost::asio::ip::address& ip) const {
    return lookup(ip.to_string());
}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const std::string& ip_string) const {
    auto conn = ctx_.acquire();
    if (!conn) {
        return std::unexpected(geolocation_error::database_not_available);
    }

    try {
        // Query the geoip_lookup function
        const std::string query =
            "SELECT country_code, city_name, latitude, longitude "
            "FROM ores.geoip_lookup($1::inet)";

        auto result = conn->exec_params(query, ip_string);

        if (result.empty()) {
            return std::unexpected(geolocation_error::address_not_found);
        }

        geolocation_result geo_result;
        geo_result.country_code = result[0]["country_code"].as<std::string>("");
        geo_result.city = result[0]["city_name"].as<std::string>("");

        if (!result[0]["latitude"].is_null()) {
            geo_result.latitude = result[0]["latitude"].as<double>();
        }
        if (!result[0]["longitude"].is_null()) {
            geo_result.longitude = result[0]["longitude"].as<double>();
        }

        return geo_result;

    } catch (const std::exception&) {
        return std::unexpected(geolocation_error::lookup_failed);
    }
}

}
