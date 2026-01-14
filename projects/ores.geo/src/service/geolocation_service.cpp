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

#include <boost/system/error_code.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::geo::service {

geolocation_service::geolocation_service(database::context ctx)
    : ctx_(std::move(ctx)) {}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const boost::asio::ip::address& ip) const {
    try {
        // Use the validated IP address's string representation to prevent injection
        const auto ip_str = ip.to_string();
        const std::string sql =
            "SELECT country_code FROM ores.geo_ip2country_lookup_fn('" + ip_str + "'::inet)";

        auto rows = database::repository::execute_raw_multi_column_query(
            ctx_, sql, lg(), "Geolocation lookup for " + ip_str);

        if (rows.empty()) {
            return std::unexpected(geolocation_error::address_not_found);
        }

        const auto& row = rows[0];
        geolocation_result geo_result;

        if (row.size() >= 1 && row[0].has_value()) {
            geo_result.country_code = row[0].value();
        }

        // Filter out "None" which indicates unrouted IP ranges
        if (geo_result.country_code == "None" || geo_result.country_code.empty()) {
            return std::unexpected(geolocation_error::address_not_found);
        }

        return geo_result;

    } catch (const std::runtime_error&) {
        return std::unexpected(geolocation_error::lookup_failed);
    }
}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const std::string& ip_string) const {
    // Validate and parse the IP address to prevent SQL injection
    boost::system::error_code ec;
    auto ip = boost::asio::ip::make_address(ip_string, ec);
    if (ec) {
        return std::unexpected(geolocation_error::invalid_address);
    }
    return lookup(ip);
}

}
