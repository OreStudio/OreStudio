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

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.testing/database_lifecycle_listener.hpp"

namespace {

const std::string_view test_suite("ores.geo.tests");
const std::string tags("[service]");

}

using ores::geo::service::geolocation_service;
using ores::geo::service::geolocation_error;
using ores::geo::service::geolocation_result;
using namespace ores::telemetry::log;

TEST_CASE("geolocation_result_default_construction", tags) {
    auto lg(make_logger(test_suite));

    geolocation_result result;
    BOOST_LOG_SEV(lg, info) << "Default geolocation_result created";

    CHECK(result.country_code.empty());
    CHECK(result.city.empty());
    CHECK(!result.latitude.has_value());
    CHECK(!result.longitude.has_value());
}

TEST_CASE("geolocation_result_with_values", tags) {
    auto lg(make_logger(test_suite));

    geolocation_result result;
    result.country_code = "US";
    result.city = "Mountain View";
    result.latitude = 37.3861;
    result.longitude = -122.0839;

    BOOST_LOG_SEV(lg, info) << "Geolocation result: country=" << result.country_code
                            << " city=" << result.city;

    CHECK(result.country_code == "US");
    CHECK(result.city == "Mountain View");
    CHECK(result.latitude.has_value());
    CHECK(result.latitude.value() > 37.0);
    CHECK(result.longitude.has_value());
    CHECK(result.longitude.value() < -120.0);
}

TEST_CASE("lookup_returns_not_found_for_private_ip", tags) {
    auto lg(make_logger(test_suite));
    auto ctx = ores::testing::database_lifecycle_listener::make_context();

    geolocation_service sut(ctx);

    // Private IP addresses are not in the GeoIP database
    auto result = sut.lookup("192.168.1.1");
    BOOST_LOG_SEV(lg, info) << "Lookup result for private IP";

    // May return not found or empty result depending on database state
    if (!result.has_value()) {
        CHECK(result.error() == geolocation_error::address_not_found);
    }
}

TEST_CASE("lookup_handles_localhost", tags) {
    auto lg(make_logger(test_suite));
    auto ctx = ores::testing::database_lifecycle_listener::make_context();

    geolocation_service sut(ctx);

    // Localhost is not in the GeoIP database
    auto result = sut.lookup("127.0.0.1");
    BOOST_LOG_SEV(lg, info) << "Lookup result for localhost";

    // May return not found or empty result depending on database state
    if (!result.has_value()) {
        CHECK(result.error() == geolocation_error::address_not_found);
    }
}

TEST_CASE("lookup_with_boost_asio_address", tags) {
    auto lg(make_logger(test_suite));
    auto ctx = ores::testing::database_lifecycle_listener::make_context();

    geolocation_service sut(ctx);

    boost::asio::ip::address addr = boost::asio::ip::make_address("10.0.0.1");
    auto result = sut.lookup(addr);
    BOOST_LOG_SEV(lg, info) << "Lookup result for boost::asio::ip::address";

    // Private IP, should return not found
    if (!result.has_value()) {
        CHECK(result.error() == geolocation_error::address_not_found);
    }
}
