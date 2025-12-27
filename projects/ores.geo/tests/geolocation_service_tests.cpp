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

namespace {

const std::string_view test_suite("ores.geo.tests");
const std::string tags("[service]");

}

using ores::geo::service::geolocation_service;
using ores::geo::service::geolocation_error;
using ores::geo::service::geolocation_result;
using namespace ores::telemetry::log;

TEST_CASE("default_construction_creates_unloaded_service", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service sut;
    BOOST_LOG_SEV(lg, info) << "Created default geolocation service";

    CHECK(sut.is_loaded() == false);
}

TEST_CASE("lookup_with_unloaded_database_returns_error", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service sut;
    auto result = sut.lookup("8.8.8.8");
    BOOST_LOG_SEV(lg, info) << "Lookup result on unloaded database";

    CHECK(!result.has_value());
    CHECK(result.error() == geolocation_error::database_not_loaded);
}

TEST_CASE("lookup_with_invalid_ip_on_unloaded_database_returns_not_loaded_error", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service sut;
    auto result = sut.lookup("not-an-ip");
    BOOST_LOG_SEV(lg, info) << "Lookup with invalid IP on unloaded database";

    CHECK(!result.has_value());
    CHECK(result.error() == geolocation_error::database_not_loaded);
}

TEST_CASE("load_with_nonexistent_file_returns_false", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service sut;
    bool loaded = sut.load("/nonexistent/path/to/database.mmdb");
    BOOST_LOG_SEV(lg, info) << "Load result for nonexistent file: " << loaded;

    CHECK(loaded == false);
    CHECK(sut.is_loaded() == false);
}

TEST_CASE("move_construction_transfers_state", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service original;
    BOOST_LOG_SEV(lg, info) << "Original is_loaded: " << original.is_loaded();

    geolocation_service moved(std::move(original));
    BOOST_LOG_SEV(lg, info) << "Moved is_loaded: " << moved.is_loaded();

    CHECK(moved.is_loaded() == false);
}

TEST_CASE("move_assignment_transfers_state", tags) {
    auto lg(make_logger(test_suite));

    geolocation_service original;
    geolocation_service target;

    target = std::move(original);
    BOOST_LOG_SEV(lg, info) << "Target after move assignment is_loaded: "
                            << target.is_loaded();

    CHECK(target.is_loaded() == false);
}

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
