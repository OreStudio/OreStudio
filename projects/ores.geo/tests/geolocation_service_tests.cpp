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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.geo/service/geolocation_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/asio/ip/address.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.geo.tests");
const std::string tags("[service]");

// 198.51.100.0/24 is RFC 5737 documentation space. No ip2country dataset maps
// it to a real country, so the only row that can answer a lookup inside it is
// the one a test seeds.
constexpr const char* seeded_ip = "198.51.100.1";
constexpr const char* seeded_range = "int8range(3325256704, 3325256960, '[)')";
constexpr const char* seeded_country = "ZZ";

// 203.0.113.0/24 is RFC 5737 documentation space too, and the tests leave it
// unseeded.
constexpr const char* uncovered_ip = "203.0.113.1";

}

using ores::geo::service::geolocation_service;
using ores::geo::service::geolocation_error;
using namespace ores::logging;

TEST_CASE("lookup returns the country code for a seeded range", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    const auto tenant = h.tenant_id().to_string();

    // Clear first so a leaked row from an interrupted run cannot mask the
    // seeded value.
    const std::string clear_sql =
        "delete from ores_geo_ip2country_tbl where ip_range = " +
        std::string(seeded_range) + " and tenant_id = '" + tenant + "'::uuid";
    const std::string seed_sql =
        "insert into ores_geo_ip2country_tbl (ip_range, tenant_id, country_code) values (" +
        std::string(seeded_range) + ", '" + tenant + "'::uuid, '" + seeded_country + "')";
    ores::database::repository::execute_raw_command(
        h.context(), clear_sql, lg, "Clearing the seeded ip2country range");
    ores::database::repository::execute_raw_command(
        h.context(), seed_sql, lg, "Seeding an ip2country range");

    geolocation_service sut(h.context());
    const auto result = sut.lookup(boost::asio::ip::make_address(seeded_ip));

    REQUIRE(result.has_value());
    REQUIRE(result->country_code == seeded_country);

    ores::database::repository::execute_raw_command(
        h.context(), clear_sql, lg, "Clearing the seeded ip2country range");
}

TEST_CASE("lookup returns invalid_address for a string that is not an address", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    geolocation_service sut(h.context());

    const auto result = sut.lookup("not-an-ip-address");

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == geolocation_error::invalid_address);
}

TEST_CASE("lookup returns address_not_found for a private address", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    geolocation_service sut(h.context());

    const auto result = sut.lookup("192.168.1.1");

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == geolocation_error::address_not_found);
}

TEST_CASE("lookup returns address_not_found for an uncovered address", tags) {
    auto lg(make_logger(test_suite));
    ores::testing::database_helper h;
    geolocation_service sut(h.context());

    const auto result = sut.lookup(uncovered_ip);

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == geolocation_error::address_not_found);
}
