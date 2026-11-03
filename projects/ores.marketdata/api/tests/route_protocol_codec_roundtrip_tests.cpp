/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.marketdata.api/domain/route_codec.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::marketdata::messaging::route_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_key>();
}

TEST_CASE("ores::marketdata::messaging::route_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_write>();
}

TEST_CASE("ores::marketdata::messaging::route_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_change>();
}

TEST_CASE("ores::marketdata::messaging::route_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_removal>();
}

TEST_CASE("ores::marketdata::messaging::route_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_lookup>();
}

TEST_CASE("ores::marketdata::messaging::routes_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::routes_filter>();
}

TEST_CASE("ores::marketdata::messaging::route_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_event>();
}

TEST_CASE("ores::marketdata::messaging::route_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_version_key>();
}

TEST_CASE("ores::marketdata::messaging::route_versions_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::route_versions_filter>();
}

TEST_CASE("ores::marketdata::messaging::list_routes_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::list_routes_request>();
}

TEST_CASE("ores::marketdata::messaging::list_routes_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::list_routes_response>();
}

TEST_CASE("ores::marketdata::messaging::get_route_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_route_request>();
}

TEST_CASE("ores::marketdata::messaging::get_route_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_route_response>();
}

TEST_CASE("ores::marketdata::messaging::get_many_routes_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_many_routes_request>();
}

TEST_CASE("ores::marketdata::messaging::get_many_routes_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_many_routes_response>();
}

TEST_CASE("ores::marketdata::messaging::put_route_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::put_route_request>();
}

TEST_CASE("ores::marketdata::messaging::put_route_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::put_route_response>();
}

TEST_CASE("ores::marketdata::messaging::put_many_routes_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::put_many_routes_request>();
}

TEST_CASE("ores::marketdata::messaging::put_many_routes_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::put_many_routes_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_route_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::delete_route_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_route_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::delete_route_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_routes_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::delete_many_routes_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_routes_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::delete_many_routes_response>();
}

TEST_CASE("ores::marketdata::messaging::list_route_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::list_route_versions_request>();
}

TEST_CASE("ores::marketdata::messaging::list_route_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_route_versions_response>();
}

TEST_CASE("ores::marketdata::messaging::get_route_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_route_version_request>();
}

TEST_CASE("ores::marketdata::messaging::get_route_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::get_route_version_response>();
}
