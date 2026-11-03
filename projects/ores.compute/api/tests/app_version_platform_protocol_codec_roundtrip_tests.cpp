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
#include "ores.compute.api/domain/app_version_platform_codec.hpp"
#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::compute::messaging::app_version_platform_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platform_key>();
}

TEST_CASE("ores::compute::messaging::app_version_platform_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platform_write>();
}

TEST_CASE("ores::compute::messaging::app_version_platform_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platform_change>();
}

TEST_CASE("ores::compute::messaging::app_version_platform_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platform_removal>();
}

TEST_CASE("ores::compute::messaging::app_version_platform_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platform_lookup>();
}

TEST_CASE("ores::compute::messaging::app_version_platforms_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::app_version_platforms_filter>();
}

TEST_CASE("ores::compute::messaging::list_app_version_platforms_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::list_app_version_platforms_request>();
}

TEST_CASE("ores::compute::messaging::list_app_version_platforms_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::list_app_version_platforms_response>();
}

TEST_CASE("ores::compute::messaging::get_app_version_platform_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::get_app_version_platform_request>();
}

TEST_CASE("ores::compute::messaging::get_app_version_platform_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::get_app_version_platform_response>();
}

TEST_CASE("ores::compute::messaging::get_many_app_version_platforms_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::get_many_app_version_platforms_request>();
}

TEST_CASE("ores::compute::messaging::get_many_app_version_platforms_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::get_many_app_version_platforms_response>();
}

TEST_CASE("ores::compute::messaging::put_app_version_platform_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::put_app_version_platform_request>();
}

TEST_CASE("ores::compute::messaging::put_app_version_platform_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::put_app_version_platform_response>();
}

TEST_CASE("ores::compute::messaging::put_many_app_version_platforms_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::put_many_app_version_platforms_request>();
}

TEST_CASE("ores::compute::messaging::put_many_app_version_platforms_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::put_many_app_version_platforms_response>();
}

TEST_CASE("ores::compute::messaging::delete_app_version_platform_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::delete_app_version_platform_request>();
}

TEST_CASE("ores::compute::messaging::delete_app_version_platform_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::delete_app_version_platform_response>();
}

TEST_CASE("ores::compute::messaging::delete_many_app_version_platforms_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::delete_many_app_version_platforms_request>();
}

TEST_CASE("ores::compute::messaging::delete_many_app_version_platforms_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::delete_many_app_version_platforms_response>();
}

TEST_CASE("ores::compute::messaging::list_by_app_version_id_app_version_platforms_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>();
}

TEST_CASE("ores::compute::messaging::list_by_app_version_id_app_version_platforms_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>();
}
