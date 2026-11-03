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
#include "ores.reporting.api/domain/engine_settings_building_codec.hpp"
#include "ores.reporting.api/messaging/engine_settings_building_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::reporting::messaging::engine_settings_building_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::engine_settings_building_key>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_write>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_change>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_removal>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_lookup>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_filter>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_event>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_version_key>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_building_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_building_versions_filter>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::get_many_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::get_many_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::put_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::put_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::put_many_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_many_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::put_many_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_many_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::delete_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::delete_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::delete_many_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_engine_settings_building_request>();
}

TEST_CASE("ores::reporting::messaging::delete_many_engine_settings_building_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::list_by_engine_settings_id_engine_settings_building_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_engine_settings_id_engine_settings_building_request>();
}

TEST_CASE(
    "ores::reporting::messaging::list_by_engine_settings_id_engine_settings_building_response",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_engine_settings_id_engine_settings_building_response>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_building_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_building_versions_request>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_building_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_building_versions_response>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_building_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_building_version_request>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_building_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_building_version_response>();
}
