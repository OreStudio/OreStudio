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
#include "ores.reporting.api/domain/engine_settings_group_setting_codec.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_setting_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_key>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_write>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_change>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_removal>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_lookup>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_settings_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_settings_filter>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_event>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_version_key>();
}

TEST_CASE("ores::reporting::messaging::engine_settings_group_setting_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::engine_settings_group_setting_versions_filter>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_group_settings_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_group_settings_request>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_group_settings_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_group_settings_response>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_group_setting_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_group_setting_request>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_group_setting_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_group_setting_response>();
}

TEST_CASE("ores::reporting::messaging::get_many_engine_settings_group_settings_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_engine_settings_group_settings_request>();
}

TEST_CASE("ores::reporting::messaging::get_many_engine_settings_group_settings_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_engine_settings_group_settings_response>();
}

TEST_CASE("ores::reporting::messaging::put_engine_settings_group_setting_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_engine_settings_group_setting_request>();
}

TEST_CASE("ores::reporting::messaging::put_engine_settings_group_setting_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_engine_settings_group_setting_response>();
}

TEST_CASE("ores::reporting::messaging::put_many_engine_settings_group_settings_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_many_engine_settings_group_settings_request>();
}

TEST_CASE("ores::reporting::messaging::put_many_engine_settings_group_settings_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::put_many_engine_settings_group_settings_response>();
}

TEST_CASE("ores::reporting::messaging::delete_engine_settings_group_setting_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_engine_settings_group_setting_request>();
}

TEST_CASE("ores::reporting::messaging::delete_engine_settings_group_setting_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_engine_settings_group_setting_response>();
}

TEST_CASE("ores::reporting::messaging::delete_many_engine_settings_group_settings_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_engine_settings_group_settings_request>();
}

TEST_CASE("ores::reporting::messaging::delete_many_engine_settings_group_settings_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_engine_settings_group_settings_response>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_group_setting_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_group_setting_versions_request>();
}

TEST_CASE("ores::reporting::messaging::list_engine_settings_group_setting_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_engine_settings_group_setting_versions_response>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_group_setting_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_group_setting_version_request>();
}

TEST_CASE("ores::reporting::messaging::get_engine_settings_group_setting_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_engine_settings_group_setting_version_response>();
}
