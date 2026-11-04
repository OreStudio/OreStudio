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
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_codec.hpp"
#include "ores.synthetic.api/messaging/yield_curve_process_parameter_definition_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_key>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_write>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_change>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_removal>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_lookup>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definitions_filter>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_event>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_version_key>();
}

TEST_CASE("ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::yield_curve_process_parameter_definition_versions_filter>();
}

TEST_CASE("ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_request>();
}

TEST_CASE("ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::list_yield_curve_process_parameter_definitions_response>();
}

TEST_CASE("ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::get_yield_curve_process_parameter_definition_request>();
}

TEST_CASE("ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::get_yield_curve_process_parameter_definition_response>();
}

TEST_CASE("ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_request>();
}

TEST_CASE("ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::get_many_yield_curve_process_parameter_definitions_response>();
}

TEST_CASE("ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::put_yield_curve_process_parameter_definition_request>();
}

TEST_CASE("ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::put_yield_curve_process_parameter_definition_response>();
}

TEST_CASE("ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_request>();
}

TEST_CASE("ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::put_many_yield_curve_process_parameter_definitions_response>();
}

TEST_CASE("ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_request>();
}

TEST_CASE("ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::delete_yield_curve_process_parameter_definition_response>();
}

TEST_CASE(
    "ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_request",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::
            delete_many_yield_curve_process_parameter_definitions_request>();
}

TEST_CASE(
    "ores::synthetic::messaging::delete_many_yield_curve_process_parameter_definitions_response",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::
            delete_many_yield_curve_process_parameter_definitions_response>();
}

TEST_CASE(
    "ores::synthetic::messaging::list_yield_curve_process_parameter_definition_versions_request",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::
            list_yield_curve_process_parameter_definition_versions_request>();
}

TEST_CASE(
    "ores::synthetic::messaging::list_yield_curve_process_parameter_definition_versions_response",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::
            list_yield_curve_process_parameter_definition_versions_response>();
}

TEST_CASE(
    "ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_request",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_request>();
}

TEST_CASE(
    "ores::synthetic::messaging::get_yield_curve_process_parameter_definition_version_response",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::synthetic::messaging::
            get_yield_curve_process_parameter_definition_version_response>();
}
