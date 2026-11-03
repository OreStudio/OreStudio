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
#include "ores.reporting.api/domain/ore_file_codec.hpp"
#include "ores.reporting.api/messaging/ore_file_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::reporting::messaging::ore_file_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_key>();
}

TEST_CASE("ores::reporting::messaging::ore_file_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_write>();
}

TEST_CASE("ores::reporting::messaging::ore_file_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_change>();
}

TEST_CASE("ores::reporting::messaging::ore_file_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_removal>();
}

TEST_CASE("ores::reporting::messaging::ore_file_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_lookup>();
}

TEST_CASE("ores::reporting::messaging::ore_files_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_files_filter>();
}

TEST_CASE("ores::reporting::messaging::ore_file_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_file_event>();
}

TEST_CASE("ores::reporting::messaging::ore_files_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_files_version_key>();
}

TEST_CASE("ores::reporting::messaging::ore_files_versions_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::ore_files_versions_filter>();
}

TEST_CASE("ores::reporting::messaging::list_ore_files_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::list_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::list_ore_files_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::list_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::get_ore_file_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_ore_file_request>();
}

TEST_CASE("ores::reporting::messaging::get_ore_file_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_ore_file_response>();
}

TEST_CASE("ores::reporting::messaging::get_many_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_many_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::get_many_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_many_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::put_ore_file_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::put_ore_file_request>();
}

TEST_CASE("ores::reporting::messaging::put_ore_file_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::put_ore_file_response>();
}

TEST_CASE("ores::reporting::messaging::put_many_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::put_many_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::put_many_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::put_many_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::delete_ore_file_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::delete_ore_file_request>();
}

TEST_CASE("ores::reporting::messaging::delete_ore_file_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::delete_ore_file_response>();
}

TEST_CASE("ores::reporting::messaging::delete_many_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::delete_many_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::delete_many_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::list_by_category_code_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_category_code_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::list_by_category_code_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_category_code_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::list_by_configuration_type_code_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_configuration_type_code_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::list_by_configuration_type_code_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_configuration_type_code_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_request>();
}

TEST_CASE("ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_schema_ore_file_code_ore_files_response>();
}

TEST_CASE("ores::reporting::messaging::list_ore_files_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_ore_files_versions_request>();
}

TEST_CASE("ores::reporting::messaging::list_ore_files_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_ore_files_versions_response>();
}

TEST_CASE("ores::reporting::messaging::get_ore_files_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_ore_files_version_request>();
}

TEST_CASE("ores::reporting::messaging::get_ore_files_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_ore_files_version_response>();
}
