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
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.trading.api/domain/script_codec.hpp"
#include "ores.trading.api/messaging/script_protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::trading::messaging::script_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_key>();
}

TEST_CASE("ores::trading::messaging::script_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_write>();
}

TEST_CASE("ores::trading::messaging::script_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_change>();
}

TEST_CASE("ores::trading::messaging::script_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_removal>();
}

TEST_CASE("ores::trading::messaging::script_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_lookup>();
}

TEST_CASE("ores::trading::messaging::scripts_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::scripts_filter>();
}

TEST_CASE("ores::trading::messaging::script_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_event>();
}

TEST_CASE("ores::trading::messaging::script_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_version_key>();
}

TEST_CASE("ores::trading::messaging::script_versions_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_versions_filter>();
}

TEST_CASE("ores::trading::messaging::list_scripts_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_scripts_request>();
}

TEST_CASE("ores::trading::messaging::list_scripts_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_scripts_response>();
}

TEST_CASE("ores::trading::messaging::get_script_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_script_request>();
}

TEST_CASE("ores::trading::messaging::get_script_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_script_response>();
}

TEST_CASE("ores::trading::messaging::get_many_scripts_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_many_scripts_request>();
}

TEST_CASE("ores::trading::messaging::get_many_scripts_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_many_scripts_response>();
}

TEST_CASE("ores::trading::messaging::put_script_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_script_request>();
}

TEST_CASE("ores::trading::messaging::put_script_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_script_response>();
}

TEST_CASE("ores::trading::messaging::put_many_scripts_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_many_scripts_request>();
}

TEST_CASE("ores::trading::messaging::put_many_scripts_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_many_scripts_response>();
}

TEST_CASE("ores::trading::messaging::delete_script_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_script_request>();
}

TEST_CASE("ores::trading::messaging::delete_script_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_script_response>();
}

TEST_CASE("ores::trading::messaging::delete_many_scripts_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_many_scripts_request>();
}

TEST_CASE("ores::trading::messaging::delete_many_scripts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_many_scripts_response>();
}

TEST_CASE("ores::trading::messaging::list_script_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_script_versions_request>();
}

TEST_CASE("ores::trading::messaging::list_script_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_script_versions_response>();
}

TEST_CASE("ores::trading::messaging::get_script_version_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_script_version_request>();
}

TEST_CASE("ores::trading::messaging::get_script_version_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_script_version_response>();
}
