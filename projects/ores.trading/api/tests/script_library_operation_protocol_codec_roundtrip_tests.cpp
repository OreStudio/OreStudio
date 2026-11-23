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
#include "ores.trading.api/domain/script_calibration_codec.hpp"
#include "ores.trading.api/domain/script_calibration_strike_codec.hpp"
#include "ores.trading.api/domain/script_coarsening_codec.hpp"
#include "ores.trading.api/domain/script_codec.hpp"
#include "ores.trading.api/domain/script_result_codec.hpp"
#include "ores.trading.api/domain/script_variant_codec.hpp"
#include "ores.trading.api/domain/trade_booking_codec.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/domain/trade_identifier_codec.hpp"
#include "ores.trading.api/messaging/script_library_operation_protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::trading::messaging::script_pin", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_pin>();
}

TEST_CASE("ores::trading::messaging::export_script_library_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::export_script_library_request>();
}

TEST_CASE("ores::trading::messaging::export_script_library_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::export_script_library_response>();
}

TEST_CASE("ores::trading::messaging::published_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::published_result>();
}

TEST_CASE("ores::trading::messaging::published_calibration", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::published_calibration>();
}

TEST_CASE("ores::trading::messaging::published_variant", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::published_variant>();
}

TEST_CASE("ores::trading::messaging::script_diagnostic", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_diagnostic>();
}

TEST_CASE("ores::trading::messaging::script_variable_value", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::script_variable_value>();
}

TEST_CASE("ores::trading::messaging::validate_script_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::validate_script_request>();
}

TEST_CASE("ores::trading::messaging::validate_script_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::validate_script_response>();
}

TEST_CASE("ores::trading::messaging::book_scripted_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::book_scripted_request>();
}

TEST_CASE("ores::trading::messaging::book_scripted_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::book_scripted_response>();
}

TEST_CASE("ores::trading::messaging::publish_script_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::publish_script_request>();
}

TEST_CASE("ores::trading::messaging::publish_script_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::publish_script_response>();
}
