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
#include "ores.ore.api/messaging/run_configuration_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::ore::messaging::run_input_file", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::run_input_file>();
}

TEST_CASE("ores::ore::messaging::saved_document", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::saved_document>();
}

TEST_CASE("ores::ore::messaging::import_run_configuration_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::import_run_configuration_request>();
}

TEST_CASE("ores::ore::messaging::import_run_configuration_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::import_run_configuration_response>();
}

TEST_CASE("ores::ore::messaging::run_configuration_import_execute_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::ore::messaging::run_configuration_import_execute_request>();
}

TEST_CASE("ores::ore::messaging::run_configuration_import_execute_result",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::ore::messaging::run_configuration_import_execute_result>();
}

TEST_CASE("ores::ore::messaging::run_configuration_import_rollback_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::ore::messaging::run_configuration_import_rollback_request>();
}

TEST_CASE("ores::ore::messaging::export_run_configuration_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::export_run_configuration_request>();
}

TEST_CASE("ores::ore::messaging::export_run_configuration_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::ore::messaging::export_run_configuration_response>();
}
