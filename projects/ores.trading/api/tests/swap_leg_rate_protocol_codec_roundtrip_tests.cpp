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
#include "ores.trading.api/domain/swap_leg_rate_codec.hpp"
#include "ores.trading.api/messaging/swap_leg_rate_protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::trading::messaging::swap_leg_rate_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_key>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_write>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_change>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_removal>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_lookup>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rates_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rates_filter>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_event>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_version_key>();
}

TEST_CASE("ores::trading::messaging::swap_leg_rate_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::swap_leg_rate_versions_filter>();
}

TEST_CASE("ores::trading::messaging::list_swap_leg_rates_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_swap_leg_rates_request>();
}

TEST_CASE("ores::trading::messaging::list_swap_leg_rates_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::list_swap_leg_rates_response>();
}

TEST_CASE("ores::trading::messaging::get_swap_leg_rate_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_swap_leg_rate_request>();
}

TEST_CASE("ores::trading::messaging::get_swap_leg_rate_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::get_swap_leg_rate_response>();
}

TEST_CASE("ores::trading::messaging::get_many_swap_leg_rates_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::get_many_swap_leg_rates_request>();
}

TEST_CASE("ores::trading::messaging::get_many_swap_leg_rates_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::get_many_swap_leg_rates_response>();
}

TEST_CASE("ores::trading::messaging::put_swap_leg_rate_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_swap_leg_rate_request>();
}

TEST_CASE("ores::trading::messaging::put_swap_leg_rate_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::put_swap_leg_rate_response>();
}

TEST_CASE("ores::trading::messaging::put_many_swap_leg_rates_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::put_many_swap_leg_rates_request>();
}

TEST_CASE("ores::trading::messaging::put_many_swap_leg_rates_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::put_many_swap_leg_rates_response>();
}

TEST_CASE("ores::trading::messaging::delete_swap_leg_rate_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_swap_leg_rate_request>();
}

TEST_CASE("ores::trading::messaging::delete_swap_leg_rate_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::delete_swap_leg_rate_response>();
}

TEST_CASE("ores::trading::messaging::delete_many_swap_leg_rates_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::delete_many_swap_leg_rates_request>();
}

TEST_CASE("ores::trading::messaging::delete_many_swap_leg_rates_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::delete_many_swap_leg_rates_response>();
}

TEST_CASE("ores::trading::messaging::list_by_trade_id_swap_leg_rates_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::list_by_trade_id_swap_leg_rates_request>();
}

TEST_CASE("ores::trading::messaging::list_by_trade_id_swap_leg_rates_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::list_by_trade_id_swap_leg_rates_response>();
}

TEST_CASE("ores::trading::messaging::list_swap_leg_rate_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::list_swap_leg_rate_versions_request>();
}

TEST_CASE("ores::trading::messaging::list_swap_leg_rate_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::list_swap_leg_rate_versions_response>();
}

TEST_CASE("ores::trading::messaging::get_swap_leg_rate_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::get_swap_leg_rate_version_request>();
}

TEST_CASE("ores::trading::messaging::get_swap_leg_rate_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::get_swap_leg_rate_version_response>();
}
