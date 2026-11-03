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
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::synthetic::messaging::get_market_regime_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_regime_request>();
}

TEST_CASE("ores::synthetic::messaging::market_regime_sample", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::market_regime_sample>();
}

TEST_CASE("ores::synthetic::messaging::get_market_regime_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_regime_response>();
}

TEST_CASE("ores::synthetic::messaging::get_market_trace_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_trace_request>();
}

TEST_CASE("ores::synthetic::messaging::market_trace_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::market_trace_event>();
}

TEST_CASE("ores::synthetic::messaging::market_series_state", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::market_series_state>();
}

TEST_CASE("ores::synthetic::messaging::get_market_trace_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_trace_response>();
}

TEST_CASE("ores::synthetic::messaging::get_market_history_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_history_request>();
}

TEST_CASE("ores::synthetic::messaging::market_history_bin", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::market_history_bin>();
}

TEST_CASE("ores::synthetic::messaging::get_market_history_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::get_market_history_response>();
}

TEST_CASE("ores::synthetic::messaging::pin_market_regime_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::pin_market_regime_request>();
}

TEST_CASE("ores::synthetic::messaging::pin_market_regime_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::pin_market_regime_response>();
}

TEST_CASE("ores::synthetic::messaging::configure_market_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::synthetic::messaging::configure_market_request>();
}
