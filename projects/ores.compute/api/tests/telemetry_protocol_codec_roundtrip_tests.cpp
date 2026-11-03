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
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::compute::messaging::get_grid_stats_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_stats_request>();
}

TEST_CASE("ores::compute::messaging::node_stats_summary", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::node_stats_summary>();
}

TEST_CASE("ores::compute::messaging::get_grid_stats_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_stats_response>();
}

TEST_CASE("ores::compute::messaging::get_grid_series_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_series_request>();
}

TEST_CASE("ores::compute::messaging::get_grid_series_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_series_response>();
}

TEST_CASE("ores::compute::messaging::get_node_series_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_node_series_request>();
}

TEST_CASE("ores::compute::messaging::node_series_node", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::node_series_node>();
}

TEST_CASE("ores::compute::messaging::get_node_series_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_node_series_response>();
}

TEST_CASE("ores::compute::messaging::node_sample_message", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::node_sample_message>();
}

TEST_CASE("ores::compute::messaging::get_grid_usage_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_usage_request>();
}

TEST_CASE("ores::compute::messaging::get_grid_usage_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::compute::messaging::get_grid_usage_response>();
}
