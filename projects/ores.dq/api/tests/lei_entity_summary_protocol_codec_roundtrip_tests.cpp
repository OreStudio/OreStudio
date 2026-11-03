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
#include "ores.dq.api/messaging/lei_entity_summary_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::dq::messaging::lei_entity_summary", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::lei_entity_summary>();
}

TEST_CASE("ores::dq::messaging::get_lei_entities_summary_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_lei_entities_summary_request>();
}

TEST_CASE("ores::dq::messaging::get_lei_entities_summary_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_lei_entities_summary_response>();
}

TEST_CASE("ores::dq::messaging::lei_entity_match", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::lei_entity_match>();
}

TEST_CASE("ores::dq::messaging::search_lei_entities_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::search_lei_entities_request>();
}

TEST_CASE("ores::dq::messaging::search_lei_entities_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::search_lei_entities_response>();
}
