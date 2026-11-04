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
#include "ores.analytics.api/domain/pricing_model_config_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_codec.hpp"
#include "ores.analytics.api/domain/pricing_model_product_parameter_codec.hpp"
#include "ores.analytics.api/domain/todays_market_collection_codec.hpp"
#include "ores.analytics.api/domain/todays_market_config_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_binding_codec.hpp"
#include "ores.analytics.api/domain/todays_market_configuration_codec.hpp"
#include "ores.analytics.api/domain/todays_market_entry_codec.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::analytics::messaging::pricing_engines_document", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::analytics::messaging::pricing_engines_document>();
}

TEST_CASE("ores::analytics::messaging::todays_market_document", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::analytics::messaging::todays_market_document>();
}

TEST_CASE("ores::analytics::messaging::pricing_engines_document_problem",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::pricing_engines_document_problem>();
}

TEST_CASE("ores::analytics::messaging::save_pricing_engines_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::save_pricing_engines_document_request>();
}

TEST_CASE("ores::analytics::messaging::save_pricing_engines_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::save_pricing_engines_document_response>();
}

TEST_CASE("ores::analytics::messaging::get_pricing_engines_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::get_pricing_engines_document_request>();
}

TEST_CASE("ores::analytics::messaging::get_pricing_engines_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::get_pricing_engines_document_response>();
}

TEST_CASE("ores::analytics::messaging::delete_pricing_engines_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::delete_pricing_engines_document_request>();
}

TEST_CASE("ores::analytics::messaging::delete_pricing_engines_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::delete_pricing_engines_document_response>();
}

TEST_CASE("ores::analytics::messaging::save_todays_market_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::save_todays_market_document_request>();
}

TEST_CASE("ores::analytics::messaging::save_todays_market_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::save_todays_market_document_response>();
}

TEST_CASE("ores::analytics::messaging::get_todays_market_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::get_todays_market_document_request>();
}

TEST_CASE("ores::analytics::messaging::get_todays_market_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::get_todays_market_document_response>();
}

TEST_CASE("ores::analytics::messaging::delete_todays_market_document_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::delete_todays_market_document_request>();
}

TEST_CASE("ores::analytics::messaging::delete_todays_market_document_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::analytics::messaging::delete_todays_market_document_response>();
}
