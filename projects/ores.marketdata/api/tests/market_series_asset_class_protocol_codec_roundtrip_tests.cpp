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
#include "ores.marketdata.api/domain/market_series_asset_class_codec.hpp"
#include "ores.marketdata.api/messaging/market_series_asset_class_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::marketdata::messaging::market_series_asset_class_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_class_key>();
}

TEST_CASE("ores::marketdata::messaging::market_series_asset_class_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_class_write>();
}

TEST_CASE("ores::marketdata::messaging::market_series_asset_class_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_class_change>();
}

TEST_CASE("ores::marketdata::messaging::market_series_asset_class_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_class_removal>();
}

TEST_CASE("ores::marketdata::messaging::market_series_asset_class_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_class_lookup>();
}

TEST_CASE("ores::marketdata::messaging::market_series_asset_classes_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::market_series_asset_classes_filter>();
}

TEST_CASE("ores::marketdata::messaging::list_market_series_asset_classes_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_market_series_asset_classes_request>();
}

TEST_CASE("ores::marketdata::messaging::list_market_series_asset_classes_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_market_series_asset_classes_response>();
}

TEST_CASE("ores::marketdata::messaging::get_market_series_asset_class_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_market_series_asset_class_request>();
}

TEST_CASE("ores::marketdata::messaging::get_market_series_asset_class_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_market_series_asset_class_response>();
}

TEST_CASE("ores::marketdata::messaging::get_many_market_series_asset_classes_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_many_market_series_asset_classes_request>();
}

TEST_CASE("ores::marketdata::messaging::get_many_market_series_asset_classes_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_many_market_series_asset_classes_response>();
}

TEST_CASE("ores::marketdata::messaging::put_market_series_asset_class_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_market_series_asset_class_request>();
}

TEST_CASE("ores::marketdata::messaging::put_market_series_asset_class_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_market_series_asset_class_response>();
}

TEST_CASE("ores::marketdata::messaging::put_many_market_series_asset_classes_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_many_market_series_asset_classes_request>();
}

TEST_CASE("ores::marketdata::messaging::put_many_market_series_asset_classes_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_many_market_series_asset_classes_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_market_series_asset_class_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_market_series_asset_class_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_market_series_asset_class_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_market_series_asset_class_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_market_series_asset_classes_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_many_market_series_asset_classes_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_market_series_asset_classes_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_many_market_series_asset_classes_response>();
}

TEST_CASE(
    "ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_request",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::
            list_by_market_series_id_market_series_asset_classes_request>();
}

TEST_CASE(
    "ores::marketdata::messaging::list_by_market_series_id_market_series_asset_classes_response",
    "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::
            list_by_market_series_id_market_series_asset_classes_response>();
}
