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
#include "ores.marketdata.api/domain/subscription_interval_codec.hpp"
#include "ores.marketdata.api/messaging/subscription_interval_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::marketdata::messaging::subscription_interval_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::subscription_interval_key>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::subscription_interval_write>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_interval_change>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_interval_removal>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_interval_lookup>();
}

TEST_CASE("ores::marketdata::messaging::subscription_intervals_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_intervals_filter>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::marketdata::messaging::subscription_interval_event>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_interval_version_key>();
}

TEST_CASE("ores::marketdata::messaging::subscription_interval_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::subscription_interval_versions_filter>();
}

TEST_CASE("ores::marketdata::messaging::list_subscription_intervals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_subscription_intervals_request>();
}

TEST_CASE("ores::marketdata::messaging::list_subscription_intervals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_subscription_intervals_response>();
}

TEST_CASE("ores::marketdata::messaging::get_subscription_interval_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_subscription_interval_request>();
}

TEST_CASE("ores::marketdata::messaging::get_subscription_interval_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_subscription_interval_response>();
}

TEST_CASE("ores::marketdata::messaging::get_many_subscription_intervals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_many_subscription_intervals_request>();
}

TEST_CASE("ores::marketdata::messaging::get_many_subscription_intervals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_many_subscription_intervals_response>();
}

TEST_CASE("ores::marketdata::messaging::put_subscription_interval_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_subscription_interval_request>();
}

TEST_CASE("ores::marketdata::messaging::put_subscription_interval_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_subscription_interval_response>();
}

TEST_CASE("ores::marketdata::messaging::put_many_subscription_intervals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_many_subscription_intervals_request>();
}

TEST_CASE("ores::marketdata::messaging::put_many_subscription_intervals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::put_many_subscription_intervals_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_subscription_interval_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_subscription_interval_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_subscription_interval_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_subscription_interval_response>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_subscription_intervals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_many_subscription_intervals_request>();
}

TEST_CASE("ores::marketdata::messaging::delete_many_subscription_intervals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::delete_many_subscription_intervals_response>();
}

TEST_CASE("ores::marketdata::messaging::list_subscription_interval_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_subscription_interval_versions_request>();
}

TEST_CASE("ores::marketdata::messaging::list_subscription_interval_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::list_subscription_interval_versions_response>();
}

TEST_CASE("ores::marketdata::messaging::get_subscription_interval_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_subscription_interval_version_request>();
}

TEST_CASE("ores::marketdata::messaging::get_subscription_interval_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::marketdata::messaging::get_subscription_interval_version_response>();
}
