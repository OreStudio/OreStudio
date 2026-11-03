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
#include "ores.refdata.api/domain/payment_frequency_codec.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::payment_frequency_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_key>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_write>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_change>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_removal>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_lookup>();
}

TEST_CASE("ores::refdata::messaging::payment_frequencies_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequencies_filter>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_event>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::payment_frequency_version_key>();
}

TEST_CASE("ores::refdata::messaging::payment_frequency_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::payment_frequency_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_payment_frequencies_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_payment_frequencies_request>();
}

TEST_CASE("ores::refdata::messaging::list_payment_frequencies_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_payment_frequencies_response>();
}

TEST_CASE("ores::refdata::messaging::get_payment_frequency_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_payment_frequency_request>();
}

TEST_CASE("ores::refdata::messaging::get_payment_frequency_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_payment_frequency_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_payment_frequencies_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_payment_frequencies_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_payment_frequencies_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_payment_frequencies_response>();
}

TEST_CASE("ores::refdata::messaging::put_payment_frequency_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_payment_frequency_request>();
}

TEST_CASE("ores::refdata::messaging::put_payment_frequency_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_payment_frequency_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_payment_frequencies_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_payment_frequencies_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_payment_frequencies_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_payment_frequencies_response>();
}

TEST_CASE("ores::refdata::messaging::delete_payment_frequency_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_payment_frequency_request>();
}

TEST_CASE("ores::refdata::messaging::delete_payment_frequency_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_payment_frequency_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_payment_frequencies_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_payment_frequencies_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_payment_frequencies_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_payment_frequencies_response>();
}

TEST_CASE("ores::refdata::messaging::list_payment_frequency_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_payment_frequency_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_payment_frequency_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_payment_frequency_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_payment_frequency_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_payment_frequency_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_payment_frequency_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_payment_frequency_version_response>();
}
