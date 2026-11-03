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
#include "ores.refdata.api/domain/monetary_nature_codec.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::monetary_nature_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_key>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_write>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_change>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_removal>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_lookup>();
}

TEST_CASE("ores::refdata::messaging::monetary_natures_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_natures_filter>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_event>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::monetary_nature_version_key>();
}

TEST_CASE("ores::refdata::messaging::monetary_nature_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::monetary_nature_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_monetary_natures_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_monetary_natures_request>();
}

TEST_CASE("ores::refdata::messaging::list_monetary_natures_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_monetary_natures_response>();
}

TEST_CASE("ores::refdata::messaging::get_monetary_nature_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_monetary_nature_request>();
}

TEST_CASE("ores::refdata::messaging::get_monetary_nature_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_monetary_nature_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_monetary_natures_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_monetary_natures_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_monetary_natures_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_monetary_natures_response>();
}

TEST_CASE("ores::refdata::messaging::put_monetary_nature_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_monetary_nature_request>();
}

TEST_CASE("ores::refdata::messaging::put_monetary_nature_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_monetary_nature_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_monetary_natures_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_monetary_natures_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_monetary_natures_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_monetary_natures_response>();
}

TEST_CASE("ores::refdata::messaging::delete_monetary_nature_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_monetary_nature_request>();
}

TEST_CASE("ores::refdata::messaging::delete_monetary_nature_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_monetary_nature_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_monetary_natures_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_monetary_natures_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_monetary_natures_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_monetary_natures_response>();
}

TEST_CASE("ores::refdata::messaging::list_monetary_nature_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_monetary_nature_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_monetary_nature_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_monetary_nature_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_monetary_nature_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_monetary_nature_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_monetary_nature_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_monetary_nature_version_response>();
}
