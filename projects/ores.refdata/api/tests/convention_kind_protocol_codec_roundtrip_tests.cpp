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
#include "ores.refdata.api/domain/convention_kind_codec.hpp"
#include "ores.refdata.api/messaging/convention_kind_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::convention_kind_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_key>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_write>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_change>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_removal>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_lookup>();
}

TEST_CASE("ores::refdata::messaging::convention_kinds_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kinds_filter>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_event>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::convention_kind_version_key>();
}

TEST_CASE("ores::refdata::messaging::convention_kind_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::convention_kind_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_convention_kinds_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_convention_kinds_request>();
}

TEST_CASE("ores::refdata::messaging::list_convention_kinds_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_convention_kinds_response>();
}

TEST_CASE("ores::refdata::messaging::get_convention_kind_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_convention_kind_request>();
}

TEST_CASE("ores::refdata::messaging::get_convention_kind_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_convention_kind_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_convention_kinds_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_convention_kinds_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_convention_kinds_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_convention_kinds_response>();
}

TEST_CASE("ores::refdata::messaging::put_convention_kind_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_convention_kind_request>();
}

TEST_CASE("ores::refdata::messaging::put_convention_kind_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_convention_kind_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_convention_kinds_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_convention_kinds_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_convention_kinds_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_convention_kinds_response>();
}

TEST_CASE("ores::refdata::messaging::delete_convention_kind_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_convention_kind_request>();
}

TEST_CASE("ores::refdata::messaging::delete_convention_kind_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_convention_kind_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_convention_kinds_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_convention_kinds_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_convention_kinds_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_convention_kinds_response>();
}

TEST_CASE("ores::refdata::messaging::list_convention_kind_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_convention_kind_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_convention_kind_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_convention_kind_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_convention_kind_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_convention_kind_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_convention_kind_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_convention_kind_version_response>();
}
