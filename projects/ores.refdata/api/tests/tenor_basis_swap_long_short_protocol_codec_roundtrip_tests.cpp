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
#include "ores.refdata.api/domain/tenor_basis_swap_long_short_codec.hpp"
#include "ores.refdata.api/messaging/tenor_basis_swap_long_short_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_key>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_write>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_change>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_removal>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_lookup>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_shorts_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_shorts_filter>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_event>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_version_key>();
}

TEST_CASE("ores::refdata::messaging::tenor_basis_swap_long_short_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::tenor_basis_swap_long_short_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_tenor_basis_swap_long_shorts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_tenor_basis_swap_long_shorts_request>();
}

TEST_CASE("ores::refdata::messaging::list_tenor_basis_swap_long_shorts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_tenor_basis_swap_long_shorts_response>();
}

TEST_CASE("ores::refdata::messaging::get_tenor_basis_swap_long_short_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_tenor_basis_swap_long_short_request>();
}

TEST_CASE("ores::refdata::messaging::get_tenor_basis_swap_long_short_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_tenor_basis_swap_long_short_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_tenor_basis_swap_long_shorts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_tenor_basis_swap_long_shorts_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_tenor_basis_swap_long_shorts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_tenor_basis_swap_long_shorts_response>();
}

TEST_CASE("ores::refdata::messaging::put_tenor_basis_swap_long_short_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_tenor_basis_swap_long_short_request>();
}

TEST_CASE("ores::refdata::messaging::put_tenor_basis_swap_long_short_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_tenor_basis_swap_long_short_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_tenor_basis_swap_long_shorts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_tenor_basis_swap_long_shorts_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_tenor_basis_swap_long_shorts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_tenor_basis_swap_long_shorts_response>();
}

TEST_CASE("ores::refdata::messaging::delete_tenor_basis_swap_long_short_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_tenor_basis_swap_long_short_request>();
}

TEST_CASE("ores::refdata::messaging::delete_tenor_basis_swap_long_short_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_tenor_basis_swap_long_short_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_tenor_basis_swap_long_shorts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_tenor_basis_swap_long_shorts_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_tenor_basis_swap_long_shorts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_tenor_basis_swap_long_shorts_response>();
}

TEST_CASE("ores::refdata::messaging::list_by_convention_id_tenor_basis_swap_long_shorts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_by_convention_id_tenor_basis_swap_long_shorts_request>();
}

TEST_CASE("ores::refdata::messaging::list_by_convention_id_tenor_basis_swap_long_shorts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_by_convention_id_tenor_basis_swap_long_shorts_response>();
}

TEST_CASE("ores::refdata::messaging::list_tenor_basis_swap_long_short_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_tenor_basis_swap_long_short_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_tenor_basis_swap_long_short_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_tenor_basis_swap_long_short_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_tenor_basis_swap_long_short_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_tenor_basis_swap_long_short_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_tenor_basis_swap_long_short_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_tenor_basis_swap_long_short_version_response>();
}
