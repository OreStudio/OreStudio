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
#include "ores.refdata.api/domain/fx_vol_surface_pillar_source_codec.hpp"
#include "ores.refdata.api/messaging/fx_vol_surface_pillar_source_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_key>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_write",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_write>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_change",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_change>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_removal>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_lookup",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_lookup>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_sources_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_sources_filter>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_event",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_event>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_version_key",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_version_key>();
}

TEST_CASE("ores::refdata::messaging::fx_vol_surface_pillar_source_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::fx_vol_surface_pillar_source_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_fx_vol_surface_pillar_sources_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_fx_vol_surface_pillar_sources_request>();
}

TEST_CASE("ores::refdata::messaging::list_fx_vol_surface_pillar_sources_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_fx_vol_surface_pillar_sources_response>();
}

TEST_CASE("ores::refdata::messaging::get_fx_vol_surface_pillar_source_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_fx_vol_surface_pillar_source_request>();
}

TEST_CASE("ores::refdata::messaging::get_fx_vol_surface_pillar_source_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_fx_vol_surface_pillar_source_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_fx_vol_surface_pillar_sources_response>();
}

TEST_CASE("ores::refdata::messaging::put_fx_vol_surface_pillar_source_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_fx_vol_surface_pillar_source_request>();
}

TEST_CASE("ores::refdata::messaging::put_fx_vol_surface_pillar_source_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_fx_vol_surface_pillar_source_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_fx_vol_surface_pillar_sources_response>();
}

TEST_CASE("ores::refdata::messaging::delete_fx_vol_surface_pillar_source_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_fx_vol_surface_pillar_source_request>();
}

TEST_CASE("ores::refdata::messaging::delete_fx_vol_surface_pillar_source_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_fx_vol_surface_pillar_source_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_fx_vol_surface_pillar_sources_response>();
}

TEST_CASE("ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_fx_vol_surface_pillar_source_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_fx_vol_surface_pillar_source_version_response>();
}
