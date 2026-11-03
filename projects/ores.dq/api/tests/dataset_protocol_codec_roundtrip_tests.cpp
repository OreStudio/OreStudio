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
#include "ores.dq.api/domain/dataset_codec.hpp"
#include "ores.dq.api/messaging/dataset_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::dq::messaging::dataset_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_key>();
}

TEST_CASE("ores::dq::messaging::dataset_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_write>();
}

TEST_CASE("ores::dq::messaging::dataset_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_change>();
}

TEST_CASE("ores::dq::messaging::dataset_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_removal>();
}

TEST_CASE("ores::dq::messaging::dataset_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_lookup>();
}

TEST_CASE("ores::dq::messaging::datasets_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::datasets_filter>();
}

TEST_CASE("ores::dq::messaging::dataset_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_event>();
}

TEST_CASE("ores::dq::messaging::dataset_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_version_key>();
}

TEST_CASE("ores::dq::messaging::dataset_versions_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::dataset_versions_filter>();
}

TEST_CASE("ores::dq::messaging::list_datasets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::list_datasets_request>();
}

TEST_CASE("ores::dq::messaging::list_datasets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::list_datasets_response>();
}

TEST_CASE("ores::dq::messaging::get_dataset_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_dataset_request>();
}

TEST_CASE("ores::dq::messaging::get_dataset_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_dataset_response>();
}

TEST_CASE("ores::dq::messaging::get_many_datasets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_many_datasets_request>();
}

TEST_CASE("ores::dq::messaging::get_many_datasets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_many_datasets_response>();
}

TEST_CASE("ores::dq::messaging::put_dataset_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::put_dataset_request>();
}

TEST_CASE("ores::dq::messaging::put_dataset_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::put_dataset_response>();
}

TEST_CASE("ores::dq::messaging::put_many_datasets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::put_many_datasets_request>();
}

TEST_CASE("ores::dq::messaging::put_many_datasets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::put_many_datasets_response>();
}

TEST_CASE("ores::dq::messaging::delete_dataset_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::delete_dataset_request>();
}

TEST_CASE("ores::dq::messaging::delete_dataset_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::delete_dataset_response>();
}

TEST_CASE("ores::dq::messaging::delete_many_datasets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::delete_many_datasets_request>();
}

TEST_CASE("ores::dq::messaging::delete_many_datasets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::delete_many_datasets_response>();
}

TEST_CASE("ores::dq::messaging::list_dataset_versions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::list_dataset_versions_request>();
}

TEST_CASE("ores::dq::messaging::list_dataset_versions_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::list_dataset_versions_response>();
}

TEST_CASE("ores::dq::messaging::get_dataset_version_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_dataset_version_request>();
}

TEST_CASE("ores::dq::messaging::get_dataset_version_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::dq::messaging::get_dataset_version_response>();
}
