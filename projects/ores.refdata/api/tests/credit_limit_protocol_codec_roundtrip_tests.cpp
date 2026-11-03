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
#include "ores.refdata.api/domain/credit_limit_codec.hpp"
#include "ores.refdata.api/messaging/credit_limit_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::credit_limit_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_key>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_write>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_change>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_removal>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_lookup>();
}

TEST_CASE("ores::refdata::messaging::credit_limits_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limits_filter>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_event>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_version_key>();
}

TEST_CASE("ores::refdata::messaging::credit_limit_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::credit_limit_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_credit_limits_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_credit_limits_request>();
}

TEST_CASE("ores::refdata::messaging::list_credit_limits_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_credit_limits_response>();
}

TEST_CASE("ores::refdata::messaging::get_credit_limit_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_credit_limit_request>();
}

TEST_CASE("ores::refdata::messaging::get_credit_limit_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_credit_limit_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_credit_limits_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_many_credit_limits_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_credit_limits_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_credit_limits_response>();
}

TEST_CASE("ores::refdata::messaging::put_credit_limit_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_credit_limit_request>();
}

TEST_CASE("ores::refdata::messaging::put_credit_limit_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_credit_limit_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_credit_limits_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_many_credit_limits_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_credit_limits_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_credit_limits_response>();
}

TEST_CASE("ores::refdata::messaging::delete_credit_limit_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_credit_limit_request>();
}

TEST_CASE("ores::refdata::messaging::delete_credit_limit_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_credit_limit_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_credit_limits_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_credit_limits_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_credit_limits_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_credit_limits_response>();
}

TEST_CASE("ores::refdata::messaging::list_by_counterparty_id_credit_limits_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_by_counterparty_id_credit_limits_request>();
}

TEST_CASE("ores::refdata::messaging::list_by_counterparty_id_credit_limits_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_by_counterparty_id_credit_limits_response>();
}

TEST_CASE("ores::refdata::messaging::list_credit_limit_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_credit_limit_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_credit_limit_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_credit_limit_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_credit_limit_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_credit_limit_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_credit_limit_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_credit_limit_version_response>();
}
