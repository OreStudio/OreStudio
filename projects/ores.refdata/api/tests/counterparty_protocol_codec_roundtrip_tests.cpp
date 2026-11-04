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
#include "ores.refdata.api/domain/counterparty_change_codec.hpp"
#include "ores.refdata.api/domain/counterparty_codec.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information_codec.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_codec.hpp"
#include "ores.refdata.api/domain/csa_codec.hpp"
#include "ores.refdata.api/domain/csa_eligible_currency_codec.hpp"
#include "ores.refdata.api/domain/netting_agreement_codec.hpp"
#include "ores.refdata.api/domain/netting_set_codec.hpp"
#include "ores.refdata.api/domain/netting_set_identifier_codec.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::refdata::messaging::counterparty_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_key>();
}

TEST_CASE("ores::refdata::messaging::counterparty_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_write>();
}

TEST_CASE("ores::refdata::messaging::counterparty_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_change>();
}

TEST_CASE("ores::refdata::messaging::counterparty_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_removal>();
}

TEST_CASE("ores::refdata::messaging::counterparty_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_lookup>();
}

TEST_CASE("ores::refdata::messaging::counterparties_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparties_filter>();
}

TEST_CASE("ores::refdata::messaging::counterparty_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_event>();
}

TEST_CASE("ores::refdata::messaging::counterparty_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_version_key>();
}

TEST_CASE("ores::refdata::messaging::counterparty_versions_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::counterparty_versions_filter>();
}

TEST_CASE("ores::refdata::messaging::list_counterparties_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_counterparties_request>();
}

TEST_CASE("ores::refdata::messaging::list_counterparties_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::list_counterparties_response>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_counterparty_request>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::get_counterparty_response>();
}

TEST_CASE("ores::refdata::messaging::get_many_counterparties_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_counterparties_request>();
}

TEST_CASE("ores::refdata::messaging::get_many_counterparties_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_many_counterparties_response>();
}

TEST_CASE("ores::refdata::messaging::put_counterparty_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_counterparty_request>();
}

TEST_CASE("ores::refdata::messaging::put_counterparty_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::put_counterparty_response>();
}

TEST_CASE("ores::refdata::messaging::put_many_counterparties_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_counterparties_request>();
}

TEST_CASE("ores::refdata::messaging::put_many_counterparties_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_many_counterparties_response>();
}

TEST_CASE("ores::refdata::messaging::delete_counterparty_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_counterparty_request>();
}

TEST_CASE("ores::refdata::messaging::delete_counterparty_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::refdata::messaging::delete_counterparty_response>();
}

TEST_CASE("ores::refdata::messaging::delete_many_counterparties_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_counterparties_request>();
}

TEST_CASE("ores::refdata::messaging::delete_many_counterparties_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::delete_many_counterparties_response>();
}

TEST_CASE("ores::refdata::messaging::list_counterparty_versions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_counterparty_versions_request>();
}

TEST_CASE("ores::refdata::messaging::list_counterparty_versions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::list_counterparty_versions_response>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_version_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_counterparty_version_request>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_version_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_counterparty_version_response>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_composite_as_of_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_counterparty_composite_as_of_request>();
}

TEST_CASE("ores::refdata::messaging::get_counterparty_composite_as_of_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::get_counterparty_composite_as_of_response>();
}

TEST_CASE("ores::refdata::messaging::put_counterparty_composite_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_counterparty_composite_request>();
}

TEST_CASE("ores::refdata::messaging::put_counterparty_composite_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::refdata::messaging::put_counterparty_composite_response>();
}
