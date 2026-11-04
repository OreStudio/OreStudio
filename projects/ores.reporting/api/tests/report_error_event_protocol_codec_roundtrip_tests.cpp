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
#include "ores.reporting.api/domain/report_error_event_codec.hpp"
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::reporting::messaging::report_error_event_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_error_event_key>();
}

TEST_CASE("ores::reporting::messaging::report_error_event_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_error_event_lookup>();
}

TEST_CASE("ores::reporting::messaging::report_error_events_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_error_events_filter>();
}

TEST_CASE("ores::reporting::messaging::report_error_event_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_error_event_event>();
}

TEST_CASE("ores::reporting::messaging::list_report_error_events_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_report_error_events_request>();
}

TEST_CASE("ores::reporting::messaging::list_report_error_events_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_report_error_events_response>();
}

TEST_CASE("ores::reporting::messaging::get_report_error_event_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_report_error_event_request>();
}

TEST_CASE("ores::reporting::messaging::get_report_error_event_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_report_error_event_response>();
}

TEST_CASE("ores::reporting::messaging::get_many_report_error_events_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_report_error_events_request>();
}

TEST_CASE("ores::reporting::messaging::get_many_report_error_events_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_many_report_error_events_response>();
}

TEST_CASE("ores::reporting::messaging::list_by_report_instance_id_report_error_events_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_request>();
}

TEST_CASE("ores::reporting::messaging::list_by_report_instance_id_report_error_events_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_by_report_instance_id_report_error_events_response>();
}

TEST_CASE("ores::reporting::messaging::report_error_count", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_error_count>();
}

TEST_CASE("ores::reporting::messaging::get_report_error_counts_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_report_error_counts_request>();
}

TEST_CASE("ores::reporting::messaging::get_report_error_counts_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::get_report_error_counts_response>();
}
