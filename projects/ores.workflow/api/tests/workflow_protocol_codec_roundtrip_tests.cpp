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
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::workflow::messaging::step_log_entry", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::step_log_entry>();
}

TEST_CASE("ores::workflow::messaging::step_completed_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::step_completed_event>();
}

TEST_CASE("ores::workflow::messaging::start_workflow_message", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::start_workflow_message>();
}

TEST_CASE("ores::workflow::messaging::get_step_result_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::get_step_result_request>();
}

TEST_CASE("ores::workflow::messaging::get_step_result_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::get_step_result_response>();
}

TEST_CASE("ores::workflow::messaging::workflow_instance_summary", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::workflow_instance_summary>();
}

TEST_CASE("ores::workflow::messaging::list_workflow_instance_summaries_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::list_workflow_instance_summaries_request>();
}

TEST_CASE("ores::workflow::messaging::list_workflow_instance_summaries_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::list_workflow_instance_summaries_response>();
}

TEST_CASE("ores::workflow::messaging::workflow_step_summary", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::workflow_step_summary>();
}

TEST_CASE("ores::workflow::messaging::get_workflow_steps_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::get_workflow_steps_request>();
}

TEST_CASE("ores::workflow::messaging::get_workflow_steps_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::get_workflow_steps_response>();
}

TEST_CASE("ores::workflow::messaging::workflow_step_definition_summary",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::workflow_step_definition_summary>();
}

TEST_CASE("ores::workflow::messaging::workflow_definition_summary",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::workflow::messaging::workflow_definition_summary>();
}

TEST_CASE("ores::workflow::messaging::list_workflow_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::list_workflow_definitions_request>();
}

TEST_CASE("ores::workflow::messaging::list_workflow_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::list_workflow_definitions_response>();
}

TEST_CASE("ores::workflow::messaging::retry_workflow_instance_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::retry_workflow_instance_request>();
}

TEST_CASE("ores::workflow::messaging::retry_workflow_instance_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::workflow::messaging::retry_workflow_instance_response>();
}
