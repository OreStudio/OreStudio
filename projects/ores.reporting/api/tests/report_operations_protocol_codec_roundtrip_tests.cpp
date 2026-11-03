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
#include "ores.reporting.api/domain/cut_approval_detail_codec.hpp"
#include "ores.reporting.api/domain/cut_codec.hpp"
#include "ores.reporting.api/domain/cut_gap_codec.hpp"
#include "ores.reporting.api/domain/cut_kind_codec.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::reporting::messaging::trigger_report_instance_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::trigger_report_instance_request>();
}

TEST_CASE("ores::reporting::messaging::trigger_report_instance_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::trigger_report_instance_response>();
}

TEST_CASE("ores::reporting::messaging::get_run_health_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_run_health_request>();
}

TEST_CASE("ores::reporting::messaging::get_run_health_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_run_health_response>();
}

TEST_CASE("ores::reporting::messaging::record_run_pricing_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::record_run_pricing_request>();
}

TEST_CASE("ores::reporting::messaging::record_run_pricing_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::record_run_pricing_response>();
}

TEST_CASE("ores::reporting::messaging::schedule_report_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::schedule_report_definitions_request>();
}

TEST_CASE("ores::reporting::messaging::schedule_report_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::schedule_report_definitions_response>();
}

TEST_CASE("ores::reporting::messaging::unschedule_report_definitions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::unschedule_report_definitions_request>();
}

TEST_CASE("ores::reporting::messaging::unschedule_report_definitions_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::unschedule_report_definitions_response>();
}

TEST_CASE("ores::reporting::messaging::publish_report_definitions_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::publish_report_definitions_from_dq_request>();
}

TEST_CASE("ores::reporting::messaging::publish_risk_report_configs_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::publish_risk_report_configs_from_dq_request>();
}

TEST_CASE("ores::reporting::messaging::publish_engine_settings_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::publish_engine_settings_from_dq_request>();
}

TEST_CASE("ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>();
}

TEST_CASE("ores::reporting::messaging::publish_ore_parameters_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::publish_ore_parameters_from_dq_request>();
}

TEST_CASE("ores::reporting::messaging::gather_trades_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::gather_trades_request>();
}

TEST_CASE("ores::reporting::messaging::gather_trades_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::gather_trades_result>();
}

TEST_CASE("ores::reporting::messaging::gather_market_data_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::gather_market_data_request>();
}

TEST_CASE("ores::reporting::messaging::gather_market_data_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::gather_market_data_result>();
}

TEST_CASE("ores::reporting::messaging::assemble_bundle_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::assemble_bundle_request>();
}

TEST_CASE("ores::reporting::messaging::assemble_bundle_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::assemble_bundle_result>();
}

TEST_CASE("ores::reporting::messaging::prepare_ore_package_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::prepare_ore_package_request>();
}

TEST_CASE("ores::reporting::messaging::prepare_ore_package_result",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::prepare_ore_package_result>();
}

TEST_CASE("ores::reporting::messaging::submit_compute_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::submit_compute_request>();
}

TEST_CASE("ores::reporting::messaging::submit_compute_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::submit_compute_result>();
}

TEST_CASE("ores::reporting::messaging::collect_compute_results_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::collect_compute_results_request>();
}

TEST_CASE("ores::reporting::messaging::collect_compute_results_result",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::collect_compute_results_result>();
}

TEST_CASE("ores::reporting::messaging::post_process_results_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::post_process_results_request>();
}

TEST_CASE("ores::reporting::messaging::post_process_results_result",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::post_process_results_result>();
}

TEST_CASE("ores::reporting::messaging::finalise_report_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::finalise_report_request>();
}

TEST_CASE("ores::reporting::messaging::finalise_report_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::finalise_report_result>();
}

TEST_CASE("ores::reporting::messaging::fail_report_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::fail_report_request>();
}

TEST_CASE("ores::reporting::messaging::fail_report_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::fail_report_result>();
}

TEST_CASE("ores::reporting::messaging::report_execution_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::report_execution_request>();
}

TEST_CASE("ores::reporting::messaging::resolve_prepared_input_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::resolve_prepared_input_request>();
}

TEST_CASE("ores::reporting::messaging::ignore_compute_results_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::ignore_compute_results_request>();
}

TEST_CASE("ores::reporting::messaging::definition_next_fire", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::definition_next_fire>();
}

TEST_CASE("ores::reporting::messaging::get_next_fire_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_next_fire_request>();
}

TEST_CASE("ores::reporting::messaging::get_next_fire_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_next_fire_response>();
}

TEST_CASE("ores::reporting::messaging::resolve_cut_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::resolve_cut_request>();
}

TEST_CASE("ores::reporting::messaging::resolve_cut_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::resolve_cut_response>();
}

TEST_CASE("ores::reporting::messaging::list_cut_rows_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::list_cut_rows_request>();
}

TEST_CASE("ores::reporting::messaging::cut_row", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::cut_row>();
}

TEST_CASE("ores::reporting::messaging::list_cut_rows_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::list_cut_rows_response>();
}

TEST_CASE("ores::reporting::messaging::get_cut_record_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_cut_record_request>();
}

TEST_CASE("ores::reporting::messaging::get_cut_record_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::get_cut_record_response>();
}

TEST_CASE("ores::reporting::messaging::take_cut_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::take_cut_request>();
}

TEST_CASE("ores::reporting::messaging::cut_body_part", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::cut_body_part>();
}

TEST_CASE("ores::reporting::messaging::cut_authorisation_subject", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::cut_authorisation_subject>();
}

TEST_CASE("ores::reporting::messaging::list_cut_authorisation_subjects_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_cut_authorisation_subjects_request>();
}

TEST_CASE("ores::reporting::messaging::list_cut_authorisation_subjects_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::reporting::messaging::list_cut_authorisation_subjects_response>();
}

TEST_CASE("ores::reporting::messaging::take_cut_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::reporting::messaging::take_cut_response>();
}
