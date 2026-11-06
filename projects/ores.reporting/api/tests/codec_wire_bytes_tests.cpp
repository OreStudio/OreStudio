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
#include "ores.reporting.api/domain/analytic_type_codec.hpp"
#include "ores.reporting.api/domain/concurrency_policy_codec.hpp"
#include "ores.reporting.api/domain/configuration_analytic_codec.hpp"
#include "ores.reporting.api/domain/configuration_analytic_parameter_codec.hpp"
#include "ores.reporting.api/domain/configuration_codec.hpp"
#include "ores.reporting.api/domain/configuration_market_binding_codec.hpp"
#include "ores.reporting.api/domain/configuration_parameter_codec.hpp"
#include "ores.reporting.api/domain/configuration_type_codec.hpp"
#include "ores.reporting.api/domain/cut_approval_detail_codec.hpp"
#include "ores.reporting.api/domain/cut_codec.hpp"
#include "ores.reporting.api/domain/cut_family_codec.hpp"
#include "ores.reporting.api/domain/cut_gap_codec.hpp"
#include "ores.reporting.api/domain/cut_kind_codec.hpp"
#include "ores.reporting.api/domain/cut_resolution_rule_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_building_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_change_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_execution_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_group_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_group_setting_codec.hpp"
#include "ores.reporting.api/domain/file_type_codec.hpp"
#include "ores.reporting.api/domain/ore_file_category_codec.hpp"
#include "ores.reporting.api/domain/ore_file_codec.hpp"
#include "ores.reporting.api/domain/ore_file_version_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_setting_value_set_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_domain_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_set_catalogue_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_set_codec.hpp"
#include "ores.reporting.api/domain/ore_parameter_value_set_value_codec.hpp"
#include "ores.reporting.api/domain/report_cashflow_codec.hpp"
#include "ores.reporting.api/domain/report_configuration_codec.hpp"
#include "ores.reporting.api/domain/report_definition_codec.hpp"
#include "ores.reporting.api/domain/report_definition_template.hpp"
#include "ores.reporting.api/domain/report_error_event_codec.hpp"
#include "ores.reporting.api/domain/report_error_scope_codec.hpp"
#include "ores.reporting.api/domain/report_instance_codec.hpp"
#include "ores.reporting.api/domain/report_instance_kind_codec.hpp"
#include "ores.reporting.api/domain/report_metric_codec.hpp"
#include "ores.reporting.api/domain/report_trigger_event_codec.hpp"
#include "ores.reporting.api/domain/report_trigger_type_codec.hpp"
#include "ores.reporting.api/domain/report_type_codec.hpp"
#include "ores.reporting.api/domain/report_type_configuration_type_codec.hpp"
#include "ores.reporting.api/domain/risk_report_config_codec.hpp"
#include "ores.reporting.api/domain/run_result_codec.hpp"
#include "ores.reporting.api/generators/analytic_type_generator.hpp"
#include "ores.reporting.api/generators/concurrency_policy_generator.hpp"
#include "ores.reporting.api/generators/configuration_analytic_generator.hpp"
#include "ores.reporting.api/generators/configuration_analytic_parameter_generator.hpp"
#include "ores.reporting.api/generators/configuration_generator.hpp"
#include "ores.reporting.api/generators/configuration_market_binding_generator.hpp"
#include "ores.reporting.api/generators/configuration_parameter_generator.hpp"
#include "ores.reporting.api/generators/configuration_type_generator.hpp"
#include "ores.reporting.api/generators/cut_approval_detail_generator.hpp"
#include "ores.reporting.api/generators/cut_family_generator.hpp"
#include "ores.reporting.api/generators/cut_gap_generator.hpp"
#include "ores.reporting.api/generators/cut_generator.hpp"
#include "ores.reporting.api/generators/cut_kind_generator.hpp"
#include "ores.reporting.api/generators/cut_resolution_rule_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_building_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_change_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_execution_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_fixings_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_group_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_group_setting_generator.hpp"
#include "ores.reporting.api/generators/file_type_generator.hpp"
#include "ores.reporting.api/generators/ore_file_category_generator.hpp"
#include "ores.reporting.api/generators/ore_file_generator.hpp"
#include "ores.reporting.api/generators/ore_file_version_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_setting_value_set_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_domain_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_set_catalogue_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_set_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_set_value_generator.hpp"
#include "ores.reporting.api/generators/report_cashflow_generator.hpp"
#include "ores.reporting.api/generators/report_configuration_generator.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/generators/report_error_event_generator.hpp"
#include "ores.reporting.api/generators/report_error_scope_generator.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.api/generators/report_instance_kind_generator.hpp"
#include "ores.reporting.api/generators/report_metric_generator.hpp"
#include "ores.reporting.api/generators/report_trigger_event_generator.hpp"
#include "ores.reporting.api/generators/report_trigger_type_generator.hpp"
#include "ores.reporting.api/generators/report_type_configuration_type_generator.hpp"
#include "ores.reporting.api/generators/report_type_generator.hpp"
#include "ores.reporting.api/generators/risk_report_config_generator.hpp"
#include "ores.reporting.api/generators/run_result_generator.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == ores::utility::codec::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

}

TEST_CASE("every reporting domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_analytic_type(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_concurrency_policy(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_configuration(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_configuration_analytic(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_configuration_analytic_parameter(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_configuration_market_binding(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_configuration_parameter(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_configuration_type(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_cut(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_cut_approval_detail(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_cut_family(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_cut_gap(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_cut_kind(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_cut_resolution_rule(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_engine_settings(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_building(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_change(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_execution(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_fixings(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_group(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_engine_settings_group_setting(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_file_type(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_ore_file(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_file_version(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_file_category(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_ore_parameter(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_parameter_setting_value_set(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_parameter_value_domain(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_parameter_value_set(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_parameter_value_set_catalogue(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_ore_parameter_value_set_value(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_report_cashflow(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_configuration(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_definition(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_error_event(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_error_scope(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_report_instance(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_instance_kind(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_report_metric(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_trigger_event(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_trigger_type(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_report_type(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_report_type_configuration_type(ctx));
    same_bytes_as_reflect_cpp(
        ores::reporting::generators::generate_synthetic_risk_report_config(ctx));
    same_bytes_as_reflect_cpp(ores::reporting::generators::generate_synthetic_run_result(ctx));
}

TEST_CASE("every reporting operation message's default has reflect-cpp's bytes", codec_tags) {
    using namespace ores::reporting::messaging;
    every_default_has_the_same_bytes<engine_settings_value_change,
                                     engine_settings_change_line,
                                     engine_settings_value,
                                     raise_engine_settings_change_request,
                                     raise_engine_settings_change_response,
                                     get_engine_settings_changes_request,
                                     get_engine_settings_changes_response,
                                     recheck_engine_settings_changes_request,
                                     recheck_engine_settings_changes_response,
                                     trigger_report_instance_request,
                                     trigger_report_instance_response,
                                     get_run_health_request,
                                     get_run_health_response,
                                     record_run_pricing_request,
                                     record_run_pricing_response,
                                     schedule_report_definitions_request,
                                     schedule_report_definitions_response,
                                     unschedule_report_definitions_request,
                                     unschedule_report_definitions_response,
                                     publish_report_definitions_from_dq_request,
                                     publish_risk_report_configs_from_dq_request,
                                     publish_engine_settings_from_dq_request,
                                     publish_ore_parameter_value_domains_from_dq_request,
                                     publish_ore_parameters_from_dq_request,
                                     gather_trades_request,
                                     gather_trades_result,
                                     gather_market_data_request,
                                     gather_market_data_result,
                                     assemble_bundle_request,
                                     assemble_bundle_result,
                                     prepare_ore_package_request,
                                     prepare_ore_package_result,
                                     submit_compute_request,
                                     submit_compute_result,
                                     collect_compute_results_request,
                                     collect_compute_results_result,
                                     post_process_results_request,
                                     post_process_results_result,
                                     finalise_report_request,
                                     finalise_report_result,
                                     fail_report_request,
                                     fail_report_result,
                                     report_execution_request,
                                     resolve_prepared_input_request,
                                     ignore_compute_results_request,
                                     definition_next_fire,
                                     get_next_fire_request,
                                     get_next_fire_response,
                                     resolve_cut_request,
                                     resolve_cut_response,
                                     list_cut_rows_request,
                                     cut_row,
                                     list_cut_rows_response,
                                     get_cut_record_request,
                                     get_cut_record_response,
                                     take_cut_request,
                                     cut_body_part,
                                     take_cut_response,
                                     run_parameter,
                                     run_analytic,
                                     run_file,
                                     run_engine_settings,
                                     run_document,
                                     save_run_document_request,
                                     save_run_document_response,
                                     bind_configuration_request,
                                     bind_configuration_response,
                                     get_run_document_request,
                                     get_run_document_response,
                                     delete_run_document_request,
                                     delete_run_document_response>();
}

TEST_CASE("a report definition template has reflect-cpp's bytes", codec_tags) {
    namespace object = ores::utility::codec::object;
    const ores::reporting::domain::report_definition_template t{
        "Daily risk", "End of day", "risk", "0 18 * * 1-5", "skip", 3};
    CHECK(object::write_json(t) == ores::utility::codec::json::write(t));
    CHECK(object::write_msgpack(t) == their_msgpack(t));
    CHECK(object::write_json(object::read_json<ores::reporting::domain::report_definition_template>(
              object::write_json(t))) == object::write_json(t));
}
