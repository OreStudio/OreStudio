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
 * Template: cpp_protocol_codec.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.reporting.api/domain/cut_approval_detail_codec.hpp"
#include "ores.reporting.api/domain/cut_codec.hpp"
#include "ores.reporting.api/domain/cut_gap_codec.hpp"
#include "ores.reporting.api/domain/cut_kind_codec.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::trigger_report_instance_request>::write_json(
    const ores::reporting::messaging::trigger_report_instance_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::trigger_report_instance_request
wire_traits<ores::reporting::messaging::trigger_report_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::trigger_report_instance_request>(text);
}

std::string wire_traits<ores::reporting::messaging::trigger_report_instance_request>::write_msgpack(
    const ores::reporting::messaging::trigger_report_instance_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::trigger_report_instance_request
wire_traits<ores::reporting::messaging::trigger_report_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::trigger_report_instance_request>(data);
}

std::string wire_traits<ores::reporting::messaging::trigger_report_instance_response>::write_json(
    const ores::reporting::messaging::trigger_report_instance_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::trigger_report_instance_response
wire_traits<ores::reporting::messaging::trigger_report_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::trigger_report_instance_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::trigger_report_instance_response>::write_msgpack(
    const ores::reporting::messaging::trigger_report_instance_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::trigger_report_instance_response
wire_traits<ores::reporting::messaging::trigger_report_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::trigger_report_instance_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_run_health_request>::write_json(
    const ores::reporting::messaging::get_run_health_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_run_health_request
wire_traits<ores::reporting::messaging::get_run_health_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_run_health_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_run_health_request>::write_msgpack(
    const ores::reporting::messaging::get_run_health_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_run_health_request
wire_traits<ores::reporting::messaging::get_run_health_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_run_health_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_run_health_response>::write_json(
    const ores::reporting::messaging::get_run_health_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_run_health_response
wire_traits<ores::reporting::messaging::get_run_health_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_run_health_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_run_health_response>::write_msgpack(
    const ores::reporting::messaging::get_run_health_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_run_health_response
wire_traits<ores::reporting::messaging::get_run_health_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_run_health_response>(data);
}

std::string wire_traits<ores::reporting::messaging::record_run_pricing_request>::write_json(
    const ores::reporting::messaging::record_run_pricing_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::record_run_pricing_request
wire_traits<ores::reporting::messaging::record_run_pricing_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::record_run_pricing_request>(text);
}

std::string wire_traits<ores::reporting::messaging::record_run_pricing_request>::write_msgpack(
    const ores::reporting::messaging::record_run_pricing_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::record_run_pricing_request
wire_traits<ores::reporting::messaging::record_run_pricing_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::record_run_pricing_request>(data);
}

std::string wire_traits<ores::reporting::messaging::record_run_pricing_response>::write_json(
    const ores::reporting::messaging::record_run_pricing_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::record_run_pricing_response
wire_traits<ores::reporting::messaging::record_run_pricing_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::record_run_pricing_response>(text);
}

std::string wire_traits<ores::reporting::messaging::record_run_pricing_response>::write_msgpack(
    const ores::reporting::messaging::record_run_pricing_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::record_run_pricing_response
wire_traits<ores::reporting::messaging::record_run_pricing_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::record_run_pricing_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::schedule_report_definitions_request>::write_json(
    const ores::reporting::messaging::schedule_report_definitions_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::schedule_report_definitions_request
wire_traits<ores::reporting::messaging::schedule_report_definitions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::schedule_report_definitions_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::schedule_report_definitions_request>::write_msgpack(
    const ores::reporting::messaging::schedule_report_definitions_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::schedule_report_definitions_request
wire_traits<ores::reporting::messaging::schedule_report_definitions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::schedule_report_definitions_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::schedule_report_definitions_response>::write_json(
    const ores::reporting::messaging::schedule_report_definitions_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::schedule_report_definitions_response
wire_traits<ores::reporting::messaging::schedule_report_definitions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::schedule_report_definitions_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::schedule_report_definitions_response>::write_msgpack(
    const ores::reporting::messaging::schedule_report_definitions_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::schedule_report_definitions_response
wire_traits<ores::reporting::messaging::schedule_report_definitions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::schedule_report_definitions_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::unschedule_report_definitions_request>::write_json(
    const ores::reporting::messaging::unschedule_report_definitions_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::unschedule_report_definitions_request
wire_traits<ores::reporting::messaging::unschedule_report_definitions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::unschedule_report_definitions_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::unschedule_report_definitions_request>::write_msgpack(
    const ores::reporting::messaging::unschedule_report_definitions_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::unschedule_report_definitions_request
wire_traits<ores::reporting::messaging::unschedule_report_definitions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::unschedule_report_definitions_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::unschedule_report_definitions_response>::write_json(
    const ores::reporting::messaging::unschedule_report_definitions_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::unschedule_report_definitions_response
wire_traits<ores::reporting::messaging::unschedule_report_definitions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::unschedule_report_definitions_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::unschedule_report_definitions_response>::write_msgpack(
    const ores::reporting::messaging::unschedule_report_definitions_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::unschedule_report_definitions_response
wire_traits<ores::reporting::messaging::unschedule_report_definitions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::unschedule_report_definitions_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::publish_report_definitions_from_dq_request>::write_json(
    const ores::reporting::messaging::publish_report_definitions_from_dq_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::publish_report_definitions_from_dq_request
wire_traits<ores::reporting::messaging::publish_report_definitions_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::publish_report_definitions_from_dq_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::publish_report_definitions_from_dq_request>::write_msgpack(
    const ores::reporting::messaging::publish_report_definitions_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::publish_report_definitions_from_dq_request
wire_traits<ores::reporting::messaging::publish_report_definitions_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::publish_report_definitions_from_dq_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::publish_risk_report_configs_from_dq_request>::write_json(
    const ores::reporting::messaging::publish_risk_report_configs_from_dq_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::publish_risk_report_configs_from_dq_request
wire_traits<ores::reporting::messaging::publish_risk_report_configs_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::publish_risk_report_configs_from_dq_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::publish_risk_report_configs_from_dq_request>::write_msgpack(
    const ores::reporting::messaging::publish_risk_report_configs_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::publish_risk_report_configs_from_dq_request
wire_traits<ores::reporting::messaging::publish_risk_report_configs_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::publish_risk_report_configs_from_dq_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::publish_engine_settings_from_dq_request>::write_json(
    const ores::reporting::messaging::publish_engine_settings_from_dq_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::publish_engine_settings_from_dq_request
wire_traits<ores::reporting::messaging::publish_engine_settings_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::publish_engine_settings_from_dq_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::publish_engine_settings_from_dq_request>::write_msgpack(
    const ores::reporting::messaging::publish_engine_settings_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::publish_engine_settings_from_dq_request
wire_traits<ores::reporting::messaging::publish_engine_settings_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::publish_engine_settings_from_dq_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>::
    write_json(
        const ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request
wire_traits<ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>::
    write_msgpack(
        const ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request
wire_traits<ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::publish_ore_parameter_value_domains_from_dq_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::publish_ore_parameters_from_dq_request>::write_json(
    const ores::reporting::messaging::publish_ore_parameters_from_dq_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::publish_ore_parameters_from_dq_request
wire_traits<ores::reporting::messaging::publish_ore_parameters_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::publish_ore_parameters_from_dq_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::publish_ore_parameters_from_dq_request>::write_msgpack(
    const ores::reporting::messaging::publish_ore_parameters_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::publish_ore_parameters_from_dq_request
wire_traits<ores::reporting::messaging::publish_ore_parameters_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::publish_ore_parameters_from_dq_request>(
        data);
}

std::string wire_traits<ores::reporting::messaging::gather_trades_request>::write_json(
    const ores::reporting::messaging::gather_trades_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::gather_trades_request
wire_traits<ores::reporting::messaging::gather_trades_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::gather_trades_request>(text);
}

std::string wire_traits<ores::reporting::messaging::gather_trades_request>::write_msgpack(
    const ores::reporting::messaging::gather_trades_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::gather_trades_request
wire_traits<ores::reporting::messaging::gather_trades_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::gather_trades_request>(data);
}

std::string wire_traits<ores::reporting::messaging::gather_trades_result>::write_json(
    const ores::reporting::messaging::gather_trades_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::gather_trades_result
wire_traits<ores::reporting::messaging::gather_trades_result>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::gather_trades_result>(text);
}

std::string wire_traits<ores::reporting::messaging::gather_trades_result>::write_msgpack(
    const ores::reporting::messaging::gather_trades_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::gather_trades_result
wire_traits<ores::reporting::messaging::gather_trades_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::gather_trades_result>(data);
}

std::string wire_traits<ores::reporting::messaging::gather_market_data_request>::write_json(
    const ores::reporting::messaging::gather_market_data_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::gather_market_data_request
wire_traits<ores::reporting::messaging::gather_market_data_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::gather_market_data_request>(text);
}

std::string wire_traits<ores::reporting::messaging::gather_market_data_request>::write_msgpack(
    const ores::reporting::messaging::gather_market_data_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::gather_market_data_request
wire_traits<ores::reporting::messaging::gather_market_data_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::gather_market_data_request>(data);
}

std::string wire_traits<ores::reporting::messaging::gather_market_data_result>::write_json(
    const ores::reporting::messaging::gather_market_data_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::gather_market_data_result
wire_traits<ores::reporting::messaging::gather_market_data_result>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::gather_market_data_result>(text);
}

std::string wire_traits<ores::reporting::messaging::gather_market_data_result>::write_msgpack(
    const ores::reporting::messaging::gather_market_data_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::gather_market_data_result
wire_traits<ores::reporting::messaging::gather_market_data_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::gather_market_data_result>(data);
}

std::string wire_traits<ores::reporting::messaging::assemble_bundle_request>::write_json(
    const ores::reporting::messaging::assemble_bundle_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::assemble_bundle_request
wire_traits<ores::reporting::messaging::assemble_bundle_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::assemble_bundle_request>(text);
}

std::string wire_traits<ores::reporting::messaging::assemble_bundle_request>::write_msgpack(
    const ores::reporting::messaging::assemble_bundle_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::assemble_bundle_request
wire_traits<ores::reporting::messaging::assemble_bundle_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::assemble_bundle_request>(data);
}

std::string wire_traits<ores::reporting::messaging::assemble_bundle_result>::write_json(
    const ores::reporting::messaging::assemble_bundle_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::assemble_bundle_result
wire_traits<ores::reporting::messaging::assemble_bundle_result>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::assemble_bundle_result>(text);
}

std::string wire_traits<ores::reporting::messaging::assemble_bundle_result>::write_msgpack(
    const ores::reporting::messaging::assemble_bundle_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::assemble_bundle_result
wire_traits<ores::reporting::messaging::assemble_bundle_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::assemble_bundle_result>(data);
}

std::string wire_traits<ores::reporting::messaging::prepare_ore_package_request>::write_json(
    const ores::reporting::messaging::prepare_ore_package_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::prepare_ore_package_request
wire_traits<ores::reporting::messaging::prepare_ore_package_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::prepare_ore_package_request>(text);
}

std::string wire_traits<ores::reporting::messaging::prepare_ore_package_request>::write_msgpack(
    const ores::reporting::messaging::prepare_ore_package_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::prepare_ore_package_request
wire_traits<ores::reporting::messaging::prepare_ore_package_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::prepare_ore_package_request>(data);
}

std::string wire_traits<ores::reporting::messaging::prepare_ore_package_result>::write_json(
    const ores::reporting::messaging::prepare_ore_package_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::prepare_ore_package_result
wire_traits<ores::reporting::messaging::prepare_ore_package_result>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::prepare_ore_package_result>(text);
}

std::string wire_traits<ores::reporting::messaging::prepare_ore_package_result>::write_msgpack(
    const ores::reporting::messaging::prepare_ore_package_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::prepare_ore_package_result
wire_traits<ores::reporting::messaging::prepare_ore_package_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::prepare_ore_package_result>(data);
}

std::string wire_traits<ores::reporting::messaging::submit_compute_request>::write_json(
    const ores::reporting::messaging::submit_compute_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::submit_compute_request
wire_traits<ores::reporting::messaging::submit_compute_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::submit_compute_request>(text);
}

std::string wire_traits<ores::reporting::messaging::submit_compute_request>::write_msgpack(
    const ores::reporting::messaging::submit_compute_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::submit_compute_request
wire_traits<ores::reporting::messaging::submit_compute_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::submit_compute_request>(data);
}

std::string wire_traits<ores::reporting::messaging::submit_compute_result>::write_json(
    const ores::reporting::messaging::submit_compute_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::submit_compute_result
wire_traits<ores::reporting::messaging::submit_compute_result>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::submit_compute_result>(text);
}

std::string wire_traits<ores::reporting::messaging::submit_compute_result>::write_msgpack(
    const ores::reporting::messaging::submit_compute_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::submit_compute_result
wire_traits<ores::reporting::messaging::submit_compute_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::submit_compute_result>(data);
}

std::string wire_traits<ores::reporting::messaging::collect_compute_results_request>::write_json(
    const ores::reporting::messaging::collect_compute_results_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::collect_compute_results_request
wire_traits<ores::reporting::messaging::collect_compute_results_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::collect_compute_results_request>(text);
}

std::string wire_traits<ores::reporting::messaging::collect_compute_results_request>::write_msgpack(
    const ores::reporting::messaging::collect_compute_results_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::collect_compute_results_request
wire_traits<ores::reporting::messaging::collect_compute_results_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::collect_compute_results_request>(data);
}

std::string wire_traits<ores::reporting::messaging::collect_compute_results_result>::write_json(
    const ores::reporting::messaging::collect_compute_results_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::collect_compute_results_result
wire_traits<ores::reporting::messaging::collect_compute_results_result>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::collect_compute_results_result>(text);
}

std::string wire_traits<ores::reporting::messaging::collect_compute_results_result>::write_msgpack(
    const ores::reporting::messaging::collect_compute_results_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::collect_compute_results_result
wire_traits<ores::reporting::messaging::collect_compute_results_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::collect_compute_results_result>(data);
}

std::string wire_traits<ores::reporting::messaging::post_process_results_request>::write_json(
    const ores::reporting::messaging::post_process_results_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::post_process_results_request
wire_traits<ores::reporting::messaging::post_process_results_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::post_process_results_request>(text);
}

std::string wire_traits<ores::reporting::messaging::post_process_results_request>::write_msgpack(
    const ores::reporting::messaging::post_process_results_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::post_process_results_request
wire_traits<ores::reporting::messaging::post_process_results_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::post_process_results_request>(data);
}

std::string wire_traits<ores::reporting::messaging::post_process_results_result>::write_json(
    const ores::reporting::messaging::post_process_results_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::post_process_results_result
wire_traits<ores::reporting::messaging::post_process_results_result>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::post_process_results_result>(text);
}

std::string wire_traits<ores::reporting::messaging::post_process_results_result>::write_msgpack(
    const ores::reporting::messaging::post_process_results_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::post_process_results_result
wire_traits<ores::reporting::messaging::post_process_results_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::post_process_results_result>(data);
}

std::string wire_traits<ores::reporting::messaging::finalise_report_request>::write_json(
    const ores::reporting::messaging::finalise_report_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::finalise_report_request
wire_traits<ores::reporting::messaging::finalise_report_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::finalise_report_request>(text);
}

std::string wire_traits<ores::reporting::messaging::finalise_report_request>::write_msgpack(
    const ores::reporting::messaging::finalise_report_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::finalise_report_request
wire_traits<ores::reporting::messaging::finalise_report_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::finalise_report_request>(data);
}

std::string wire_traits<ores::reporting::messaging::finalise_report_result>::write_json(
    const ores::reporting::messaging::finalise_report_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::finalise_report_result
wire_traits<ores::reporting::messaging::finalise_report_result>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::finalise_report_result>(text);
}

std::string wire_traits<ores::reporting::messaging::finalise_report_result>::write_msgpack(
    const ores::reporting::messaging::finalise_report_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::finalise_report_result
wire_traits<ores::reporting::messaging::finalise_report_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::finalise_report_result>(data);
}

std::string wire_traits<ores::reporting::messaging::fail_report_request>::write_json(
    const ores::reporting::messaging::fail_report_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::fail_report_request
wire_traits<ores::reporting::messaging::fail_report_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::fail_report_request>(text);
}

std::string wire_traits<ores::reporting::messaging::fail_report_request>::write_msgpack(
    const ores::reporting::messaging::fail_report_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::fail_report_request
wire_traits<ores::reporting::messaging::fail_report_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::fail_report_request>(data);
}

std::string wire_traits<ores::reporting::messaging::fail_report_result>::write_json(
    const ores::reporting::messaging::fail_report_result& v) {
    return object::write_json(v);
}

ores::reporting::messaging::fail_report_result
wire_traits<ores::reporting::messaging::fail_report_result>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::fail_report_result>(text);
}

std::string wire_traits<ores::reporting::messaging::fail_report_result>::write_msgpack(
    const ores::reporting::messaging::fail_report_result& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::fail_report_result
wire_traits<ores::reporting::messaging::fail_report_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::fail_report_result>(data);
}

std::string wire_traits<ores::reporting::messaging::report_execution_request>::write_json(
    const ores::reporting::messaging::report_execution_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_execution_request
wire_traits<ores::reporting::messaging::report_execution_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_execution_request>(text);
}

std::string wire_traits<ores::reporting::messaging::report_execution_request>::write_msgpack(
    const ores::reporting::messaging::report_execution_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_execution_request
wire_traits<ores::reporting::messaging::report_execution_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_execution_request>(data);
}

std::string wire_traits<ores::reporting::messaging::resolve_prepared_input_request>::write_json(
    const ores::reporting::messaging::resolve_prepared_input_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::resolve_prepared_input_request
wire_traits<ores::reporting::messaging::resolve_prepared_input_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::resolve_prepared_input_request>(text);
}

std::string wire_traits<ores::reporting::messaging::resolve_prepared_input_request>::write_msgpack(
    const ores::reporting::messaging::resolve_prepared_input_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::resolve_prepared_input_request
wire_traits<ores::reporting::messaging::resolve_prepared_input_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::resolve_prepared_input_request>(data);
}

std::string wire_traits<ores::reporting::messaging::ignore_compute_results_request>::write_json(
    const ores::reporting::messaging::ignore_compute_results_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ignore_compute_results_request
wire_traits<ores::reporting::messaging::ignore_compute_results_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ignore_compute_results_request>(text);
}

std::string wire_traits<ores::reporting::messaging::ignore_compute_results_request>::write_msgpack(
    const ores::reporting::messaging::ignore_compute_results_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ignore_compute_results_request
wire_traits<ores::reporting::messaging::ignore_compute_results_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::ignore_compute_results_request>(data);
}

std::string wire_traits<ores::reporting::messaging::definition_next_fire>::write_json(
    const ores::reporting::messaging::definition_next_fire& v) {
    return object::write_json(v);
}

ores::reporting::messaging::definition_next_fire
wire_traits<ores::reporting::messaging::definition_next_fire>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::definition_next_fire>(text);
}

std::string wire_traits<ores::reporting::messaging::definition_next_fire>::write_msgpack(
    const ores::reporting::messaging::definition_next_fire& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::definition_next_fire
wire_traits<ores::reporting::messaging::definition_next_fire>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::definition_next_fire>(data);
}

std::string wire_traits<ores::reporting::messaging::get_next_fire_request>::write_json(
    const ores::reporting::messaging::get_next_fire_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_next_fire_request
wire_traits<ores::reporting::messaging::get_next_fire_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_next_fire_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_next_fire_request>::write_msgpack(
    const ores::reporting::messaging::get_next_fire_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_next_fire_request
wire_traits<ores::reporting::messaging::get_next_fire_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_next_fire_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_next_fire_response>::write_json(
    const ores::reporting::messaging::get_next_fire_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_next_fire_response
wire_traits<ores::reporting::messaging::get_next_fire_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_next_fire_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_next_fire_response>::write_msgpack(
    const ores::reporting::messaging::get_next_fire_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_next_fire_response
wire_traits<ores::reporting::messaging::get_next_fire_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_next_fire_response>(data);
}

std::string wire_traits<ores::reporting::messaging::resolve_cut_request>::write_json(
    const ores::reporting::messaging::resolve_cut_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::resolve_cut_request
wire_traits<ores::reporting::messaging::resolve_cut_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::resolve_cut_request>(text);
}

std::string wire_traits<ores::reporting::messaging::resolve_cut_request>::write_msgpack(
    const ores::reporting::messaging::resolve_cut_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::resolve_cut_request
wire_traits<ores::reporting::messaging::resolve_cut_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::resolve_cut_request>(data);
}

std::string wire_traits<ores::reporting::messaging::resolve_cut_response>::write_json(
    const ores::reporting::messaging::resolve_cut_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::resolve_cut_response
wire_traits<ores::reporting::messaging::resolve_cut_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::resolve_cut_response>(text);
}

std::string wire_traits<ores::reporting::messaging::resolve_cut_response>::write_msgpack(
    const ores::reporting::messaging::resolve_cut_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::resolve_cut_response
wire_traits<ores::reporting::messaging::resolve_cut_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::resolve_cut_response>(data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_rows_request>::write_json(
    const ores::reporting::messaging::list_cut_rows_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_rows_request
wire_traits<ores::reporting::messaging::list_cut_rows_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_rows_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_cut_rows_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_rows_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_rows_request
wire_traits<ores::reporting::messaging::list_cut_rows_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_rows_request>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_row>::write_json(
    const ores::reporting::messaging::cut_row& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_row
wire_traits<ores::reporting::messaging::cut_row>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_row>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_row>::write_msgpack(
    const ores::reporting::messaging::cut_row& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_row
wire_traits<ores::reporting::messaging::cut_row>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_row>(data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_rows_response>::write_json(
    const ores::reporting::messaging::list_cut_rows_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_rows_response
wire_traits<ores::reporting::messaging::list_cut_rows_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_rows_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_cut_rows_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_rows_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_rows_response
wire_traits<ores::reporting::messaging::list_cut_rows_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_rows_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_record_request>::write_json(
    const ores::reporting::messaging::get_cut_record_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_record_request
wire_traits<ores::reporting::messaging::get_cut_record_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_record_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_record_request>::write_msgpack(
    const ores::reporting::messaging::get_cut_record_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_record_request
wire_traits<ores::reporting::messaging::get_cut_record_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_record_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_record_response>::write_json(
    const ores::reporting::messaging::get_cut_record_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_record_response
wire_traits<ores::reporting::messaging::get_cut_record_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_record_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_record_response>::write_msgpack(
    const ores::reporting::messaging::get_cut_record_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_record_response
wire_traits<ores::reporting::messaging::get_cut_record_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_record_response>(data);
}

std::string wire_traits<ores::reporting::messaging::take_cut_request>::write_json(
    const ores::reporting::messaging::take_cut_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::take_cut_request
wire_traits<ores::reporting::messaging::take_cut_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::take_cut_request>(text);
}

std::string wire_traits<ores::reporting::messaging::take_cut_request>::write_msgpack(
    const ores::reporting::messaging::take_cut_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::take_cut_request
wire_traits<ores::reporting::messaging::take_cut_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::take_cut_request>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_body_part>::write_json(
    const ores::reporting::messaging::cut_body_part& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_body_part
wire_traits<ores::reporting::messaging::cut_body_part>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_body_part>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_body_part>::write_msgpack(
    const ores::reporting::messaging::cut_body_part& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_body_part
wire_traits<ores::reporting::messaging::cut_body_part>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_body_part>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_authorisation_subject>::write_json(
    const ores::reporting::messaging::cut_authorisation_subject& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_authorisation_subject
wire_traits<ores::reporting::messaging::cut_authorisation_subject>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_authorisation_subject>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_authorisation_subject>::write_msgpack(
    const ores::reporting::messaging::cut_authorisation_subject& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_authorisation_subject
wire_traits<ores::reporting::messaging::cut_authorisation_subject>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_authorisation_subject>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_request>::write_json(
    const ores::reporting::messaging::list_cut_authorisation_subjects_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_authorisation_subjects_request
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_authorisation_subjects_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_authorisation_subjects_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_authorisation_subjects_request
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_cut_authorisation_subjects_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_response>::write_json(
    const ores::reporting::messaging::list_cut_authorisation_subjects_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_authorisation_subjects_response
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_authorisation_subjects_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_authorisation_subjects_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_authorisation_subjects_response
wire_traits<ores::reporting::messaging::list_cut_authorisation_subjects_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_cut_authorisation_subjects_response>(data);
}

std::string wire_traits<ores::reporting::messaging::take_cut_response>::write_json(
    const ores::reporting::messaging::take_cut_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::take_cut_response
wire_traits<ores::reporting::messaging::take_cut_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::take_cut_response>(text);
}

std::string wire_traits<ores::reporting::messaging::take_cut_response>::write_msgpack(
    const ores::reporting::messaging::take_cut_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::take_cut_response
wire_traits<ores::reporting::messaging::take_cut_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::take_cut_response>(data);
}

}
