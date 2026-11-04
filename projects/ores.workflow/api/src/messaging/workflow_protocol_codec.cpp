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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::workflow::messaging::step_log_entry>::write_json(
    const ores::workflow::messaging::step_log_entry& v) {
    return object::write_json(v);
}

ores::workflow::messaging::step_log_entry
wire_traits<ores::workflow::messaging::step_log_entry>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::step_log_entry>(text);
}

std::string wire_traits<ores::workflow::messaging::step_log_entry>::write_msgpack(
    const ores::workflow::messaging::step_log_entry& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::step_log_entry
wire_traits<ores::workflow::messaging::step_log_entry>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::step_log_entry>(data);
}

std::string wire_traits<ores::workflow::messaging::step_completed_event>::write_json(
    const ores::workflow::messaging::step_completed_event& v) {
    return object::write_json(v);
}

ores::workflow::messaging::step_completed_event
wire_traits<ores::workflow::messaging::step_completed_event>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::step_completed_event>(text);
}

std::string wire_traits<ores::workflow::messaging::step_completed_event>::write_msgpack(
    const ores::workflow::messaging::step_completed_event& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::step_completed_event
wire_traits<ores::workflow::messaging::step_completed_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::step_completed_event>(data);
}

std::string wire_traits<ores::workflow::messaging::start_workflow_message>::write_json(
    const ores::workflow::messaging::start_workflow_message& v) {
    return object::write_json(v);
}

ores::workflow::messaging::start_workflow_message
wire_traits<ores::workflow::messaging::start_workflow_message>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::start_workflow_message>(text);
}

std::string wire_traits<ores::workflow::messaging::start_workflow_message>::write_msgpack(
    const ores::workflow::messaging::start_workflow_message& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::start_workflow_message
wire_traits<ores::workflow::messaging::start_workflow_message>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::start_workflow_message>(data);
}

std::string wire_traits<ores::workflow::messaging::get_step_result_request>::write_json(
    const ores::workflow::messaging::get_step_result_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_step_result_request
wire_traits<ores::workflow::messaging::get_step_result_request>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_step_result_request>(text);
}

std::string wire_traits<ores::workflow::messaging::get_step_result_request>::write_msgpack(
    const ores::workflow::messaging::get_step_result_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_step_result_request
wire_traits<ores::workflow::messaging::get_step_result_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_step_result_request>(data);
}

std::string wire_traits<ores::workflow::messaging::get_step_result_response>::write_json(
    const ores::workflow::messaging::get_step_result_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_step_result_response
wire_traits<ores::workflow::messaging::get_step_result_response>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_step_result_response>(text);
}

std::string wire_traits<ores::workflow::messaging::get_step_result_response>::write_msgpack(
    const ores::workflow::messaging::get_step_result_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_step_result_response
wire_traits<ores::workflow::messaging::get_step_result_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_step_result_response>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_summary>::write_json(
    const ores::workflow::messaging::workflow_instance_summary& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_instance_summary
wire_traits<ores::workflow::messaging::workflow_instance_summary>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_instance_summary>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_instance_summary>::write_msgpack(
    const ores::workflow::messaging::workflow_instance_summary& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_instance_summary
wire_traits<ores::workflow::messaging::workflow_instance_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_instance_summary>(data);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_request>::write_json(
    const ores::workflow::messaging::list_workflow_instance_summaries_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instance_summaries_request
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instance_summaries_request>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_request>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instance_summaries_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instance_summaries_request
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::workflow::messaging::list_workflow_instance_summaries_request>(data);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_response>::write_json(
    const ores::workflow::messaging::list_workflow_instance_summaries_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_instance_summaries_response
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_instance_summaries_response>(
        text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_response>::write_msgpack(
    const ores::workflow::messaging::list_workflow_instance_summaries_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_instance_summaries_response
wire_traits<ores::workflow::messaging::list_workflow_instance_summaries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::workflow::messaging::list_workflow_instance_summaries_response>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_step_summary>::write_json(
    const ores::workflow::messaging::workflow_step_summary& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_step_summary
wire_traits<ores::workflow::messaging::workflow_step_summary>::read_json(std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_step_summary>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_step_summary>::write_msgpack(
    const ores::workflow::messaging::workflow_step_summary& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_step_summary
wire_traits<ores::workflow::messaging::workflow_step_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_step_summary>(data);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_steps_request>::write_json(
    const ores::workflow::messaging::get_workflow_steps_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_steps_request
wire_traits<ores::workflow::messaging::get_workflow_steps_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_steps_request>(text);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_steps_request>::write_msgpack(
    const ores::workflow::messaging::get_workflow_steps_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_steps_request
wire_traits<ores::workflow::messaging::get_workflow_steps_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_steps_request>(data);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_steps_response>::write_json(
    const ores::workflow::messaging::get_workflow_steps_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::get_workflow_steps_response
wire_traits<ores::workflow::messaging::get_workflow_steps_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::get_workflow_steps_response>(text);
}

std::string wire_traits<ores::workflow::messaging::get_workflow_steps_response>::write_msgpack(
    const ores::workflow::messaging::get_workflow_steps_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::get_workflow_steps_response
wire_traits<ores::workflow::messaging::get_workflow_steps_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::get_workflow_steps_response>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_step_definition_summary>::write_json(
    const ores::workflow::messaging::workflow_step_definition_summary& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_step_definition_summary
wire_traits<ores::workflow::messaging::workflow_step_definition_summary>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_step_definition_summary>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_step_definition_summary>::write_msgpack(
    const ores::workflow::messaging::workflow_step_definition_summary& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_step_definition_summary
wire_traits<ores::workflow::messaging::workflow_step_definition_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_step_definition_summary>(data);
}

std::string wire_traits<ores::workflow::messaging::workflow_definition_summary>::write_json(
    const ores::workflow::messaging::workflow_definition_summary& v) {
    return object::write_json(v);
}

ores::workflow::messaging::workflow_definition_summary
wire_traits<ores::workflow::messaging::workflow_definition_summary>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::workflow_definition_summary>(text);
}

std::string wire_traits<ores::workflow::messaging::workflow_definition_summary>::write_msgpack(
    const ores::workflow::messaging::workflow_definition_summary& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::workflow_definition_summary
wire_traits<ores::workflow::messaging::workflow_definition_summary>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::workflow_definition_summary>(data);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_definitions_request>::write_json(
    const ores::workflow::messaging::list_workflow_definitions_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_definitions_request
wire_traits<ores::workflow::messaging::list_workflow_definitions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_definitions_request>(text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_definitions_request>::write_msgpack(
    const ores::workflow::messaging::list_workflow_definitions_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_definitions_request
wire_traits<ores::workflow::messaging::list_workflow_definitions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::list_workflow_definitions_request>(data);
}

std::string wire_traits<ores::workflow::messaging::list_workflow_definitions_response>::write_json(
    const ores::workflow::messaging::list_workflow_definitions_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::list_workflow_definitions_response
wire_traits<ores::workflow::messaging::list_workflow_definitions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::list_workflow_definitions_response>(text);
}

std::string
wire_traits<ores::workflow::messaging::list_workflow_definitions_response>::write_msgpack(
    const ores::workflow::messaging::list_workflow_definitions_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::list_workflow_definitions_response
wire_traits<ores::workflow::messaging::list_workflow_definitions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::list_workflow_definitions_response>(
        data);
}

std::string wire_traits<ores::workflow::messaging::retry_workflow_instance_request>::write_json(
    const ores::workflow::messaging::retry_workflow_instance_request& v) {
    return object::write_json(v);
}

ores::workflow::messaging::retry_workflow_instance_request
wire_traits<ores::workflow::messaging::retry_workflow_instance_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::retry_workflow_instance_request>(text);
}

std::string wire_traits<ores::workflow::messaging::retry_workflow_instance_request>::write_msgpack(
    const ores::workflow::messaging::retry_workflow_instance_request& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::retry_workflow_instance_request
wire_traits<ores::workflow::messaging::retry_workflow_instance_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::retry_workflow_instance_request>(data);
}

std::string wire_traits<ores::workflow::messaging::retry_workflow_instance_response>::write_json(
    const ores::workflow::messaging::retry_workflow_instance_response& v) {
    return object::write_json(v);
}

ores::workflow::messaging::retry_workflow_instance_response
wire_traits<ores::workflow::messaging::retry_workflow_instance_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::workflow::messaging::retry_workflow_instance_response>(text);
}

std::string wire_traits<ores::workflow::messaging::retry_workflow_instance_response>::write_msgpack(
    const ores::workflow::messaging::retry_workflow_instance_response& v) {
    return object::write_msgpack(v);
}

ores::workflow::messaging::retry_workflow_instance_response
wire_traits<ores::workflow::messaging::retry_workflow_instance_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::workflow::messaging::retry_workflow_instance_response>(data);
}

}
