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
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::compute::messaging::work_assignment_event>::write_json(
    const ores::compute::messaging::work_assignment_event& v) {
    return object::write_json(v);
}

ores::compute::messaging::work_assignment_event
wire_traits<ores::compute::messaging::work_assignment_event>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::work_assignment_event>(text);
}

std::string wire_traits<ores::compute::messaging::work_assignment_event>::write_msgpack(
    const ores::compute::messaging::work_assignment_event& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::work_assignment_event
wire_traits<ores::compute::messaging::work_assignment_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::work_assignment_event>(data);
}

std::string wire_traits<ores::compute::messaging::heartbeat_message>::write_json(
    const ores::compute::messaging::heartbeat_message& v) {
    return object::write_json(v);
}

ores::compute::messaging::heartbeat_message
wire_traits<ores::compute::messaging::heartbeat_message>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::heartbeat_message>(text);
}

std::string wire_traits<ores::compute::messaging::heartbeat_message>::write_msgpack(
    const ores::compute::messaging::heartbeat_message& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::heartbeat_message
wire_traits<ores::compute::messaging::heartbeat_message>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::heartbeat_message>(data);
}

std::string wire_traits<ores::compute::messaging::reap_work_message>::write_json(
    const ores::compute::messaging::reap_work_message& v) {
    return object::write_json(v);
}

ores::compute::messaging::reap_work_message
wire_traits<ores::compute::messaging::reap_work_message>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::reap_work_message>(text);
}

std::string wire_traits<ores::compute::messaging::reap_work_message>::write_msgpack(
    const ores::compute::messaging::reap_work_message& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::reap_work_message
wire_traits<ores::compute::messaging::reap_work_message>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::reap_work_message>(data);
}

std::string wire_traits<ores::compute::messaging::claim_work_request>::write_json(
    const ores::compute::messaging::claim_work_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::claim_work_request
wire_traits<ores::compute::messaging::claim_work_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::claim_work_request>(text);
}

std::string wire_traits<ores::compute::messaging::claim_work_request>::write_msgpack(
    const ores::compute::messaging::claim_work_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::claim_work_request
wire_traits<ores::compute::messaging::claim_work_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::claim_work_request>(data);
}

std::string wire_traits<ores::compute::messaging::claim_work_response>::write_json(
    const ores::compute::messaging::claim_work_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::claim_work_response
wire_traits<ores::compute::messaging::claim_work_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::claim_work_response>(text);
}

std::string wire_traits<ores::compute::messaging::claim_work_response>::write_msgpack(
    const ores::compute::messaging::claim_work_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::claim_work_response
wire_traits<ores::compute::messaging::claim_work_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::claim_work_response>(data);
}

std::string wire_traits<ores::compute::messaging::work_available_event>::write_json(
    const ores::compute::messaging::work_available_event& v) {
    return object::write_json(v);
}

ores::compute::messaging::work_available_event
wire_traits<ores::compute::messaging::work_available_event>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::work_available_event>(text);
}

std::string wire_traits<ores::compute::messaging::work_available_event>::write_msgpack(
    const ores::compute::messaging::work_available_event& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::work_available_event
wire_traits<ores::compute::messaging::work_available_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::work_available_event>(data);
}

std::string wire_traits<ores::compute::messaging::submit_result_request>::write_json(
    const ores::compute::messaging::submit_result_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::submit_result_request
wire_traits<ores::compute::messaging::submit_result_request>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::submit_result_request>(text);
}

std::string wire_traits<ores::compute::messaging::submit_result_request>::write_msgpack(
    const ores::compute::messaging::submit_result_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::submit_result_request
wire_traits<ores::compute::messaging::submit_result_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::submit_result_request>(data);
}

std::string wire_traits<ores::compute::messaging::submit_result_response>::write_json(
    const ores::compute::messaging::submit_result_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::submit_result_response
wire_traits<ores::compute::messaging::submit_result_response>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::submit_result_response>(text);
}

std::string wire_traits<ores::compute::messaging::submit_result_response>::write_msgpack(
    const ores::compute::messaging::submit_result_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::submit_result_response
wire_traits<ores::compute::messaging::submit_result_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::submit_result_response>(data);
}

}
