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
#include "ores.telemetry.core/domain/service_state.hpp"
#include "ores.telemetry.core/messaging/service_samples_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::telemetry::messaging::service_sample>::write_json(
    const ores::telemetry::messaging::service_sample& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::service_sample
wire_traits<ores::telemetry::messaging::service_sample>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::service_sample>(text);
}

std::string wire_traits<ores::telemetry::messaging::service_sample>::write_msgpack(
    const ores::telemetry::messaging::service_sample& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::service_sample
wire_traits<ores::telemetry::messaging::service_sample>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::service_sample>(data);
}

std::string wire_traits<ores::telemetry::messaging::service_heartbeat_message>::write_json(
    const ores::telemetry::messaging::service_heartbeat_message& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::service_heartbeat_message
wire_traits<ores::telemetry::messaging::service_heartbeat_message>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::service_heartbeat_message>(text);
}

std::string wire_traits<ores::telemetry::messaging::service_heartbeat_message>::write_msgpack(
    const ores::telemetry::messaging::service_heartbeat_message& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::service_heartbeat_message
wire_traits<ores::telemetry::messaging::service_heartbeat_message>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::service_heartbeat_message>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_service_samples_request>::write_json(
    const ores::telemetry::messaging::get_service_samples_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_service_samples_request
wire_traits<ores::telemetry::messaging::get_service_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_service_samples_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_service_samples_request>::write_msgpack(
    const ores::telemetry::messaging::get_service_samples_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_service_samples_request
wire_traits<ores::telemetry::messaging::get_service_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_service_samples_request>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_service_samples_response>::write_json(
    const ores::telemetry::messaging::get_service_samples_response& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_service_samples_response
wire_traits<ores::telemetry::messaging::get_service_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_service_samples_response>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_service_samples_response>::write_msgpack(
    const ores::telemetry::messaging::get_service_samples_response& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_service_samples_response
wire_traits<ores::telemetry::messaging::get_service_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_service_samples_response>(data);
}

std::string wire_traits<ores::telemetry::messaging::service_roster_slot>::write_json(
    const ores::telemetry::messaging::service_roster_slot& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::service_roster_slot
wire_traits<ores::telemetry::messaging::service_roster_slot>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::service_roster_slot>(text);
}

std::string wire_traits<ores::telemetry::messaging::service_roster_slot>::write_msgpack(
    const ores::telemetry::messaging::service_roster_slot& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::service_roster_slot
wire_traits<ores::telemetry::messaging::service_roster_slot>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::service_roster_slot>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_service_roster_request>::write_json(
    const ores::telemetry::messaging::get_service_roster_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_service_roster_request
wire_traits<ores::telemetry::messaging::get_service_roster_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_service_roster_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_service_roster_request>::write_msgpack(
    const ores::telemetry::messaging::get_service_roster_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_service_roster_request
wire_traits<ores::telemetry::messaging::get_service_roster_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_service_roster_request>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_service_roster_response>::write_json(
    const ores::telemetry::messaging::get_service_roster_response& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_service_roster_response
wire_traits<ores::telemetry::messaging::get_service_roster_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_service_roster_response>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_service_roster_response>::write_msgpack(
    const ores::telemetry::messaging::get_service_roster_response& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_service_roster_response
wire_traits<ores::telemetry::messaging::get_service_roster_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_service_roster_response>(data);
}

}
