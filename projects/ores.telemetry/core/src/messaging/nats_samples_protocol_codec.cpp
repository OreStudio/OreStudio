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
#include "ores.telemetry.core/messaging/nats_samples_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::telemetry::messaging::nats_server_sample>::write_json(
    const ores::telemetry::messaging::nats_server_sample& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::nats_server_sample
wire_traits<ores::telemetry::messaging::nats_server_sample>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::nats_server_sample>(text);
}

std::string wire_traits<ores::telemetry::messaging::nats_server_sample>::write_msgpack(
    const ores::telemetry::messaging::nats_server_sample& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::nats_server_sample
wire_traits<ores::telemetry::messaging::nats_server_sample>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::nats_server_sample>(data);
}

std::string wire_traits<ores::telemetry::messaging::nats_stream_sample>::write_json(
    const ores::telemetry::messaging::nats_stream_sample& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::nats_stream_sample
wire_traits<ores::telemetry::messaging::nats_stream_sample>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::nats_stream_sample>(text);
}

std::string wire_traits<ores::telemetry::messaging::nats_stream_sample>::write_msgpack(
    const ores::telemetry::messaging::nats_stream_sample& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::nats_stream_sample
wire_traits<ores::telemetry::messaging::nats_stream_sample>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::nats_stream_sample>(data);
}

std::string wire_traits<ores::telemetry::messaging::nats_server_samples_query>::write_json(
    const ores::telemetry::messaging::nats_server_samples_query& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::nats_server_samples_query
wire_traits<ores::telemetry::messaging::nats_server_samples_query>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::nats_server_samples_query>(text);
}

std::string wire_traits<ores::telemetry::messaging::nats_server_samples_query>::write_msgpack(
    const ores::telemetry::messaging::nats_server_samples_query& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::nats_server_samples_query
wire_traits<ores::telemetry::messaging::nats_server_samples_query>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::nats_server_samples_query>(data);
}

std::string wire_traits<ores::telemetry::messaging::nats_stream_samples_query>::write_json(
    const ores::telemetry::messaging::nats_stream_samples_query& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::nats_stream_samples_query
wire_traits<ores::telemetry::messaging::nats_stream_samples_query>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::nats_stream_samples_query>(text);
}

std::string wire_traits<ores::telemetry::messaging::nats_stream_samples_query>::write_msgpack(
    const ores::telemetry::messaging::nats_stream_samples_query& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::nats_stream_samples_query
wire_traits<ores::telemetry::messaging::nats_stream_samples_query>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::nats_stream_samples_query>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_server_samples_request>::write_json(
    const ores::telemetry::messaging::get_nats_server_samples_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_nats_server_samples_request
wire_traits<ores::telemetry::messaging::get_nats_server_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_nats_server_samples_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_server_samples_request>::write_msgpack(
    const ores::telemetry::messaging::get_nats_server_samples_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_nats_server_samples_request
wire_traits<ores::telemetry::messaging::get_nats_server_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_nats_server_samples_request>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_server_samples_response>::write_json(
    const ores::telemetry::messaging::get_nats_server_samples_response& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_nats_server_samples_response
wire_traits<ores::telemetry::messaging::get_nats_server_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_nats_server_samples_response>(text);
}

std::string
wire_traits<ores::telemetry::messaging::get_nats_server_samples_response>::write_msgpack(
    const ores::telemetry::messaging::get_nats_server_samples_response& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_nats_server_samples_response
wire_traits<ores::telemetry::messaging::get_nats_server_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_nats_server_samples_response>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_stream_samples_request>::write_json(
    const ores::telemetry::messaging::get_nats_stream_samples_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_nats_stream_samples_request
wire_traits<ores::telemetry::messaging::get_nats_stream_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_nats_stream_samples_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_stream_samples_request>::write_msgpack(
    const ores::telemetry::messaging::get_nats_stream_samples_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_nats_stream_samples_request
wire_traits<ores::telemetry::messaging::get_nats_stream_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_nats_stream_samples_request>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_nats_stream_samples_response>::write_json(
    const ores::telemetry::messaging::get_nats_stream_samples_response& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_nats_stream_samples_response
wire_traits<ores::telemetry::messaging::get_nats_stream_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_nats_stream_samples_response>(text);
}

std::string
wire_traits<ores::telemetry::messaging::get_nats_stream_samples_response>::write_msgpack(
    const ores::telemetry::messaging::get_nats_stream_samples_response& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_nats_stream_samples_response
wire_traits<ores::telemetry::messaging::get_nats_stream_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_nats_stream_samples_response>(data);
}

}
