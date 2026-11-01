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
#include "ores.telemetry.core/domain/telemetry_source.hpp"
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::telemetry::messaging::telemetry_log_entry>::write_json(
    const ores::telemetry::messaging::telemetry_log_entry& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::telemetry_log_entry
wire_traits<ores::telemetry::messaging::telemetry_log_entry>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::telemetry_log_entry>(text);
}

std::string wire_traits<ores::telemetry::messaging::telemetry_log_entry>::write_msgpack(
    const ores::telemetry::messaging::telemetry_log_entry& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::telemetry_log_entry
wire_traits<ores::telemetry::messaging::telemetry_log_entry>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::telemetry_log_entry>(data);
}

std::string wire_traits<ores::telemetry::messaging::telemetry_query>::write_json(
    const ores::telemetry::messaging::telemetry_query& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::telemetry_query
wire_traits<ores::telemetry::messaging::telemetry_query>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::telemetry_query>(text);
}

std::string wire_traits<ores::telemetry::messaging::telemetry_query>::write_msgpack(
    const ores::telemetry::messaging::telemetry_query& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::telemetry_query
wire_traits<ores::telemetry::messaging::telemetry_query>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::telemetry_query>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_telemetry_logs_request>::write_json(
    const ores::telemetry::messaging::get_telemetry_logs_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_telemetry_logs_request
wire_traits<ores::telemetry::messaging::get_telemetry_logs_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_telemetry_logs_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_telemetry_logs_request>::write_msgpack(
    const ores::telemetry::messaging::get_telemetry_logs_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_telemetry_logs_request
wire_traits<ores::telemetry::messaging::get_telemetry_logs_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_telemetry_logs_request>(data);
}

std::string wire_traits<ores::telemetry::messaging::get_telemetry_logs_response>::write_json(
    const ores::telemetry::messaging::get_telemetry_logs_response& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::get_telemetry_logs_response
wire_traits<ores::telemetry::messaging::get_telemetry_logs_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::get_telemetry_logs_response>(text);
}

std::string wire_traits<ores::telemetry::messaging::get_telemetry_logs_response>::write_msgpack(
    const ores::telemetry::messaging::get_telemetry_logs_response& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::get_telemetry_logs_response
wire_traits<ores::telemetry::messaging::get_telemetry_logs_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::get_telemetry_logs_response>(data);
}

std::string wire_traits<ores::telemetry::messaging::publish_log_entry_item>::write_json(
    const ores::telemetry::messaging::publish_log_entry_item& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::publish_log_entry_item
wire_traits<ores::telemetry::messaging::publish_log_entry_item>::read_json(std::string_view text) {
    return object::read_json<ores::telemetry::messaging::publish_log_entry_item>(text);
}

std::string wire_traits<ores::telemetry::messaging::publish_log_entry_item>::write_msgpack(
    const ores::telemetry::messaging::publish_log_entry_item& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::publish_log_entry_item
wire_traits<ores::telemetry::messaging::publish_log_entry_item>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::publish_log_entry_item>(data);
}

std::string wire_traits<ores::telemetry::messaging::publish_log_entries_request>::write_json(
    const ores::telemetry::messaging::publish_log_entries_request& v) {
    return object::write_json(v);
}

ores::telemetry::messaging::publish_log_entries_request
wire_traits<ores::telemetry::messaging::publish_log_entries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::telemetry::messaging::publish_log_entries_request>(text);
}

std::string wire_traits<ores::telemetry::messaging::publish_log_entries_request>::write_msgpack(
    const ores::telemetry::messaging::publish_log_entries_request& v) {
    return object::write_msgpack(v);
}

ores::telemetry::messaging::publish_log_entries_request
wire_traits<ores::telemetry::messaging::publish_log_entries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::telemetry::messaging::publish_log_entries_request>(data);
}

}
