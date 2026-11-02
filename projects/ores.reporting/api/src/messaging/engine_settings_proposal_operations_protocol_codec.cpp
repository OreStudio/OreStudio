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
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::engine_settings_value_change>::write_json(
    const ores::reporting::messaging::engine_settings_value_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::engine_settings_value_change
wire_traits<ores::reporting::messaging::engine_settings_value_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::engine_settings_value_change>(text);
}

std::string wire_traits<ores::reporting::messaging::engine_settings_value_change>::write_msgpack(
    const ores::reporting::messaging::engine_settings_value_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::engine_settings_value_change
wire_traits<ores::reporting::messaging::engine_settings_value_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::engine_settings_value_change>(data);
}

std::string wire_traits<ores::reporting::messaging::engine_settings_change_line>::write_json(
    const ores::reporting::messaging::engine_settings_change_line& v) {
    return object::write_json(v);
}

ores::reporting::messaging::engine_settings_change_line
wire_traits<ores::reporting::messaging::engine_settings_change_line>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::engine_settings_change_line>(text);
}

std::string wire_traits<ores::reporting::messaging::engine_settings_change_line>::write_msgpack(
    const ores::reporting::messaging::engine_settings_change_line& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::engine_settings_change_line
wire_traits<ores::reporting::messaging::engine_settings_change_line>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::engine_settings_change_line>(data);
}

std::string wire_traits<ores::reporting::messaging::engine_settings_value>::write_json(
    const ores::reporting::messaging::engine_settings_value& v) {
    return object::write_json(v);
}

ores::reporting::messaging::engine_settings_value
wire_traits<ores::reporting::messaging::engine_settings_value>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::engine_settings_value>(text);
}

std::string wire_traits<ores::reporting::messaging::engine_settings_value>::write_msgpack(
    const ores::reporting::messaging::engine_settings_value& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::engine_settings_value
wire_traits<ores::reporting::messaging::engine_settings_value>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::engine_settings_value>(data);
}

std::string
wire_traits<ores::reporting::messaging::raise_engine_settings_change_request>::write_json(
    const ores::reporting::messaging::raise_engine_settings_change_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::raise_engine_settings_change_request
wire_traits<ores::reporting::messaging::raise_engine_settings_change_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::raise_engine_settings_change_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::raise_engine_settings_change_request>::write_msgpack(
    const ores::reporting::messaging::raise_engine_settings_change_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::raise_engine_settings_change_request
wire_traits<ores::reporting::messaging::raise_engine_settings_change_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::raise_engine_settings_change_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::raise_engine_settings_change_response>::write_json(
    const ores::reporting::messaging::raise_engine_settings_change_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::raise_engine_settings_change_response
wire_traits<ores::reporting::messaging::raise_engine_settings_change_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::raise_engine_settings_change_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::raise_engine_settings_change_response>::write_msgpack(
    const ores::reporting::messaging::raise_engine_settings_change_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::raise_engine_settings_change_response
wire_traits<ores::reporting::messaging::raise_engine_settings_change_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::raise_engine_settings_change_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_engine_settings_changes_request>::write_json(
    const ores::reporting::messaging::get_engine_settings_changes_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_engine_settings_changes_request
wire_traits<ores::reporting::messaging::get_engine_settings_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_engine_settings_changes_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_engine_settings_changes_request>::write_msgpack(
    const ores::reporting::messaging::get_engine_settings_changes_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_engine_settings_changes_request
wire_traits<ores::reporting::messaging::get_engine_settings_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_engine_settings_changes_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_engine_settings_changes_response>::write_json(
    const ores::reporting::messaging::get_engine_settings_changes_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_engine_settings_changes_response
wire_traits<ores::reporting::messaging::get_engine_settings_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_engine_settings_changes_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_engine_settings_changes_response>::write_msgpack(
    const ores::reporting::messaging::get_engine_settings_changes_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_engine_settings_changes_response
wire_traits<ores::reporting::messaging::get_engine_settings_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_engine_settings_changes_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_request>::write_json(
    const ores::reporting::messaging::recheck_engine_settings_changes_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::recheck_engine_settings_changes_request
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::recheck_engine_settings_changes_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_request>::write_msgpack(
    const ores::reporting::messaging::recheck_engine_settings_changes_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::recheck_engine_settings_changes_request
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::recheck_engine_settings_changes_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_response>::write_json(
    const ores::reporting::messaging::recheck_engine_settings_changes_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::recheck_engine_settings_changes_response
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::recheck_engine_settings_changes_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_response>::write_msgpack(
    const ores::reporting::messaging::recheck_engine_settings_changes_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::recheck_engine_settings_changes_response
wire_traits<ores::reporting::messaging::recheck_engine_settings_changes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::recheck_engine_settings_changes_response>(data);
}

}
