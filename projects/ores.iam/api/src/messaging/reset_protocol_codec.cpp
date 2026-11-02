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
#include "ores.iam.api/messaging/reset_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::reset_tenant_command>::write_json(
    const ores::iam::messaging::reset_tenant_command& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_tenant_command
wire_traits<ores::iam::messaging::reset_tenant_command>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_tenant_command>(text);
}

std::string wire_traits<ores::iam::messaging::reset_tenant_command>::write_msgpack(
    const ores::iam::messaging::reset_tenant_command& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_tenant_command
wire_traits<ores::iam::messaging::reset_tenant_command>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_tenant_command>(data);
}

std::string wire_traits<ores::iam::messaging::reset_tenant_result>::write_json(
    const ores::iam::messaging::reset_tenant_result& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_tenant_result
wire_traits<ores::iam::messaging::reset_tenant_result>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_tenant_result>(text);
}

std::string wire_traits<ores::iam::messaging::reset_tenant_result>::write_msgpack(
    const ores::iam::messaging::reset_tenant_result& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_tenant_result
wire_traits<ores::iam::messaging::reset_tenant_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_tenant_result>(data);
}

std::string wire_traits<ores::iam::messaging::reset_system_command>::write_json(
    const ores::iam::messaging::reset_system_command& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_system_command
wire_traits<ores::iam::messaging::reset_system_command>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_system_command>(text);
}

std::string wire_traits<ores::iam::messaging::reset_system_command>::write_msgpack(
    const ores::iam::messaging::reset_system_command& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_system_command
wire_traits<ores::iam::messaging::reset_system_command>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_system_command>(data);
}

std::string wire_traits<ores::iam::messaging::reset_system_result>::write_json(
    const ores::iam::messaging::reset_system_result& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_system_result
wire_traits<ores::iam::messaging::reset_system_result>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_system_result>(text);
}

std::string wire_traits<ores::iam::messaging::reset_system_result>::write_msgpack(
    const ores::iam::messaging::reset_system_result& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_system_result
wire_traits<ores::iam::messaging::reset_system_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_system_result>(data);
}

}
