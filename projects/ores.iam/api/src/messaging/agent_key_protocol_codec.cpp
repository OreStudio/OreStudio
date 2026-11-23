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
#include "ores.iam.api/domain/agent_key_codec.hpp"
#include "ores.iam.api/messaging/agent_key_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::agent_key_key>::write_json(
    const ores::iam::messaging::agent_key_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_key_key
wire_traits<ores::iam::messaging::agent_key_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_key_key>(text);
}

std::string wire_traits<ores::iam::messaging::agent_key_key>::write_msgpack(
    const ores::iam::messaging::agent_key_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_key_key
wire_traits<ores::iam::messaging::agent_key_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_key_key>(data);
}

std::string wire_traits<ores::iam::messaging::agent_key_lookup>::write_json(
    const ores::iam::messaging::agent_key_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_key_lookup
wire_traits<ores::iam::messaging::agent_key_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_key_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::agent_key_lookup>::write_msgpack(
    const ores::iam::messaging::agent_key_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_key_lookup
wire_traits<ores::iam::messaging::agent_key_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_key_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::agent_keys_filter>::write_json(
    const ores::iam::messaging::agent_keys_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_keys_filter
wire_traits<ores::iam::messaging::agent_keys_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_keys_filter>(text);
}

std::string wire_traits<ores::iam::messaging::agent_keys_filter>::write_msgpack(
    const ores::iam::messaging::agent_keys_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_keys_filter
wire_traits<ores::iam::messaging::agent_keys_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_keys_filter>(data);
}

std::string wire_traits<ores::iam::messaging::agent_key_event>::write_json(
    const ores::iam::messaging::agent_key_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_key_event
wire_traits<ores::iam::messaging::agent_key_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_key_event>(text);
}

std::string wire_traits<ores::iam::messaging::agent_key_event>::write_msgpack(
    const ores::iam::messaging::agent_key_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_key_event
wire_traits<ores::iam::messaging::agent_key_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_key_event>(data);
}

std::string wire_traits<ores::iam::messaging::agent_key_version_key>::write_json(
    const ores::iam::messaging::agent_key_version_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_key_version_key
wire_traits<ores::iam::messaging::agent_key_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_key_version_key>(text);
}

std::string wire_traits<ores::iam::messaging::agent_key_version_key>::write_msgpack(
    const ores::iam::messaging::agent_key_version_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_key_version_key
wire_traits<ores::iam::messaging::agent_key_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_key_version_key>(data);
}

std::string wire_traits<ores::iam::messaging::agent_key_versions_filter>::write_json(
    const ores::iam::messaging::agent_key_versions_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::agent_key_versions_filter
wire_traits<ores::iam::messaging::agent_key_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::agent_key_versions_filter>(text);
}

std::string wire_traits<ores::iam::messaging::agent_key_versions_filter>::write_msgpack(
    const ores::iam::messaging::agent_key_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::agent_key_versions_filter
wire_traits<ores::iam::messaging::agent_key_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::agent_key_versions_filter>(data);
}

std::string wire_traits<ores::iam::messaging::list_agent_keys_request>::write_json(
    const ores::iam::messaging::list_agent_keys_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_agent_keys_request
wire_traits<ores::iam::messaging::list_agent_keys_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_agent_keys_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_agent_keys_request>::write_msgpack(
    const ores::iam::messaging::list_agent_keys_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_agent_keys_request
wire_traits<ores::iam::messaging::list_agent_keys_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_agent_keys_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_agent_keys_response>::write_json(
    const ores::iam::messaging::list_agent_keys_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_agent_keys_response
wire_traits<ores::iam::messaging::list_agent_keys_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_agent_keys_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_agent_keys_response>::write_msgpack(
    const ores::iam::messaging::list_agent_keys_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_agent_keys_response
wire_traits<ores::iam::messaging::list_agent_keys_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_agent_keys_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_request>::write_json(
    const ores::iam::messaging::get_agent_key_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_key_request
wire_traits<ores::iam::messaging::get_agent_key_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_key_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_request>::write_msgpack(
    const ores::iam::messaging::get_agent_key_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_key_request
wire_traits<ores::iam::messaging::get_agent_key_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_key_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_response>::write_json(
    const ores::iam::messaging::get_agent_key_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_key_response
wire_traits<ores::iam::messaging::get_agent_key_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_key_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_response>::write_msgpack(
    const ores::iam::messaging::get_agent_key_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_key_response
wire_traits<ores::iam::messaging::get_agent_key_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_key_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_agent_keys_request>::write_json(
    const ores::iam::messaging::get_many_agent_keys_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_agent_keys_request
wire_traits<ores::iam::messaging::get_many_agent_keys_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_agent_keys_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_agent_keys_request>::write_msgpack(
    const ores::iam::messaging::get_many_agent_keys_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_agent_keys_request
wire_traits<ores::iam::messaging::get_many_agent_keys_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_agent_keys_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_agent_keys_response>::write_json(
    const ores::iam::messaging::get_many_agent_keys_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_agent_keys_response
wire_traits<ores::iam::messaging::get_many_agent_keys_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_agent_keys_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_agent_keys_response>::write_msgpack(
    const ores::iam::messaging::get_many_agent_keys_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_agent_keys_response
wire_traits<ores::iam::messaging::get_many_agent_keys_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_agent_keys_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_request>::write_json(
    const ores::iam::messaging::list_by_grant_id_agent_keys_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_grant_id_agent_keys_request
wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_grant_id_agent_keys_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_request>::write_msgpack(
    const ores::iam::messaging::list_by_grant_id_agent_keys_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_grant_id_agent_keys_request
wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_grant_id_agent_keys_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_response>::write_json(
    const ores::iam::messaging::list_by_grant_id_agent_keys_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_grant_id_agent_keys_response
wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_grant_id_agent_keys_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_response>::write_msgpack(
    const ores::iam::messaging::list_by_grant_id_agent_keys_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_grant_id_agent_keys_response
wire_traits<ores::iam::messaging::list_by_grant_id_agent_keys_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_grant_id_agent_keys_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_agent_key_versions_request>::write_json(
    const ores::iam::messaging::list_agent_key_versions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_agent_key_versions_request
wire_traits<ores::iam::messaging::list_agent_key_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_agent_key_versions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_agent_key_versions_request>::write_msgpack(
    const ores::iam::messaging::list_agent_key_versions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_agent_key_versions_request
wire_traits<ores::iam::messaging::list_agent_key_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_agent_key_versions_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_agent_key_versions_response>::write_json(
    const ores::iam::messaging::list_agent_key_versions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_agent_key_versions_response
wire_traits<ores::iam::messaging::list_agent_key_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_agent_key_versions_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_agent_key_versions_response>::write_msgpack(
    const ores::iam::messaging::list_agent_key_versions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_agent_key_versions_response
wire_traits<ores::iam::messaging::list_agent_key_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_agent_key_versions_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_version_request>::write_json(
    const ores::iam::messaging::get_agent_key_version_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_key_version_request
wire_traits<ores::iam::messaging::get_agent_key_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_key_version_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_version_request>::write_msgpack(
    const ores::iam::messaging::get_agent_key_version_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_key_version_request
wire_traits<ores::iam::messaging::get_agent_key_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_key_version_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_version_response>::write_json(
    const ores::iam::messaging::get_agent_key_version_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_agent_key_version_response
wire_traits<ores::iam::messaging::get_agent_key_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_agent_key_version_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_agent_key_version_response>::write_msgpack(
    const ores::iam::messaging::get_agent_key_version_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_agent_key_version_response
wire_traits<ores::iam::messaging::get_agent_key_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_agent_key_version_response>(data);
}

}
