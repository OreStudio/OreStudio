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
#include "ores.dq.api/domain/fsm_state_codec.hpp"
#include "ores.dq.api/messaging/fsm_state_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::fsm_state_key>::write_json(
    const ores::dq::messaging::fsm_state_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_key
wire_traits<ores::dq::messaging::fsm_state_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_key>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_key>::write_msgpack(
    const ores::dq::messaging::fsm_state_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_key
wire_traits<ores::dq::messaging::fsm_state_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_key>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_write>::write_json(
    const ores::dq::messaging::fsm_state_write& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_write
wire_traits<ores::dq::messaging::fsm_state_write>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_write>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_write>::write_msgpack(
    const ores::dq::messaging::fsm_state_write& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_write
wire_traits<ores::dq::messaging::fsm_state_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_write>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_change>::write_json(
    const ores::dq::messaging::fsm_state_change& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_change
wire_traits<ores::dq::messaging::fsm_state_change>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_change>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_change>::write_msgpack(
    const ores::dq::messaging::fsm_state_change& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_change
wire_traits<ores::dq::messaging::fsm_state_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_change>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_removal>::write_json(
    const ores::dq::messaging::fsm_state_removal& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_removal
wire_traits<ores::dq::messaging::fsm_state_removal>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_removal>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_removal>::write_msgpack(
    const ores::dq::messaging::fsm_state_removal& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_removal
wire_traits<ores::dq::messaging::fsm_state_removal>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_removal>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_lookup>::write_json(
    const ores::dq::messaging::fsm_state_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_lookup
wire_traits<ores::dq::messaging::fsm_state_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_lookup>::write_msgpack(
    const ores::dq::messaging::fsm_state_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_lookup
wire_traits<ores::dq::messaging::fsm_state_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_states_filter>::write_json(
    const ores::dq::messaging::fsm_states_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_states_filter
wire_traits<ores::dq::messaging::fsm_states_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_states_filter>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_states_filter>::write_msgpack(
    const ores::dq::messaging::fsm_states_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_states_filter
wire_traits<ores::dq::messaging::fsm_states_filter>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_states_filter>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_event>::write_json(
    const ores::dq::messaging::fsm_state_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_event
wire_traits<ores::dq::messaging::fsm_state_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_event>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_event>::write_msgpack(
    const ores::dq::messaging::fsm_state_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_event
wire_traits<ores::dq::messaging::fsm_state_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_event>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_version_key>::write_json(
    const ores::dq::messaging::fsm_state_version_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_version_key
wire_traits<ores::dq::messaging::fsm_state_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_version_key>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_version_key>::write_msgpack(
    const ores::dq::messaging::fsm_state_version_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_version_key
wire_traits<ores::dq::messaging::fsm_state_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_version_key>(data);
}

std::string wire_traits<ores::dq::messaging::fsm_state_versions_filter>::write_json(
    const ores::dq::messaging::fsm_state_versions_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::fsm_state_versions_filter
wire_traits<ores::dq::messaging::fsm_state_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::fsm_state_versions_filter>(text);
}

std::string wire_traits<ores::dq::messaging::fsm_state_versions_filter>::write_msgpack(
    const ores::dq::messaging::fsm_state_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::fsm_state_versions_filter
wire_traits<ores::dq::messaging::fsm_state_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::fsm_state_versions_filter>(data);
}

std::string wire_traits<ores::dq::messaging::list_fsm_states_request>::write_json(
    const ores::dq::messaging::list_fsm_states_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_fsm_states_request
wire_traits<ores::dq::messaging::list_fsm_states_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_fsm_states_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_fsm_states_request>::write_msgpack(
    const ores::dq::messaging::list_fsm_states_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_fsm_states_request
wire_traits<ores::dq::messaging::list_fsm_states_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_fsm_states_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_fsm_states_response>::write_json(
    const ores::dq::messaging::list_fsm_states_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_fsm_states_response
wire_traits<ores::dq::messaging::list_fsm_states_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_fsm_states_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_fsm_states_response>::write_msgpack(
    const ores::dq::messaging::list_fsm_states_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_fsm_states_response
wire_traits<ores::dq::messaging::list_fsm_states_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_fsm_states_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_request>::write_json(
    const ores::dq::messaging::get_fsm_state_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_fsm_state_request
wire_traits<ores::dq::messaging::get_fsm_state_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_fsm_state_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_request>::write_msgpack(
    const ores::dq::messaging::get_fsm_state_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_fsm_state_request
wire_traits<ores::dq::messaging::get_fsm_state_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_fsm_state_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_response>::write_json(
    const ores::dq::messaging::get_fsm_state_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_fsm_state_response
wire_traits<ores::dq::messaging::get_fsm_state_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_fsm_state_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_response>::write_msgpack(
    const ores::dq::messaging::get_fsm_state_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_fsm_state_response
wire_traits<ores::dq::messaging::get_fsm_state_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_fsm_state_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_fsm_states_request>::write_json(
    const ores::dq::messaging::get_many_fsm_states_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_fsm_states_request
wire_traits<ores::dq::messaging::get_many_fsm_states_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_fsm_states_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_fsm_states_request>::write_msgpack(
    const ores::dq::messaging::get_many_fsm_states_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_fsm_states_request
wire_traits<ores::dq::messaging::get_many_fsm_states_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_fsm_states_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_fsm_states_response>::write_json(
    const ores::dq::messaging::get_many_fsm_states_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_fsm_states_response
wire_traits<ores::dq::messaging::get_many_fsm_states_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_fsm_states_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_fsm_states_response>::write_msgpack(
    const ores::dq::messaging::get_many_fsm_states_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_fsm_states_response
wire_traits<ores::dq::messaging::get_many_fsm_states_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_fsm_states_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_fsm_state_request>::write_json(
    const ores::dq::messaging::put_fsm_state_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_fsm_state_request
wire_traits<ores::dq::messaging::put_fsm_state_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_fsm_state_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_fsm_state_request>::write_msgpack(
    const ores::dq::messaging::put_fsm_state_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_fsm_state_request
wire_traits<ores::dq::messaging::put_fsm_state_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_fsm_state_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_fsm_state_response>::write_json(
    const ores::dq::messaging::put_fsm_state_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_fsm_state_response
wire_traits<ores::dq::messaging::put_fsm_state_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_fsm_state_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_fsm_state_response>::write_msgpack(
    const ores::dq::messaging::put_fsm_state_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_fsm_state_response
wire_traits<ores::dq::messaging::put_fsm_state_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_fsm_state_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_fsm_states_request>::write_json(
    const ores::dq::messaging::put_many_fsm_states_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_fsm_states_request
wire_traits<ores::dq::messaging::put_many_fsm_states_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_fsm_states_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_fsm_states_request>::write_msgpack(
    const ores::dq::messaging::put_many_fsm_states_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_fsm_states_request
wire_traits<ores::dq::messaging::put_many_fsm_states_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_fsm_states_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_fsm_states_response>::write_json(
    const ores::dq::messaging::put_many_fsm_states_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_fsm_states_response
wire_traits<ores::dq::messaging::put_many_fsm_states_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_fsm_states_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_fsm_states_response>::write_msgpack(
    const ores::dq::messaging::put_many_fsm_states_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_fsm_states_response
wire_traits<ores::dq::messaging::put_many_fsm_states_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_fsm_states_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_fsm_state_request>::write_json(
    const ores::dq::messaging::delete_fsm_state_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_fsm_state_request
wire_traits<ores::dq::messaging::delete_fsm_state_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_fsm_state_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_fsm_state_request>::write_msgpack(
    const ores::dq::messaging::delete_fsm_state_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_fsm_state_request
wire_traits<ores::dq::messaging::delete_fsm_state_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_fsm_state_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_fsm_state_response>::write_json(
    const ores::dq::messaging::delete_fsm_state_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_fsm_state_response
wire_traits<ores::dq::messaging::delete_fsm_state_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_fsm_state_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_fsm_state_response>::write_msgpack(
    const ores::dq::messaging::delete_fsm_state_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_fsm_state_response
wire_traits<ores::dq::messaging::delete_fsm_state_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_fsm_state_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_fsm_states_request>::write_json(
    const ores::dq::messaging::delete_many_fsm_states_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_fsm_states_request
wire_traits<ores::dq::messaging::delete_many_fsm_states_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_fsm_states_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_fsm_states_request>::write_msgpack(
    const ores::dq::messaging::delete_many_fsm_states_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_fsm_states_request
wire_traits<ores::dq::messaging::delete_many_fsm_states_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_fsm_states_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_fsm_states_response>::write_json(
    const ores::dq::messaging::delete_many_fsm_states_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_fsm_states_response
wire_traits<ores::dq::messaging::delete_many_fsm_states_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_fsm_states_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_fsm_states_response>::write_msgpack(
    const ores::dq::messaging::delete_many_fsm_states_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_fsm_states_response
wire_traits<ores::dq::messaging::delete_many_fsm_states_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_fsm_states_response>(data);
}

std::string wire_traits<ores::dq::messaging::list_fsm_state_versions_request>::write_json(
    const ores::dq::messaging::list_fsm_state_versions_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_fsm_state_versions_request
wire_traits<ores::dq::messaging::list_fsm_state_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_fsm_state_versions_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_fsm_state_versions_request>::write_msgpack(
    const ores::dq::messaging::list_fsm_state_versions_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_fsm_state_versions_request
wire_traits<ores::dq::messaging::list_fsm_state_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_fsm_state_versions_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_fsm_state_versions_response>::write_json(
    const ores::dq::messaging::list_fsm_state_versions_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_fsm_state_versions_response
wire_traits<ores::dq::messaging::list_fsm_state_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_fsm_state_versions_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_fsm_state_versions_response>::write_msgpack(
    const ores::dq::messaging::list_fsm_state_versions_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_fsm_state_versions_response
wire_traits<ores::dq::messaging::list_fsm_state_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_fsm_state_versions_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_version_request>::write_json(
    const ores::dq::messaging::get_fsm_state_version_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_fsm_state_version_request
wire_traits<ores::dq::messaging::get_fsm_state_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_fsm_state_version_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_version_request>::write_msgpack(
    const ores::dq::messaging::get_fsm_state_version_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_fsm_state_version_request
wire_traits<ores::dq::messaging::get_fsm_state_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_fsm_state_version_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_version_response>::write_json(
    const ores::dq::messaging::get_fsm_state_version_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_fsm_state_version_response
wire_traits<ores::dq::messaging::get_fsm_state_version_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_fsm_state_version_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_fsm_state_version_response>::write_msgpack(
    const ores::dq::messaging::get_fsm_state_version_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_fsm_state_version_response
wire_traits<ores::dq::messaging::get_fsm_state_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_fsm_state_version_response>(data);
}

}
