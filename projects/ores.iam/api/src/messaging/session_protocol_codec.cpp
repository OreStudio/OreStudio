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
#include "ores.iam.api/domain/session_codec.hpp"
#include "ores.iam.api/messaging/session_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::session_key>::write_json(
    const ores::iam::messaging::session_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_key
wire_traits<ores::iam::messaging::session_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_key>(text);
}

std::string wire_traits<ores::iam::messaging::session_key>::write_msgpack(
    const ores::iam::messaging::session_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_key
wire_traits<ores::iam::messaging::session_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_key>(data);
}

std::string wire_traits<ores::iam::messaging::session_write>::write_json(
    const ores::iam::messaging::session_write& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_write
wire_traits<ores::iam::messaging::session_write>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_write>(text);
}

std::string wire_traits<ores::iam::messaging::session_write>::write_msgpack(
    const ores::iam::messaging::session_write& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_write
wire_traits<ores::iam::messaging::session_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_write>(data);
}

std::string wire_traits<ores::iam::messaging::session_change>::write_json(
    const ores::iam::messaging::session_change& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_change
wire_traits<ores::iam::messaging::session_change>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_change>(text);
}

std::string wire_traits<ores::iam::messaging::session_change>::write_msgpack(
    const ores::iam::messaging::session_change& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_change
wire_traits<ores::iam::messaging::session_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_change>(data);
}

std::string wire_traits<ores::iam::messaging::session_removal>::write_json(
    const ores::iam::messaging::session_removal& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_removal
wire_traits<ores::iam::messaging::session_removal>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_removal>(text);
}

std::string wire_traits<ores::iam::messaging::session_removal>::write_msgpack(
    const ores::iam::messaging::session_removal& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_removal
wire_traits<ores::iam::messaging::session_removal>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_removal>(data);
}

std::string wire_traits<ores::iam::messaging::session_lookup>::write_json(
    const ores::iam::messaging::session_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_lookup
wire_traits<ores::iam::messaging::session_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::session_lookup>::write_msgpack(
    const ores::iam::messaging::session_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_lookup
wire_traits<ores::iam::messaging::session_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::sessions_filter>::write_json(
    const ores::iam::messaging::sessions_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::sessions_filter
wire_traits<ores::iam::messaging::sessions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::sessions_filter>(text);
}

std::string wire_traits<ores::iam::messaging::sessions_filter>::write_msgpack(
    const ores::iam::messaging::sessions_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::sessions_filter
wire_traits<ores::iam::messaging::sessions_filter>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::sessions_filter>(data);
}

std::string wire_traits<ores::iam::messaging::session_event>::write_json(
    const ores::iam::messaging::session_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::session_event
wire_traits<ores::iam::messaging::session_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::session_event>(text);
}

std::string wire_traits<ores::iam::messaging::session_event>::write_msgpack(
    const ores::iam::messaging::session_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::session_event
wire_traits<ores::iam::messaging::session_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::session_event>(data);
}

std::string wire_traits<ores::iam::messaging::list_sessions_request>::write_json(
    const ores::iam::messaging::list_sessions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_sessions_request
wire_traits<ores::iam::messaging::list_sessions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_sessions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_sessions_request>::write_msgpack(
    const ores::iam::messaging::list_sessions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_sessions_request
wire_traits<ores::iam::messaging::list_sessions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_sessions_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_sessions_response>::write_json(
    const ores::iam::messaging::list_sessions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_sessions_response
wire_traits<ores::iam::messaging::list_sessions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_sessions_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_sessions_response>::write_msgpack(
    const ores::iam::messaging::list_sessions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_sessions_response
wire_traits<ores::iam::messaging::list_sessions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_sessions_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_session_request>::write_json(
    const ores::iam::messaging::get_session_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_session_request
wire_traits<ores::iam::messaging::get_session_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_session_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_session_request>::write_msgpack(
    const ores::iam::messaging::get_session_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_session_request
wire_traits<ores::iam::messaging::get_session_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_session_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_session_response>::write_json(
    const ores::iam::messaging::get_session_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_session_response
wire_traits<ores::iam::messaging::get_session_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_session_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_session_response>::write_msgpack(
    const ores::iam::messaging::get_session_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_session_response
wire_traits<ores::iam::messaging::get_session_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_session_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_sessions_request>::write_json(
    const ores::iam::messaging::get_many_sessions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_sessions_request
wire_traits<ores::iam::messaging::get_many_sessions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_sessions_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_sessions_request>::write_msgpack(
    const ores::iam::messaging::get_many_sessions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_sessions_request
wire_traits<ores::iam::messaging::get_many_sessions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_sessions_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_sessions_response>::write_json(
    const ores::iam::messaging::get_many_sessions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_sessions_response
wire_traits<ores::iam::messaging::get_many_sessions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_sessions_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_sessions_response>::write_msgpack(
    const ores::iam::messaging::get_many_sessions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_sessions_response
wire_traits<ores::iam::messaging::get_many_sessions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_sessions_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_session_request>::write_json(
    const ores::iam::messaging::put_session_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_session_request
wire_traits<ores::iam::messaging::put_session_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_session_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_session_request>::write_msgpack(
    const ores::iam::messaging::put_session_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_session_request
wire_traits<ores::iam::messaging::put_session_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_session_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_session_response>::write_json(
    const ores::iam::messaging::put_session_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_session_response
wire_traits<ores::iam::messaging::put_session_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_session_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_session_response>::write_msgpack(
    const ores::iam::messaging::put_session_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_session_response
wire_traits<ores::iam::messaging::put_session_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_session_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_sessions_request>::write_json(
    const ores::iam::messaging::put_many_sessions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_sessions_request
wire_traits<ores::iam::messaging::put_many_sessions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_sessions_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_sessions_request>::write_msgpack(
    const ores::iam::messaging::put_many_sessions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_sessions_request
wire_traits<ores::iam::messaging::put_many_sessions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_sessions_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_sessions_response>::write_json(
    const ores::iam::messaging::put_many_sessions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_sessions_response
wire_traits<ores::iam::messaging::put_many_sessions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_sessions_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_sessions_response>::write_msgpack(
    const ores::iam::messaging::put_many_sessions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_sessions_response
wire_traits<ores::iam::messaging::put_many_sessions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_sessions_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_session_request>::write_json(
    const ores::iam::messaging::delete_session_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_session_request
wire_traits<ores::iam::messaging::delete_session_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_session_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_session_request>::write_msgpack(
    const ores::iam::messaging::delete_session_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_session_request
wire_traits<ores::iam::messaging::delete_session_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_session_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_session_response>::write_json(
    const ores::iam::messaging::delete_session_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_session_response
wire_traits<ores::iam::messaging::delete_session_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_session_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_session_response>::write_msgpack(
    const ores::iam::messaging::delete_session_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_session_response
wire_traits<ores::iam::messaging::delete_session_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_session_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_sessions_request>::write_json(
    const ores::iam::messaging::delete_many_sessions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_sessions_request
wire_traits<ores::iam::messaging::delete_many_sessions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_sessions_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_sessions_request>::write_msgpack(
    const ores::iam::messaging::delete_many_sessions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_sessions_request
wire_traits<ores::iam::messaging::delete_many_sessions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_sessions_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_sessions_response>::write_json(
    const ores::iam::messaging::delete_many_sessions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_sessions_response
wire_traits<ores::iam::messaging::delete_many_sessions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_sessions_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_sessions_response>::write_msgpack(
    const ores::iam::messaging::delete_many_sessions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_sessions_response
wire_traits<ores::iam::messaging::delete_many_sessions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_sessions_response>(data);
}

}
