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
#include "ores.vault.api/domain/secret_use_codec.hpp"
#include "ores.vault.api/messaging/secret_use_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::vault::messaging::secret_use_key>::write_json(
    const ores::vault::messaging::secret_use_key& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_key
wire_traits<ores::vault::messaging::secret_use_key>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_key>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_key>::write_msgpack(
    const ores::vault::messaging::secret_use_key& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_key
wire_traits<ores::vault::messaging::secret_use_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_key>(data);
}

std::string wire_traits<ores::vault::messaging::secret_use_write>::write_json(
    const ores::vault::messaging::secret_use_write& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_write
wire_traits<ores::vault::messaging::secret_use_write>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_write>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_write>::write_msgpack(
    const ores::vault::messaging::secret_use_write& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_write
wire_traits<ores::vault::messaging::secret_use_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_write>(data);
}

std::string wire_traits<ores::vault::messaging::secret_use_change>::write_json(
    const ores::vault::messaging::secret_use_change& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_change
wire_traits<ores::vault::messaging::secret_use_change>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_change>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_change>::write_msgpack(
    const ores::vault::messaging::secret_use_change& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_change
wire_traits<ores::vault::messaging::secret_use_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_change>(data);
}

std::string wire_traits<ores::vault::messaging::secret_use_removal>::write_json(
    const ores::vault::messaging::secret_use_removal& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_removal
wire_traits<ores::vault::messaging::secret_use_removal>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_removal>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_removal>::write_msgpack(
    const ores::vault::messaging::secret_use_removal& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_removal
wire_traits<ores::vault::messaging::secret_use_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_removal>(data);
}

std::string wire_traits<ores::vault::messaging::secret_use_lookup>::write_json(
    const ores::vault::messaging::secret_use_lookup& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_lookup
wire_traits<ores::vault::messaging::secret_use_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_lookup>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_lookup>::write_msgpack(
    const ores::vault::messaging::secret_use_lookup& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_lookup
wire_traits<ores::vault::messaging::secret_use_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_lookup>(data);
}

std::string wire_traits<ores::vault::messaging::secret_uses_filter>::write_json(
    const ores::vault::messaging::secret_uses_filter& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_uses_filter
wire_traits<ores::vault::messaging::secret_uses_filter>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_uses_filter>(text);
}

std::string wire_traits<ores::vault::messaging::secret_uses_filter>::write_msgpack(
    const ores::vault::messaging::secret_uses_filter& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_uses_filter
wire_traits<ores::vault::messaging::secret_uses_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_uses_filter>(data);
}

std::string wire_traits<ores::vault::messaging::secret_use_event>::write_json(
    const ores::vault::messaging::secret_use_event& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_use_event
wire_traits<ores::vault::messaging::secret_use_event>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_use_event>(text);
}

std::string wire_traits<ores::vault::messaging::secret_use_event>::write_msgpack(
    const ores::vault::messaging::secret_use_event& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_use_event
wire_traits<ores::vault::messaging::secret_use_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_use_event>(data);
}

std::string wire_traits<ores::vault::messaging::list_secret_uses_request>::write_json(
    const ores::vault::messaging::list_secret_uses_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secret_uses_request
wire_traits<ores::vault::messaging::list_secret_uses_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secret_uses_request>(text);
}

std::string wire_traits<ores::vault::messaging::list_secret_uses_request>::write_msgpack(
    const ores::vault::messaging::list_secret_uses_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secret_uses_request
wire_traits<ores::vault::messaging::list_secret_uses_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secret_uses_request>(data);
}

std::string wire_traits<ores::vault::messaging::list_secret_uses_response>::write_json(
    const ores::vault::messaging::list_secret_uses_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secret_uses_response
wire_traits<ores::vault::messaging::list_secret_uses_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secret_uses_response>(text);
}

std::string wire_traits<ores::vault::messaging::list_secret_uses_response>::write_msgpack(
    const ores::vault::messaging::list_secret_uses_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secret_uses_response
wire_traits<ores::vault::messaging::list_secret_uses_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secret_uses_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_use_request>::write_json(
    const ores::vault::messaging::get_secret_use_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_use_request
wire_traits<ores::vault::messaging::get_secret_use_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_use_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_use_request>::write_msgpack(
    const ores::vault::messaging::get_secret_use_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_use_request
wire_traits<ores::vault::messaging::get_secret_use_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_use_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_use_response>::write_json(
    const ores::vault::messaging::get_secret_use_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_use_response
wire_traits<ores::vault::messaging::get_secret_use_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_use_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_use_response>::write_msgpack(
    const ores::vault::messaging::get_secret_use_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_use_response
wire_traits<ores::vault::messaging::get_secret_use_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_use_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_secret_uses_request>::write_json(
    const ores::vault::messaging::get_many_secret_uses_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_secret_uses_request
wire_traits<ores::vault::messaging::get_many_secret_uses_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_secret_uses_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_secret_uses_request>::write_msgpack(
    const ores::vault::messaging::get_many_secret_uses_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_secret_uses_request
wire_traits<ores::vault::messaging::get_many_secret_uses_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_secret_uses_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_secret_uses_response>::write_json(
    const ores::vault::messaging::get_many_secret_uses_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_secret_uses_response
wire_traits<ores::vault::messaging::get_many_secret_uses_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_secret_uses_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_secret_uses_response>::write_msgpack(
    const ores::vault::messaging::get_many_secret_uses_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_secret_uses_response
wire_traits<ores::vault::messaging::get_many_secret_uses_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_secret_uses_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_secret_use_request>::write_json(
    const ores::vault::messaging::put_secret_use_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_secret_use_request
wire_traits<ores::vault::messaging::put_secret_use_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_secret_use_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_secret_use_request>::write_msgpack(
    const ores::vault::messaging::put_secret_use_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_secret_use_request
wire_traits<ores::vault::messaging::put_secret_use_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_secret_use_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_secret_use_response>::write_json(
    const ores::vault::messaging::put_secret_use_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_secret_use_response
wire_traits<ores::vault::messaging::put_secret_use_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_secret_use_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_secret_use_response>::write_msgpack(
    const ores::vault::messaging::put_secret_use_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_secret_use_response
wire_traits<ores::vault::messaging::put_secret_use_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_secret_use_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_secret_uses_request>::write_json(
    const ores::vault::messaging::put_many_secret_uses_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_secret_uses_request
wire_traits<ores::vault::messaging::put_many_secret_uses_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_secret_uses_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_secret_uses_request>::write_msgpack(
    const ores::vault::messaging::put_many_secret_uses_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_secret_uses_request
wire_traits<ores::vault::messaging::put_many_secret_uses_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_secret_uses_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_secret_uses_response>::write_json(
    const ores::vault::messaging::put_many_secret_uses_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_secret_uses_response
wire_traits<ores::vault::messaging::put_many_secret_uses_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_secret_uses_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_secret_uses_response>::write_msgpack(
    const ores::vault::messaging::put_many_secret_uses_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_secret_uses_response
wire_traits<ores::vault::messaging::put_many_secret_uses_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_secret_uses_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_secret_use_request>::write_json(
    const ores::vault::messaging::delete_secret_use_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_secret_use_request
wire_traits<ores::vault::messaging::delete_secret_use_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_secret_use_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_secret_use_request>::write_msgpack(
    const ores::vault::messaging::delete_secret_use_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_secret_use_request
wire_traits<ores::vault::messaging::delete_secret_use_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_secret_use_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_secret_use_response>::write_json(
    const ores::vault::messaging::delete_secret_use_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_secret_use_response
wire_traits<ores::vault::messaging::delete_secret_use_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_secret_use_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_secret_use_response>::write_msgpack(
    const ores::vault::messaging::delete_secret_use_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_secret_use_response
wire_traits<ores::vault::messaging::delete_secret_use_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_secret_use_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_secret_uses_request>::write_json(
    const ores::vault::messaging::delete_many_secret_uses_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_secret_uses_request
wire_traits<ores::vault::messaging::delete_many_secret_uses_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_secret_uses_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_secret_uses_request>::write_msgpack(
    const ores::vault::messaging::delete_many_secret_uses_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_secret_uses_request
wire_traits<ores::vault::messaging::delete_many_secret_uses_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_secret_uses_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_secret_uses_response>::write_json(
    const ores::vault::messaging::delete_many_secret_uses_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_secret_uses_response
wire_traits<ores::vault::messaging::delete_many_secret_uses_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_secret_uses_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_secret_uses_response>::write_msgpack(
    const ores::vault::messaging::delete_many_secret_uses_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_secret_uses_response
wire_traits<ores::vault::messaging::delete_many_secret_uses_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_secret_uses_response>(data);
}

}
