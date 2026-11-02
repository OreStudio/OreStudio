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
#include "ores.iam.api/domain/role_permission_codec.hpp"
#include "ores.iam.api/messaging/role_permission_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::role_permission_key>::write_json(
    const ores::iam::messaging::role_permission_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_key
wire_traits<ores::iam::messaging::role_permission_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_key>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_key>::write_msgpack(
    const ores::iam::messaging::role_permission_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_key
wire_traits<ores::iam::messaging::role_permission_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_key>(data);
}

std::string wire_traits<ores::iam::messaging::role_permission_write>::write_json(
    const ores::iam::messaging::role_permission_write& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_write
wire_traits<ores::iam::messaging::role_permission_write>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_write>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_write>::write_msgpack(
    const ores::iam::messaging::role_permission_write& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_write
wire_traits<ores::iam::messaging::role_permission_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_write>(data);
}

std::string wire_traits<ores::iam::messaging::role_permission_change>::write_json(
    const ores::iam::messaging::role_permission_change& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_change
wire_traits<ores::iam::messaging::role_permission_change>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_change>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_change>::write_msgpack(
    const ores::iam::messaging::role_permission_change& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_change
wire_traits<ores::iam::messaging::role_permission_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_change>(data);
}

std::string wire_traits<ores::iam::messaging::role_permission_removal>::write_json(
    const ores::iam::messaging::role_permission_removal& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_removal
wire_traits<ores::iam::messaging::role_permission_removal>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_removal>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_removal>::write_msgpack(
    const ores::iam::messaging::role_permission_removal& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_removal
wire_traits<ores::iam::messaging::role_permission_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_removal>(data);
}

std::string wire_traits<ores::iam::messaging::role_permission_lookup>::write_json(
    const ores::iam::messaging::role_permission_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_lookup
wire_traits<ores::iam::messaging::role_permission_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_lookup>::write_msgpack(
    const ores::iam::messaging::role_permission_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_lookup
wire_traits<ores::iam::messaging::role_permission_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::role_permissions_filter>::write_json(
    const ores::iam::messaging::role_permissions_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permissions_filter
wire_traits<ores::iam::messaging::role_permissions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permissions_filter>(text);
}

std::string wire_traits<ores::iam::messaging::role_permissions_filter>::write_msgpack(
    const ores::iam::messaging::role_permissions_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permissions_filter
wire_traits<ores::iam::messaging::role_permissions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permissions_filter>(data);
}

std::string wire_traits<ores::iam::messaging::role_permission_event>::write_json(
    const ores::iam::messaging::role_permission_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_permission_event
wire_traits<ores::iam::messaging::role_permission_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_permission_event>(text);
}

std::string wire_traits<ores::iam::messaging::role_permission_event>::write_msgpack(
    const ores::iam::messaging::role_permission_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_permission_event
wire_traits<ores::iam::messaging::role_permission_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_permission_event>(data);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_request>::write_json(
    const ores::iam::messaging::list_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_role_permissions_request
wire_traits<ores::iam::messaging::list_role_permissions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::list_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_role_permissions_request
wire_traits<ores::iam::messaging::list_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_response>::write_json(
    const ores::iam::messaging::list_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_role_permissions_response
wire_traits<ores::iam::messaging::list_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_role_permissions_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::list_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_role_permissions_response
wire_traits<ores::iam::messaging::list_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_role_permissions_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_role_permission_request>::write_json(
    const ores::iam::messaging::get_role_permission_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_role_permission_request
wire_traits<ores::iam::messaging::get_role_permission_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_role_permission_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_role_permission_request>::write_msgpack(
    const ores::iam::messaging::get_role_permission_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_role_permission_request
wire_traits<ores::iam::messaging::get_role_permission_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_role_permission_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_role_permission_response>::write_json(
    const ores::iam::messaging::get_role_permission_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_role_permission_response
wire_traits<ores::iam::messaging::get_role_permission_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_role_permission_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_role_permission_response>::write_msgpack(
    const ores::iam::messaging::get_role_permission_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_role_permission_response
wire_traits<ores::iam::messaging::get_role_permission_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_role_permission_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_role_permissions_request>::write_json(
    const ores::iam::messaging::get_many_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_role_permissions_request
wire_traits<ores::iam::messaging::get_many_role_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::get_many_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_role_permissions_request
wire_traits<ores::iam::messaging::get_many_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_role_permissions_response>::write_json(
    const ores::iam::messaging::get_many_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_role_permissions_response
wire_traits<ores::iam::messaging::get_many_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_role_permissions_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::get_many_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_role_permissions_response
wire_traits<ores::iam::messaging::get_many_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_role_permissions_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_role_permission_request>::write_json(
    const ores::iam::messaging::put_role_permission_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_role_permission_request
wire_traits<ores::iam::messaging::put_role_permission_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_role_permission_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_role_permission_request>::write_msgpack(
    const ores::iam::messaging::put_role_permission_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_role_permission_request
wire_traits<ores::iam::messaging::put_role_permission_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_role_permission_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_role_permission_response>::write_json(
    const ores::iam::messaging::put_role_permission_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_role_permission_response
wire_traits<ores::iam::messaging::put_role_permission_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_role_permission_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_role_permission_response>::write_msgpack(
    const ores::iam::messaging::put_role_permission_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_role_permission_response
wire_traits<ores::iam::messaging::put_role_permission_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_role_permission_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_role_permissions_request>::write_json(
    const ores::iam::messaging::put_many_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_role_permissions_request
wire_traits<ores::iam::messaging::put_many_role_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::put_many_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_role_permissions_request
wire_traits<ores::iam::messaging::put_many_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_role_permissions_response>::write_json(
    const ores::iam::messaging::put_many_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_role_permissions_response
wire_traits<ores::iam::messaging::put_many_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_role_permissions_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::put_many_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_role_permissions_response
wire_traits<ores::iam::messaging::put_many_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_role_permissions_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_role_permission_request>::write_json(
    const ores::iam::messaging::delete_role_permission_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_role_permission_request
wire_traits<ores::iam::messaging::delete_role_permission_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_role_permission_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_role_permission_request>::write_msgpack(
    const ores::iam::messaging::delete_role_permission_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_role_permission_request
wire_traits<ores::iam::messaging::delete_role_permission_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_role_permission_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_role_permission_response>::write_json(
    const ores::iam::messaging::delete_role_permission_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_role_permission_response
wire_traits<ores::iam::messaging::delete_role_permission_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_role_permission_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_role_permission_response>::write_msgpack(
    const ores::iam::messaging::delete_role_permission_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_role_permission_response
wire_traits<ores::iam::messaging::delete_role_permission_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_role_permission_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_role_permissions_request>::write_json(
    const ores::iam::messaging::delete_many_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_role_permissions_request
wire_traits<ores::iam::messaging::delete_many_role_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::delete_many_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_role_permissions_request
wire_traits<ores::iam::messaging::delete_many_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_role_permissions_response>::write_json(
    const ores::iam::messaging::delete_many_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_role_permissions_response
wire_traits<ores::iam::messaging::delete_many_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_role_permissions_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::delete_many_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_role_permissions_response
wire_traits<ores::iam::messaging::delete_many_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_role_permissions_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_request>::write_json(
    const ores::iam::messaging::list_by_role_id_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_role_id_role_permissions_request
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_role_id_role_permissions_request>(text);
}

std::string
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::list_by_role_id_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_role_id_role_permissions_request
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_role_id_role_permissions_request>(
        data);
}

std::string
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_response>::write_json(
    const ores::iam::messaging::list_by_role_id_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_role_id_role_permissions_response
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_role_id_role_permissions_response>(text);
}

std::string
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::list_by_role_id_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_role_id_role_permissions_response
wire_traits<ores::iam::messaging::list_by_role_id_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_role_id_role_permissions_response>(
        data);
}

std::string
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_request>::write_json(
    const ores::iam::messaging::list_by_permission_id_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_permission_id_role_permissions_request
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_permission_id_role_permissions_request>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::list_by_permission_id_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_permission_id_role_permissions_request
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_permission_id_role_permissions_request>(data);
}

std::string
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_response>::write_json(
    const ores::iam::messaging::list_by_permission_id_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_permission_id_role_permissions_response
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_permission_id_role_permissions_response>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::list_by_permission_id_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_permission_id_role_permissions_response
wire_traits<ores::iam::messaging::list_by_permission_id_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::list_by_permission_id_role_permissions_response>(data);
}

}
