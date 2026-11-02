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
#include "ores.inbox.api/domain/approval_part_codec.hpp"
#include "ores.inbox.api/messaging/approval_part_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::approval_part_key>::write_json(
    const ores::inbox::messaging::approval_part_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_key
wire_traits<ores::inbox::messaging::approval_part_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_key>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_key>::write_msgpack(
    const ores::inbox::messaging::approval_part_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_key
wire_traits<ores::inbox::messaging::approval_part_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_key>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_write>::write_json(
    const ores::inbox::messaging::approval_part_write& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_write
wire_traits<ores::inbox::messaging::approval_part_write>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_write>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_write>::write_msgpack(
    const ores::inbox::messaging::approval_part_write& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_write
wire_traits<ores::inbox::messaging::approval_part_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_write>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_change>::write_json(
    const ores::inbox::messaging::approval_part_change& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_change
wire_traits<ores::inbox::messaging::approval_part_change>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_change>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_change>::write_msgpack(
    const ores::inbox::messaging::approval_part_change& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_change
wire_traits<ores::inbox::messaging::approval_part_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_change>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_removal>::write_json(
    const ores::inbox::messaging::approval_part_removal& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_removal
wire_traits<ores::inbox::messaging::approval_part_removal>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_removal>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_removal>::write_msgpack(
    const ores::inbox::messaging::approval_part_removal& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_removal
wire_traits<ores::inbox::messaging::approval_part_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_removal>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_lookup>::write_json(
    const ores::inbox::messaging::approval_part_lookup& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_lookup
wire_traits<ores::inbox::messaging::approval_part_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_lookup>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_lookup>::write_msgpack(
    const ores::inbox::messaging::approval_part_lookup& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_lookup
wire_traits<ores::inbox::messaging::approval_part_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_lookup>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_parts_filter>::write_json(
    const ores::inbox::messaging::approval_parts_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_parts_filter
wire_traits<ores::inbox::messaging::approval_parts_filter>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_parts_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_parts_filter>::write_msgpack(
    const ores::inbox::messaging::approval_parts_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_parts_filter
wire_traits<ores::inbox::messaging::approval_parts_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_parts_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_event>::write_json(
    const ores::inbox::messaging::approval_part_event& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_event
wire_traits<ores::inbox::messaging::approval_part_event>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_event>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_event>::write_msgpack(
    const ores::inbox::messaging::approval_part_event& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_event
wire_traits<ores::inbox::messaging::approval_part_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_event>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_version_key>::write_json(
    const ores::inbox::messaging::approval_part_version_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_version_key
wire_traits<ores::inbox::messaging::approval_part_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_version_key>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_version_key>::write_msgpack(
    const ores::inbox::messaging::approval_part_version_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_version_key
wire_traits<ores::inbox::messaging::approval_part_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_version_key>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_versions_filter>::write_json(
    const ores::inbox::messaging::approval_part_versions_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_versions_filter
wire_traits<ores::inbox::messaging::approval_part_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_versions_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_versions_filter>::write_msgpack(
    const ores::inbox::messaging::approval_part_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_versions_filter
wire_traits<ores::inbox::messaging::approval_part_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_versions_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_parts_request>::write_json(
    const ores::inbox::messaging::list_approval_parts_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_parts_request
wire_traits<ores::inbox::messaging::list_approval_parts_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_parts_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_approval_parts_request>::write_msgpack(
    const ores::inbox::messaging::list_approval_parts_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_parts_request
wire_traits<ores::inbox::messaging::list_approval_parts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_parts_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_parts_response>::write_json(
    const ores::inbox::messaging::list_approval_parts_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_parts_response
wire_traits<ores::inbox::messaging::list_approval_parts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_parts_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_approval_parts_response>::write_msgpack(
    const ores::inbox::messaging::list_approval_parts_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_parts_response
wire_traits<ores::inbox::messaging::list_approval_parts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_parts_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_request>::write_json(
    const ores::inbox::messaging::get_approval_part_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_part_request
wire_traits<ores::inbox::messaging::get_approval_part_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_part_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_request>::write_msgpack(
    const ores::inbox::messaging::get_approval_part_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_part_request
wire_traits<ores::inbox::messaging::get_approval_part_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_part_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_response>::write_json(
    const ores::inbox::messaging::get_approval_part_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_part_response
wire_traits<ores::inbox::messaging::get_approval_part_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_part_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_response>::write_msgpack(
    const ores::inbox::messaging::get_approval_part_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_part_response
wire_traits<ores::inbox::messaging::get_approval_part_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_part_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_many_approval_parts_request>::write_json(
    const ores::inbox::messaging::get_many_approval_parts_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_approval_parts_request
wire_traits<ores::inbox::messaging::get_many_approval_parts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_approval_parts_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_many_approval_parts_request>::write_msgpack(
    const ores::inbox::messaging::get_many_approval_parts_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_approval_parts_request
wire_traits<ores::inbox::messaging::get_many_approval_parts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_approval_parts_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_many_approval_parts_response>::write_json(
    const ores::inbox::messaging::get_many_approval_parts_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_approval_parts_response
wire_traits<ores::inbox::messaging::get_many_approval_parts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_approval_parts_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_many_approval_parts_response>::write_msgpack(
    const ores::inbox::messaging::get_many_approval_parts_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_approval_parts_response
wire_traits<ores::inbox::messaging::get_many_approval_parts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_approval_parts_response>(data);
}

std::string wire_traits<ores::inbox::messaging::put_approval_part_request>::write_json(
    const ores::inbox::messaging::put_approval_part_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_approval_part_request
wire_traits<ores::inbox::messaging::put_approval_part_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_approval_part_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_approval_part_request>::write_msgpack(
    const ores::inbox::messaging::put_approval_part_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_approval_part_request
wire_traits<ores::inbox::messaging::put_approval_part_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_approval_part_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_approval_part_response>::write_json(
    const ores::inbox::messaging::put_approval_part_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_approval_part_response
wire_traits<ores::inbox::messaging::put_approval_part_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_approval_part_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_approval_part_response>::write_msgpack(
    const ores::inbox::messaging::put_approval_part_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_approval_part_response
wire_traits<ores::inbox::messaging::put_approval_part_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_approval_part_response>(data);
}

std::string wire_traits<ores::inbox::messaging::put_many_approval_parts_request>::write_json(
    const ores::inbox::messaging::put_many_approval_parts_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_approval_parts_request
wire_traits<ores::inbox::messaging::put_many_approval_parts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_approval_parts_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_many_approval_parts_request>::write_msgpack(
    const ores::inbox::messaging::put_many_approval_parts_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_approval_parts_request
wire_traits<ores::inbox::messaging::put_many_approval_parts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_approval_parts_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_many_approval_parts_response>::write_json(
    const ores::inbox::messaging::put_many_approval_parts_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_approval_parts_response
wire_traits<ores::inbox::messaging::put_many_approval_parts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_approval_parts_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_many_approval_parts_response>::write_msgpack(
    const ores::inbox::messaging::put_many_approval_parts_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_approval_parts_response
wire_traits<ores::inbox::messaging::put_many_approval_parts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_approval_parts_response>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_approval_part_request>::write_json(
    const ores::inbox::messaging::delete_approval_part_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_approval_part_request
wire_traits<ores::inbox::messaging::delete_approval_part_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_approval_part_request>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_approval_part_request>::write_msgpack(
    const ores::inbox::messaging::delete_approval_part_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_approval_part_request
wire_traits<ores::inbox::messaging::delete_approval_part_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_approval_part_request>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_approval_part_response>::write_json(
    const ores::inbox::messaging::delete_approval_part_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_approval_part_response
wire_traits<ores::inbox::messaging::delete_approval_part_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_approval_part_response>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_approval_part_response>::write_msgpack(
    const ores::inbox::messaging::delete_approval_part_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_approval_part_response
wire_traits<ores::inbox::messaging::delete_approval_part_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_approval_part_response>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_many_approval_parts_request>::write_json(
    const ores::inbox::messaging::delete_many_approval_parts_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_approval_parts_request
wire_traits<ores::inbox::messaging::delete_many_approval_parts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_approval_parts_request>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_many_approval_parts_request>::write_msgpack(
    const ores::inbox::messaging::delete_many_approval_parts_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_approval_parts_request
wire_traits<ores::inbox::messaging::delete_many_approval_parts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_many_approval_parts_request>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_many_approval_parts_response>::write_json(
    const ores::inbox::messaging::delete_many_approval_parts_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_approval_parts_response
wire_traits<ores::inbox::messaging::delete_many_approval_parts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_approval_parts_response>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_many_approval_parts_response>::write_msgpack(
    const ores::inbox::messaging::delete_many_approval_parts_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_approval_parts_response
wire_traits<ores::inbox::messaging::delete_many_approval_parts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_many_approval_parts_response>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_part_versions_request>::write_json(
    const ores::inbox::messaging::list_approval_part_versions_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_part_versions_request
wire_traits<ores::inbox::messaging::list_approval_part_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_part_versions_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_approval_part_versions_request>::write_msgpack(
    const ores::inbox::messaging::list_approval_part_versions_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_part_versions_request
wire_traits<ores::inbox::messaging::list_approval_part_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_part_versions_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_part_versions_response>::write_json(
    const ores::inbox::messaging::list_approval_part_versions_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_part_versions_response
wire_traits<ores::inbox::messaging::list_approval_part_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_part_versions_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_approval_part_versions_response>::write_msgpack(
    const ores::inbox::messaging::list_approval_part_versions_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_part_versions_response
wire_traits<ores::inbox::messaging::list_approval_part_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_part_versions_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_version_request>::write_json(
    const ores::inbox::messaging::get_approval_part_version_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_part_version_request
wire_traits<ores::inbox::messaging::get_approval_part_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_part_version_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_version_request>::write_msgpack(
    const ores::inbox::messaging::get_approval_part_version_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_part_version_request
wire_traits<ores::inbox::messaging::get_approval_part_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_part_version_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_version_response>::write_json(
    const ores::inbox::messaging::get_approval_part_version_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_part_version_response
wire_traits<ores::inbox::messaging::get_approval_part_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_part_version_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_part_version_response>::write_msgpack(
    const ores::inbox::messaging::get_approval_part_version_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_part_version_response
wire_traits<ores::inbox::messaging::get_approval_part_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_part_version_response>(data);
}

}
