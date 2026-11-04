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
#include "ores.synthetic.api/domain/folder_codec.hpp"
#include "ores.synthetic.api/messaging/folder_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::folder_key>::write_json(
    const ores::synthetic::messaging::folder_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_key
wire_traits<ores::synthetic::messaging::folder_key>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_key>::write_msgpack(
    const ores::synthetic::messaging::folder_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_key
wire_traits<ores::synthetic::messaging::folder_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_write>::write_json(
    const ores::synthetic::messaging::folder_write& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_write
wire_traits<ores::synthetic::messaging::folder_write>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_write>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_write>::write_msgpack(
    const ores::synthetic::messaging::folder_write& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_write
wire_traits<ores::synthetic::messaging::folder_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_write>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_change>::write_json(
    const ores::synthetic::messaging::folder_change& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_change
wire_traits<ores::synthetic::messaging::folder_change>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_change>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_change>::write_msgpack(
    const ores::synthetic::messaging::folder_change& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_change
wire_traits<ores::synthetic::messaging::folder_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_change>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_removal>::write_json(
    const ores::synthetic::messaging::folder_removal& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_removal
wire_traits<ores::synthetic::messaging::folder_removal>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_removal>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_removal>::write_msgpack(
    const ores::synthetic::messaging::folder_removal& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_removal
wire_traits<ores::synthetic::messaging::folder_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_removal>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_lookup>::write_json(
    const ores::synthetic::messaging::folder_lookup& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_lookup
wire_traits<ores::synthetic::messaging::folder_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_lookup>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_lookup>::write_msgpack(
    const ores::synthetic::messaging::folder_lookup& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_lookup
wire_traits<ores::synthetic::messaging::folder_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_lookup>(data);
}

std::string wire_traits<ores::synthetic::messaging::folders_filter>::write_json(
    const ores::synthetic::messaging::folders_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folders_filter
wire_traits<ores::synthetic::messaging::folders_filter>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folders_filter>(text);
}

std::string wire_traits<ores::synthetic::messaging::folders_filter>::write_msgpack(
    const ores::synthetic::messaging::folders_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folders_filter
wire_traits<ores::synthetic::messaging::folders_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folders_filter>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_event>::write_json(
    const ores::synthetic::messaging::folder_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_event
wire_traits<ores::synthetic::messaging::folder_event>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_event>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_event>::write_msgpack(
    const ores::synthetic::messaging::folder_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_event
wire_traits<ores::synthetic::messaging::folder_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_event>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_version_key>::write_json(
    const ores::synthetic::messaging::folder_version_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_version_key
wire_traits<ores::synthetic::messaging::folder_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_version_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_version_key>::write_msgpack(
    const ores::synthetic::messaging::folder_version_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_version_key
wire_traits<ores::synthetic::messaging::folder_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_version_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::folder_versions_filter>::write_json(
    const ores::synthetic::messaging::folder_versions_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::folder_versions_filter
wire_traits<ores::synthetic::messaging::folder_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::folder_versions_filter>(text);
}

std::string wire_traits<ores::synthetic::messaging::folder_versions_filter>::write_msgpack(
    const ores::synthetic::messaging::folder_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::folder_versions_filter
wire_traits<ores::synthetic::messaging::folder_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::folder_versions_filter>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_folders_request>::write_json(
    const ores::synthetic::messaging::list_folders_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_folders_request
wire_traits<ores::synthetic::messaging::list_folders_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_folders_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_folders_request>::write_msgpack(
    const ores::synthetic::messaging::list_folders_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_folders_request
wire_traits<ores::synthetic::messaging::list_folders_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_folders_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_folders_response>::write_json(
    const ores::synthetic::messaging::list_folders_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_folders_response
wire_traits<ores::synthetic::messaging::list_folders_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_folders_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_folders_response>::write_msgpack(
    const ores::synthetic::messaging::list_folders_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_folders_response
wire_traits<ores::synthetic::messaging::list_folders_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_folders_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_request>::write_json(
    const ores::synthetic::messaging::get_folder_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_folder_request
wire_traits<ores::synthetic::messaging::get_folder_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_folder_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_request>::write_msgpack(
    const ores::synthetic::messaging::get_folder_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_folder_request
wire_traits<ores::synthetic::messaging::get_folder_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_folder_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_response>::write_json(
    const ores::synthetic::messaging::get_folder_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_folder_response
wire_traits<ores::synthetic::messaging::get_folder_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_folder_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_response>::write_msgpack(
    const ores::synthetic::messaging::get_folder_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_folder_response
wire_traits<ores::synthetic::messaging::get_folder_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_folder_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_many_folders_request>::write_json(
    const ores::synthetic::messaging::get_many_folders_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_folders_request
wire_traits<ores::synthetic::messaging::get_many_folders_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_folders_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_many_folders_request>::write_msgpack(
    const ores::synthetic::messaging::get_many_folders_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_folders_request
wire_traits<ores::synthetic::messaging::get_many_folders_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_folders_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_many_folders_response>::write_json(
    const ores::synthetic::messaging::get_many_folders_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_folders_response
wire_traits<ores::synthetic::messaging::get_many_folders_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_folders_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_many_folders_response>::write_msgpack(
    const ores::synthetic::messaging::get_many_folders_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_folders_response
wire_traits<ores::synthetic::messaging::get_many_folders_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_folders_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_folder_request>::write_json(
    const ores::synthetic::messaging::put_folder_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_folder_request
wire_traits<ores::synthetic::messaging::put_folder_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_folder_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_folder_request>::write_msgpack(
    const ores::synthetic::messaging::put_folder_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_folder_request
wire_traits<ores::synthetic::messaging::put_folder_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_folder_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_folder_response>::write_json(
    const ores::synthetic::messaging::put_folder_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_folder_response
wire_traits<ores::synthetic::messaging::put_folder_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_folder_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_folder_response>::write_msgpack(
    const ores::synthetic::messaging::put_folder_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_folder_response
wire_traits<ores::synthetic::messaging::put_folder_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_folder_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_many_folders_request>::write_json(
    const ores::synthetic::messaging::put_many_folders_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_folders_request
wire_traits<ores::synthetic::messaging::put_many_folders_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_folders_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_many_folders_request>::write_msgpack(
    const ores::synthetic::messaging::put_many_folders_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_folders_request
wire_traits<ores::synthetic::messaging::put_many_folders_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_folders_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_many_folders_response>::write_json(
    const ores::synthetic::messaging::put_many_folders_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_folders_response
wire_traits<ores::synthetic::messaging::put_many_folders_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_folders_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_many_folders_response>::write_msgpack(
    const ores::synthetic::messaging::put_many_folders_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_folders_response
wire_traits<ores::synthetic::messaging::put_many_folders_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_folders_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::delete_folder_request>::write_json(
    const ores::synthetic::messaging::delete_folder_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_folder_request
wire_traits<ores::synthetic::messaging::delete_folder_request>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_folder_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::delete_folder_request>::write_msgpack(
    const ores::synthetic::messaging::delete_folder_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_folder_request
wire_traits<ores::synthetic::messaging::delete_folder_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_folder_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::delete_folder_response>::write_json(
    const ores::synthetic::messaging::delete_folder_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_folder_response
wire_traits<ores::synthetic::messaging::delete_folder_response>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_folder_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::delete_folder_response>::write_msgpack(
    const ores::synthetic::messaging::delete_folder_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_folder_response
wire_traits<ores::synthetic::messaging::delete_folder_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_folder_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::delete_many_folders_request>::write_json(
    const ores::synthetic::messaging::delete_many_folders_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_folders_request
wire_traits<ores::synthetic::messaging::delete_many_folders_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_folders_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::delete_many_folders_request>::write_msgpack(
    const ores::synthetic::messaging::delete_many_folders_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_folders_request
wire_traits<ores::synthetic::messaging::delete_many_folders_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_many_folders_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::delete_many_folders_response>::write_json(
    const ores::synthetic::messaging::delete_many_folders_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_folders_response
wire_traits<ores::synthetic::messaging::delete_many_folders_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_folders_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::delete_many_folders_response>::write_msgpack(
    const ores::synthetic::messaging::delete_many_folders_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_folders_response
wire_traits<ores::synthetic::messaging::delete_many_folders_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_many_folders_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_folder_versions_request>::write_json(
    const ores::synthetic::messaging::list_folder_versions_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_folder_versions_request
wire_traits<ores::synthetic::messaging::list_folder_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_folder_versions_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_folder_versions_request>::write_msgpack(
    const ores::synthetic::messaging::list_folder_versions_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_folder_versions_request
wire_traits<ores::synthetic::messaging::list_folder_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_folder_versions_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::list_folder_versions_response>::write_json(
    const ores::synthetic::messaging::list_folder_versions_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_folder_versions_response
wire_traits<ores::synthetic::messaging::list_folder_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_folder_versions_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::list_folder_versions_response>::write_msgpack(
    const ores::synthetic::messaging::list_folder_versions_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_folder_versions_response
wire_traits<ores::synthetic::messaging::list_folder_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_folder_versions_response>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_version_request>::write_json(
    const ores::synthetic::messaging::get_folder_version_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_folder_version_request
wire_traits<ores::synthetic::messaging::get_folder_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_folder_version_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_version_request>::write_msgpack(
    const ores::synthetic::messaging::get_folder_version_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_folder_version_request
wire_traits<ores::synthetic::messaging::get_folder_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_folder_version_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_version_response>::write_json(
    const ores::synthetic::messaging::get_folder_version_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_folder_version_response
wire_traits<ores::synthetic::messaging::get_folder_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_folder_version_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_folder_version_response>::write_msgpack(
    const ores::synthetic::messaging::get_folder_version_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_folder_version_response
wire_traits<ores::synthetic::messaging::get_folder_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_folder_version_response>(data);
}

}
