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
#include "ores.assets.api/domain/image_tag_codec.hpp"
#include "ores.assets.api/messaging/image_tag_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::assets::messaging::image_tag_key>::write_json(
    const ores::assets::messaging::image_tag_key& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tag_key
wire_traits<ores::assets::messaging::image_tag_key>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tag_key>(text);
}

std::string wire_traits<ores::assets::messaging::image_tag_key>::write_msgpack(
    const ores::assets::messaging::image_tag_key& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tag_key
wire_traits<ores::assets::messaging::image_tag_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tag_key>(data);
}

std::string wire_traits<ores::assets::messaging::image_tag_write>::write_json(
    const ores::assets::messaging::image_tag_write& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tag_write
wire_traits<ores::assets::messaging::image_tag_write>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tag_write>(text);
}

std::string wire_traits<ores::assets::messaging::image_tag_write>::write_msgpack(
    const ores::assets::messaging::image_tag_write& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tag_write
wire_traits<ores::assets::messaging::image_tag_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tag_write>(data);
}

std::string wire_traits<ores::assets::messaging::image_tag_change>::write_json(
    const ores::assets::messaging::image_tag_change& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tag_change
wire_traits<ores::assets::messaging::image_tag_change>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tag_change>(text);
}

std::string wire_traits<ores::assets::messaging::image_tag_change>::write_msgpack(
    const ores::assets::messaging::image_tag_change& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tag_change
wire_traits<ores::assets::messaging::image_tag_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tag_change>(data);
}

std::string wire_traits<ores::assets::messaging::image_tag_removal>::write_json(
    const ores::assets::messaging::image_tag_removal& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tag_removal
wire_traits<ores::assets::messaging::image_tag_removal>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tag_removal>(text);
}

std::string wire_traits<ores::assets::messaging::image_tag_removal>::write_msgpack(
    const ores::assets::messaging::image_tag_removal& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tag_removal
wire_traits<ores::assets::messaging::image_tag_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tag_removal>(data);
}

std::string wire_traits<ores::assets::messaging::image_tag_lookup>::write_json(
    const ores::assets::messaging::image_tag_lookup& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tag_lookup
wire_traits<ores::assets::messaging::image_tag_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tag_lookup>(text);
}

std::string wire_traits<ores::assets::messaging::image_tag_lookup>::write_msgpack(
    const ores::assets::messaging::image_tag_lookup& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tag_lookup
wire_traits<ores::assets::messaging::image_tag_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tag_lookup>(data);
}

std::string wire_traits<ores::assets::messaging::image_tags_filter>::write_json(
    const ores::assets::messaging::image_tags_filter& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_tags_filter
wire_traits<ores::assets::messaging::image_tags_filter>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_tags_filter>(text);
}

std::string wire_traits<ores::assets::messaging::image_tags_filter>::write_msgpack(
    const ores::assets::messaging::image_tags_filter& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_tags_filter
wire_traits<ores::assets::messaging::image_tags_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_tags_filter>(data);
}

std::string wire_traits<ores::assets::messaging::list_image_tags_request>::write_json(
    const ores::assets::messaging::list_image_tags_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_image_tags_request
wire_traits<ores::assets::messaging::list_image_tags_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::list_image_tags_request>(text);
}

std::string wire_traits<ores::assets::messaging::list_image_tags_request>::write_msgpack(
    const ores::assets::messaging::list_image_tags_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_image_tags_request
wire_traits<ores::assets::messaging::list_image_tags_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_image_tags_request>(data);
}

std::string wire_traits<ores::assets::messaging::list_image_tags_response>::write_json(
    const ores::assets::messaging::list_image_tags_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_image_tags_response
wire_traits<ores::assets::messaging::list_image_tags_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::list_image_tags_response>(text);
}

std::string wire_traits<ores::assets::messaging::list_image_tags_response>::write_msgpack(
    const ores::assets::messaging::list_image_tags_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_image_tags_response
wire_traits<ores::assets::messaging::list_image_tags_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_image_tags_response>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_tag_request>::write_json(
    const ores::assets::messaging::get_image_tag_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_tag_request
wire_traits<ores::assets::messaging::get_image_tag_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_tag_request>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_tag_request>::write_msgpack(
    const ores::assets::messaging::get_image_tag_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_tag_request
wire_traits<ores::assets::messaging::get_image_tag_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_tag_request>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_tag_response>::write_json(
    const ores::assets::messaging::get_image_tag_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_tag_response
wire_traits<ores::assets::messaging::get_image_tag_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_tag_response>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_tag_response>::write_msgpack(
    const ores::assets::messaging::get_image_tag_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_tag_response
wire_traits<ores::assets::messaging::get_image_tag_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_tag_response>(data);
}

std::string wire_traits<ores::assets::messaging::get_many_image_tags_request>::write_json(
    const ores::assets::messaging::get_many_image_tags_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_many_image_tags_request
wire_traits<ores::assets::messaging::get_many_image_tags_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::get_many_image_tags_request>(text);
}

std::string wire_traits<ores::assets::messaging::get_many_image_tags_request>::write_msgpack(
    const ores::assets::messaging::get_many_image_tags_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_many_image_tags_request
wire_traits<ores::assets::messaging::get_many_image_tags_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_many_image_tags_request>(data);
}

std::string wire_traits<ores::assets::messaging::get_many_image_tags_response>::write_json(
    const ores::assets::messaging::get_many_image_tags_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_many_image_tags_response
wire_traits<ores::assets::messaging::get_many_image_tags_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::get_many_image_tags_response>(text);
}

std::string wire_traits<ores::assets::messaging::get_many_image_tags_response>::write_msgpack(
    const ores::assets::messaging::get_many_image_tags_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_many_image_tags_response
wire_traits<ores::assets::messaging::get_many_image_tags_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_many_image_tags_response>(data);
}

std::string wire_traits<ores::assets::messaging::put_image_tag_request>::write_json(
    const ores::assets::messaging::put_image_tag_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_image_tag_request
wire_traits<ores::assets::messaging::put_image_tag_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_image_tag_request>(text);
}

std::string wire_traits<ores::assets::messaging::put_image_tag_request>::write_msgpack(
    const ores::assets::messaging::put_image_tag_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_image_tag_request
wire_traits<ores::assets::messaging::put_image_tag_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_image_tag_request>(data);
}

std::string wire_traits<ores::assets::messaging::put_image_tag_response>::write_json(
    const ores::assets::messaging::put_image_tag_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_image_tag_response
wire_traits<ores::assets::messaging::put_image_tag_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_image_tag_response>(text);
}

std::string wire_traits<ores::assets::messaging::put_image_tag_response>::write_msgpack(
    const ores::assets::messaging::put_image_tag_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_image_tag_response
wire_traits<ores::assets::messaging::put_image_tag_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_image_tag_response>(data);
}

std::string wire_traits<ores::assets::messaging::put_many_image_tags_request>::write_json(
    const ores::assets::messaging::put_many_image_tags_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_many_image_tags_request
wire_traits<ores::assets::messaging::put_many_image_tags_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::put_many_image_tags_request>(text);
}

std::string wire_traits<ores::assets::messaging::put_many_image_tags_request>::write_msgpack(
    const ores::assets::messaging::put_many_image_tags_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_many_image_tags_request
wire_traits<ores::assets::messaging::put_many_image_tags_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_many_image_tags_request>(data);
}

std::string wire_traits<ores::assets::messaging::put_many_image_tags_response>::write_json(
    const ores::assets::messaging::put_many_image_tags_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_many_image_tags_response
wire_traits<ores::assets::messaging::put_many_image_tags_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::put_many_image_tags_response>(text);
}

std::string wire_traits<ores::assets::messaging::put_many_image_tags_response>::write_msgpack(
    const ores::assets::messaging::put_many_image_tags_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_many_image_tags_response
wire_traits<ores::assets::messaging::put_many_image_tags_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_many_image_tags_response>(data);
}

std::string wire_traits<ores::assets::messaging::delete_image_tag_request>::write_json(
    const ores::assets::messaging::delete_image_tag_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_image_tag_request
wire_traits<ores::assets::messaging::delete_image_tag_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_image_tag_request>(text);
}

std::string wire_traits<ores::assets::messaging::delete_image_tag_request>::write_msgpack(
    const ores::assets::messaging::delete_image_tag_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_image_tag_request
wire_traits<ores::assets::messaging::delete_image_tag_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_image_tag_request>(data);
}

std::string wire_traits<ores::assets::messaging::delete_image_tag_response>::write_json(
    const ores::assets::messaging::delete_image_tag_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_image_tag_response
wire_traits<ores::assets::messaging::delete_image_tag_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_image_tag_response>(text);
}

std::string wire_traits<ores::assets::messaging::delete_image_tag_response>::write_msgpack(
    const ores::assets::messaging::delete_image_tag_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_image_tag_response
wire_traits<ores::assets::messaging::delete_image_tag_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_image_tag_response>(data);
}

std::string wire_traits<ores::assets::messaging::delete_many_image_tags_request>::write_json(
    const ores::assets::messaging::delete_many_image_tags_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_many_image_tags_request
wire_traits<ores::assets::messaging::delete_many_image_tags_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_many_image_tags_request>(text);
}

std::string wire_traits<ores::assets::messaging::delete_many_image_tags_request>::write_msgpack(
    const ores::assets::messaging::delete_many_image_tags_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_many_image_tags_request
wire_traits<ores::assets::messaging::delete_many_image_tags_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_many_image_tags_request>(data);
}

std::string wire_traits<ores::assets::messaging::delete_many_image_tags_response>::write_json(
    const ores::assets::messaging::delete_many_image_tags_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_many_image_tags_response
wire_traits<ores::assets::messaging::delete_many_image_tags_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_many_image_tags_response>(text);
}

std::string wire_traits<ores::assets::messaging::delete_many_image_tags_response>::write_msgpack(
    const ores::assets::messaging::delete_many_image_tags_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_many_image_tags_response
wire_traits<ores::assets::messaging::delete_many_image_tags_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_many_image_tags_response>(data);
}

std::string wire_traits<ores::assets::messaging::list_by_image_id_image_tags_request>::write_json(
    const ores::assets::messaging::list_by_image_id_image_tags_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_by_image_id_image_tags_request
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::list_by_image_id_image_tags_request>(text);
}

std::string
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_request>::write_msgpack(
    const ores::assets::messaging::list_by_image_id_image_tags_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_by_image_id_image_tags_request
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_by_image_id_image_tags_request>(data);
}

std::string wire_traits<ores::assets::messaging::list_by_image_id_image_tags_response>::write_json(
    const ores::assets::messaging::list_by_image_id_image_tags_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_by_image_id_image_tags_response
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::list_by_image_id_image_tags_response>(text);
}

std::string
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_response>::write_msgpack(
    const ores::assets::messaging::list_by_image_id_image_tags_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_by_image_id_image_tags_response
wire_traits<ores::assets::messaging::list_by_image_id_image_tags_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_by_image_id_image_tags_response>(
        data);
}

}
