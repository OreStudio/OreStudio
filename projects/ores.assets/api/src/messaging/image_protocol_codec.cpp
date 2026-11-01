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
#include "ores.assets.api/domain/image_codec.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::assets::messaging::image_key>::write_json(
    const ores::assets::messaging::image_key& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_key
wire_traits<ores::assets::messaging::image_key>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_key>(text);
}

std::string wire_traits<ores::assets::messaging::image_key>::write_msgpack(
    const ores::assets::messaging::image_key& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_key
wire_traits<ores::assets::messaging::image_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_key>(data);
}

std::string wire_traits<ores::assets::messaging::image_write>::write_json(
    const ores::assets::messaging::image_write& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_write
wire_traits<ores::assets::messaging::image_write>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_write>(text);
}

std::string wire_traits<ores::assets::messaging::image_write>::write_msgpack(
    const ores::assets::messaging::image_write& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_write
wire_traits<ores::assets::messaging::image_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_write>(data);
}

std::string wire_traits<ores::assets::messaging::image_change>::write_json(
    const ores::assets::messaging::image_change& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_change
wire_traits<ores::assets::messaging::image_change>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_change>(text);
}

std::string wire_traits<ores::assets::messaging::image_change>::write_msgpack(
    const ores::assets::messaging::image_change& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_change
wire_traits<ores::assets::messaging::image_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_change>(data);
}

std::string wire_traits<ores::assets::messaging::image_removal>::write_json(
    const ores::assets::messaging::image_removal& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_removal
wire_traits<ores::assets::messaging::image_removal>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_removal>(text);
}

std::string wire_traits<ores::assets::messaging::image_removal>::write_msgpack(
    const ores::assets::messaging::image_removal& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_removal
wire_traits<ores::assets::messaging::image_removal>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_removal>(data);
}

std::string wire_traits<ores::assets::messaging::image_lookup>::write_json(
    const ores::assets::messaging::image_lookup& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_lookup
wire_traits<ores::assets::messaging::image_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_lookup>(text);
}

std::string wire_traits<ores::assets::messaging::image_lookup>::write_msgpack(
    const ores::assets::messaging::image_lookup& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_lookup
wire_traits<ores::assets::messaging::image_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_lookup>(data);
}

std::string wire_traits<ores::assets::messaging::images_filter>::write_json(
    const ores::assets::messaging::images_filter& v) {
    return object::write_json(v);
}

ores::assets::messaging::images_filter
wire_traits<ores::assets::messaging::images_filter>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::images_filter>(text);
}

std::string wire_traits<ores::assets::messaging::images_filter>::write_msgpack(
    const ores::assets::messaging::images_filter& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::images_filter
wire_traits<ores::assets::messaging::images_filter>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::images_filter>(data);
}

std::string wire_traits<ores::assets::messaging::image_event>::write_json(
    const ores::assets::messaging::image_event& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_event
wire_traits<ores::assets::messaging::image_event>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_event>(text);
}

std::string wire_traits<ores::assets::messaging::image_event>::write_msgpack(
    const ores::assets::messaging::image_event& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_event
wire_traits<ores::assets::messaging::image_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_event>(data);
}

std::string wire_traits<ores::assets::messaging::image_version_key>::write_json(
    const ores::assets::messaging::image_version_key& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_version_key
wire_traits<ores::assets::messaging::image_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_version_key>(text);
}

std::string wire_traits<ores::assets::messaging::image_version_key>::write_msgpack(
    const ores::assets::messaging::image_version_key& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_version_key
wire_traits<ores::assets::messaging::image_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_version_key>(data);
}

std::string wire_traits<ores::assets::messaging::image_versions_filter>::write_json(
    const ores::assets::messaging::image_versions_filter& v) {
    return object::write_json(v);
}

ores::assets::messaging::image_versions_filter
wire_traits<ores::assets::messaging::image_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::image_versions_filter>(text);
}

std::string wire_traits<ores::assets::messaging::image_versions_filter>::write_msgpack(
    const ores::assets::messaging::image_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::image_versions_filter
wire_traits<ores::assets::messaging::image_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::image_versions_filter>(data);
}

std::string wire_traits<ores::assets::messaging::list_images_request>::write_json(
    const ores::assets::messaging::list_images_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_images_request
wire_traits<ores::assets::messaging::list_images_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::list_images_request>(text);
}

std::string wire_traits<ores::assets::messaging::list_images_request>::write_msgpack(
    const ores::assets::messaging::list_images_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_images_request
wire_traits<ores::assets::messaging::list_images_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_images_request>(data);
}

std::string wire_traits<ores::assets::messaging::list_images_response>::write_json(
    const ores::assets::messaging::list_images_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_images_response
wire_traits<ores::assets::messaging::list_images_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::list_images_response>(text);
}

std::string wire_traits<ores::assets::messaging::list_images_response>::write_msgpack(
    const ores::assets::messaging::list_images_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_images_response
wire_traits<ores::assets::messaging::list_images_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_images_response>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_request>::write_json(
    const ores::assets::messaging::get_image_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_request
wire_traits<ores::assets::messaging::get_image_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_request>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_request>::write_msgpack(
    const ores::assets::messaging::get_image_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_request
wire_traits<ores::assets::messaging::get_image_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_request>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_response>::write_json(
    const ores::assets::messaging::get_image_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_response
wire_traits<ores::assets::messaging::get_image_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_response>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_response>::write_msgpack(
    const ores::assets::messaging::get_image_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_response
wire_traits<ores::assets::messaging::get_image_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_response>(data);
}

std::string wire_traits<ores::assets::messaging::get_many_images_request>::write_json(
    const ores::assets::messaging::get_many_images_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_many_images_request
wire_traits<ores::assets::messaging::get_many_images_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_many_images_request>(text);
}

std::string wire_traits<ores::assets::messaging::get_many_images_request>::write_msgpack(
    const ores::assets::messaging::get_many_images_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_many_images_request
wire_traits<ores::assets::messaging::get_many_images_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_many_images_request>(data);
}

std::string wire_traits<ores::assets::messaging::get_many_images_response>::write_json(
    const ores::assets::messaging::get_many_images_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_many_images_response
wire_traits<ores::assets::messaging::get_many_images_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_many_images_response>(text);
}

std::string wire_traits<ores::assets::messaging::get_many_images_response>::write_msgpack(
    const ores::assets::messaging::get_many_images_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_many_images_response
wire_traits<ores::assets::messaging::get_many_images_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_many_images_response>(data);
}

std::string wire_traits<ores::assets::messaging::put_image_request>::write_json(
    const ores::assets::messaging::put_image_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_image_request
wire_traits<ores::assets::messaging::put_image_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_image_request>(text);
}

std::string wire_traits<ores::assets::messaging::put_image_request>::write_msgpack(
    const ores::assets::messaging::put_image_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_image_request
wire_traits<ores::assets::messaging::put_image_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_image_request>(data);
}

std::string wire_traits<ores::assets::messaging::put_image_response>::write_json(
    const ores::assets::messaging::put_image_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_image_response
wire_traits<ores::assets::messaging::put_image_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_image_response>(text);
}

std::string wire_traits<ores::assets::messaging::put_image_response>::write_msgpack(
    const ores::assets::messaging::put_image_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_image_response
wire_traits<ores::assets::messaging::put_image_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_image_response>(data);
}

std::string wire_traits<ores::assets::messaging::put_many_images_request>::write_json(
    const ores::assets::messaging::put_many_images_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_many_images_request
wire_traits<ores::assets::messaging::put_many_images_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_many_images_request>(text);
}

std::string wire_traits<ores::assets::messaging::put_many_images_request>::write_msgpack(
    const ores::assets::messaging::put_many_images_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_many_images_request
wire_traits<ores::assets::messaging::put_many_images_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_many_images_request>(data);
}

std::string wire_traits<ores::assets::messaging::put_many_images_response>::write_json(
    const ores::assets::messaging::put_many_images_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::put_many_images_response
wire_traits<ores::assets::messaging::put_many_images_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::put_many_images_response>(text);
}

std::string wire_traits<ores::assets::messaging::put_many_images_response>::write_msgpack(
    const ores::assets::messaging::put_many_images_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::put_many_images_response
wire_traits<ores::assets::messaging::put_many_images_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::put_many_images_response>(data);
}

std::string wire_traits<ores::assets::messaging::delete_image_request>::write_json(
    const ores::assets::messaging::delete_image_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_image_request
wire_traits<ores::assets::messaging::delete_image_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_image_request>(text);
}

std::string wire_traits<ores::assets::messaging::delete_image_request>::write_msgpack(
    const ores::assets::messaging::delete_image_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_image_request
wire_traits<ores::assets::messaging::delete_image_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_image_request>(data);
}

std::string wire_traits<ores::assets::messaging::delete_image_response>::write_json(
    const ores::assets::messaging::delete_image_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_image_response
wire_traits<ores::assets::messaging::delete_image_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_image_response>(text);
}

std::string wire_traits<ores::assets::messaging::delete_image_response>::write_msgpack(
    const ores::assets::messaging::delete_image_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_image_response
wire_traits<ores::assets::messaging::delete_image_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_image_response>(data);
}

std::string wire_traits<ores::assets::messaging::delete_many_images_request>::write_json(
    const ores::assets::messaging::delete_many_images_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_many_images_request
wire_traits<ores::assets::messaging::delete_many_images_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_many_images_request>(text);
}

std::string wire_traits<ores::assets::messaging::delete_many_images_request>::write_msgpack(
    const ores::assets::messaging::delete_many_images_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_many_images_request
wire_traits<ores::assets::messaging::delete_many_images_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_many_images_request>(data);
}

std::string wire_traits<ores::assets::messaging::delete_many_images_response>::write_json(
    const ores::assets::messaging::delete_many_images_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::delete_many_images_response
wire_traits<ores::assets::messaging::delete_many_images_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::delete_many_images_response>(text);
}

std::string wire_traits<ores::assets::messaging::delete_many_images_response>::write_msgpack(
    const ores::assets::messaging::delete_many_images_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::delete_many_images_response
wire_traits<ores::assets::messaging::delete_many_images_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::delete_many_images_response>(data);
}

std::string wire_traits<ores::assets::messaging::list_image_versions_request>::write_json(
    const ores::assets::messaging::list_image_versions_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_image_versions_request
wire_traits<ores::assets::messaging::list_image_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::list_image_versions_request>(text);
}

std::string wire_traits<ores::assets::messaging::list_image_versions_request>::write_msgpack(
    const ores::assets::messaging::list_image_versions_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_image_versions_request
wire_traits<ores::assets::messaging::list_image_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_image_versions_request>(data);
}

std::string wire_traits<ores::assets::messaging::list_image_versions_response>::write_json(
    const ores::assets::messaging::list_image_versions_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::list_image_versions_response
wire_traits<ores::assets::messaging::list_image_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::assets::messaging::list_image_versions_response>(text);
}

std::string wire_traits<ores::assets::messaging::list_image_versions_response>::write_msgpack(
    const ores::assets::messaging::list_image_versions_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::list_image_versions_response
wire_traits<ores::assets::messaging::list_image_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::list_image_versions_response>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_version_request>::write_json(
    const ores::assets::messaging::get_image_version_request& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_version_request
wire_traits<ores::assets::messaging::get_image_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_version_request>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_version_request>::write_msgpack(
    const ores::assets::messaging::get_image_version_request& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_version_request
wire_traits<ores::assets::messaging::get_image_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_version_request>(data);
}

std::string wire_traits<ores::assets::messaging::get_image_version_response>::write_json(
    const ores::assets::messaging::get_image_version_response& v) {
    return object::write_json(v);
}

ores::assets::messaging::get_image_version_response
wire_traits<ores::assets::messaging::get_image_version_response>::read_json(std::string_view text) {
    return object::read_json<ores::assets::messaging::get_image_version_response>(text);
}

std::string wire_traits<ores::assets::messaging::get_image_version_response>::write_msgpack(
    const ores::assets::messaging::get_image_version_response& v) {
    return object::write_msgpack(v);
}

ores::assets::messaging::get_image_version_response
wire_traits<ores::assets::messaging::get_image_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::assets::messaging::get_image_version_response>(data);
}

}
