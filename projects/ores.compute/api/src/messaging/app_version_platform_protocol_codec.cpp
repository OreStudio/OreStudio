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
#include "ores.compute.api/domain/app_version_platform_codec.hpp"
#include "ores.compute.api/messaging/app_version_platform_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::compute::messaging::app_version_platform_key>::write_json(
    const ores::compute::messaging::app_version_platform_key& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platform_key
wire_traits<ores::compute::messaging::app_version_platform_key>::read_json(std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platform_key>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_key>::write_msgpack(
    const ores::compute::messaging::app_version_platform_key& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platform_key
wire_traits<ores::compute::messaging::app_version_platform_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platform_key>(data);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_write>::write_json(
    const ores::compute::messaging::app_version_platform_write& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platform_write
wire_traits<ores::compute::messaging::app_version_platform_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platform_write>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_write>::write_msgpack(
    const ores::compute::messaging::app_version_platform_write& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platform_write
wire_traits<ores::compute::messaging::app_version_platform_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platform_write>(data);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_change>::write_json(
    const ores::compute::messaging::app_version_platform_change& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platform_change
wire_traits<ores::compute::messaging::app_version_platform_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platform_change>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_change>::write_msgpack(
    const ores::compute::messaging::app_version_platform_change& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platform_change
wire_traits<ores::compute::messaging::app_version_platform_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platform_change>(data);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_removal>::write_json(
    const ores::compute::messaging::app_version_platform_removal& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platform_removal
wire_traits<ores::compute::messaging::app_version_platform_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platform_removal>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_removal>::write_msgpack(
    const ores::compute::messaging::app_version_platform_removal& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platform_removal
wire_traits<ores::compute::messaging::app_version_platform_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platform_removal>(data);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_lookup>::write_json(
    const ores::compute::messaging::app_version_platform_lookup& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platform_lookup
wire_traits<ores::compute::messaging::app_version_platform_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platform_lookup>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platform_lookup>::write_msgpack(
    const ores::compute::messaging::app_version_platform_lookup& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platform_lookup
wire_traits<ores::compute::messaging::app_version_platform_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platform_lookup>(data);
}

std::string wire_traits<ores::compute::messaging::app_version_platforms_filter>::write_json(
    const ores::compute::messaging::app_version_platforms_filter& v) {
    return object::write_json(v);
}

ores::compute::messaging::app_version_platforms_filter
wire_traits<ores::compute::messaging::app_version_platforms_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::app_version_platforms_filter>(text);
}

std::string wire_traits<ores::compute::messaging::app_version_platforms_filter>::write_msgpack(
    const ores::compute::messaging::app_version_platforms_filter& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::app_version_platforms_filter
wire_traits<ores::compute::messaging::app_version_platforms_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::app_version_platforms_filter>(data);
}

std::string wire_traits<ores::compute::messaging::list_app_version_platforms_request>::write_json(
    const ores::compute::messaging::list_app_version_platforms_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_app_version_platforms_request
wire_traits<ores::compute::messaging::list_app_version_platforms_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::list_app_version_platforms_request>(text);
}

std::string
wire_traits<ores::compute::messaging::list_app_version_platforms_request>::write_msgpack(
    const ores::compute::messaging::list_app_version_platforms_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_app_version_platforms_request
wire_traits<ores::compute::messaging::list_app_version_platforms_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_app_version_platforms_request>(data);
}

std::string wire_traits<ores::compute::messaging::list_app_version_platforms_response>::write_json(
    const ores::compute::messaging::list_app_version_platforms_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_app_version_platforms_response
wire_traits<ores::compute::messaging::list_app_version_platforms_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::list_app_version_platforms_response>(text);
}

std::string
wire_traits<ores::compute::messaging::list_app_version_platforms_response>::write_msgpack(
    const ores::compute::messaging::list_app_version_platforms_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_app_version_platforms_response
wire_traits<ores::compute::messaging::list_app_version_platforms_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::list_app_version_platforms_response>(
        data);
}

std::string wire_traits<ores::compute::messaging::get_app_version_platform_request>::write_json(
    const ores::compute::messaging::get_app_version_platform_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_app_version_platform_request
wire_traits<ores::compute::messaging::get_app_version_platform_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::get_app_version_platform_request>(text);
}

std::string wire_traits<ores::compute::messaging::get_app_version_platform_request>::write_msgpack(
    const ores::compute::messaging::get_app_version_platform_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_app_version_platform_request
wire_traits<ores::compute::messaging::get_app_version_platform_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_app_version_platform_request>(data);
}

std::string wire_traits<ores::compute::messaging::get_app_version_platform_response>::write_json(
    const ores::compute::messaging::get_app_version_platform_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_app_version_platform_response
wire_traits<ores::compute::messaging::get_app_version_platform_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::get_app_version_platform_response>(text);
}

std::string wire_traits<ores::compute::messaging::get_app_version_platform_response>::write_msgpack(
    const ores::compute::messaging::get_app_version_platform_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_app_version_platform_response
wire_traits<ores::compute::messaging::get_app_version_platform_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_app_version_platform_response>(data);
}

std::string
wire_traits<ores::compute::messaging::get_many_app_version_platforms_request>::write_json(
    const ores::compute::messaging::get_many_app_version_platforms_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_many_app_version_platforms_request
wire_traits<ores::compute::messaging::get_many_app_version_platforms_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::get_many_app_version_platforms_request>(
        text);
}

std::string
wire_traits<ores::compute::messaging::get_many_app_version_platforms_request>::write_msgpack(
    const ores::compute::messaging::get_many_app_version_platforms_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_many_app_version_platforms_request
wire_traits<ores::compute::messaging::get_many_app_version_platforms_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_many_app_version_platforms_request>(
        data);
}

std::string
wire_traits<ores::compute::messaging::get_many_app_version_platforms_response>::write_json(
    const ores::compute::messaging::get_many_app_version_platforms_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::get_many_app_version_platforms_response
wire_traits<ores::compute::messaging::get_many_app_version_platforms_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::get_many_app_version_platforms_response>(
        text);
}

std::string
wire_traits<ores::compute::messaging::get_many_app_version_platforms_response>::write_msgpack(
    const ores::compute::messaging::get_many_app_version_platforms_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::get_many_app_version_platforms_response
wire_traits<ores::compute::messaging::get_many_app_version_platforms_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::get_many_app_version_platforms_response>(
        data);
}

std::string wire_traits<ores::compute::messaging::put_app_version_platform_request>::write_json(
    const ores::compute::messaging::put_app_version_platform_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_app_version_platform_request
wire_traits<ores::compute::messaging::put_app_version_platform_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::put_app_version_platform_request>(text);
}

std::string wire_traits<ores::compute::messaging::put_app_version_platform_request>::write_msgpack(
    const ores::compute::messaging::put_app_version_platform_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_app_version_platform_request
wire_traits<ores::compute::messaging::put_app_version_platform_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_app_version_platform_request>(data);
}

std::string wire_traits<ores::compute::messaging::put_app_version_platform_response>::write_json(
    const ores::compute::messaging::put_app_version_platform_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_app_version_platform_response
wire_traits<ores::compute::messaging::put_app_version_platform_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::put_app_version_platform_response>(text);
}

std::string wire_traits<ores::compute::messaging::put_app_version_platform_response>::write_msgpack(
    const ores::compute::messaging::put_app_version_platform_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_app_version_platform_response
wire_traits<ores::compute::messaging::put_app_version_platform_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_app_version_platform_response>(data);
}

std::string
wire_traits<ores::compute::messaging::put_many_app_version_platforms_request>::write_json(
    const ores::compute::messaging::put_many_app_version_platforms_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_many_app_version_platforms_request
wire_traits<ores::compute::messaging::put_many_app_version_platforms_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::put_many_app_version_platforms_request>(
        text);
}

std::string
wire_traits<ores::compute::messaging::put_many_app_version_platforms_request>::write_msgpack(
    const ores::compute::messaging::put_many_app_version_platforms_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_many_app_version_platforms_request
wire_traits<ores::compute::messaging::put_many_app_version_platforms_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_many_app_version_platforms_request>(
        data);
}

std::string
wire_traits<ores::compute::messaging::put_many_app_version_platforms_response>::write_json(
    const ores::compute::messaging::put_many_app_version_platforms_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::put_many_app_version_platforms_response
wire_traits<ores::compute::messaging::put_many_app_version_platforms_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::put_many_app_version_platforms_response>(
        text);
}

std::string
wire_traits<ores::compute::messaging::put_many_app_version_platforms_response>::write_msgpack(
    const ores::compute::messaging::put_many_app_version_platforms_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::put_many_app_version_platforms_response
wire_traits<ores::compute::messaging::put_many_app_version_platforms_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::put_many_app_version_platforms_response>(
        data);
}

std::string wire_traits<ores::compute::messaging::delete_app_version_platform_request>::write_json(
    const ores::compute::messaging::delete_app_version_platform_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_app_version_platform_request
wire_traits<ores::compute::messaging::delete_app_version_platform_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_app_version_platform_request>(text);
}

std::string
wire_traits<ores::compute::messaging::delete_app_version_platform_request>::write_msgpack(
    const ores::compute::messaging::delete_app_version_platform_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_app_version_platform_request
wire_traits<ores::compute::messaging::delete_app_version_platform_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_app_version_platform_request>(
        data);
}

std::string wire_traits<ores::compute::messaging::delete_app_version_platform_response>::write_json(
    const ores::compute::messaging::delete_app_version_platform_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_app_version_platform_response
wire_traits<ores::compute::messaging::delete_app_version_platform_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_app_version_platform_response>(text);
}

std::string
wire_traits<ores::compute::messaging::delete_app_version_platform_response>::write_msgpack(
    const ores::compute::messaging::delete_app_version_platform_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_app_version_platform_response
wire_traits<ores::compute::messaging::delete_app_version_platform_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::compute::messaging::delete_app_version_platform_response>(
        data);
}

std::string
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_request>::write_json(
    const ores::compute::messaging::delete_many_app_version_platforms_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_many_app_version_platforms_request
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_many_app_version_platforms_request>(
        text);
}

std::string
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_request>::write_msgpack(
    const ores::compute::messaging::delete_many_app_version_platforms_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_many_app_version_platforms_request
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::compute::messaging::delete_many_app_version_platforms_request>(data);
}

std::string
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_response>::write_json(
    const ores::compute::messaging::delete_many_app_version_platforms_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::delete_many_app_version_platforms_response
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::compute::messaging::delete_many_app_version_platforms_response>(
        text);
}

std::string
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_response>::write_msgpack(
    const ores::compute::messaging::delete_many_app_version_platforms_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::delete_many_app_version_platforms_response
wire_traits<ores::compute::messaging::delete_many_app_version_platforms_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::compute::messaging::delete_many_app_version_platforms_response>(data);
}

std::string
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>::
    write_json(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_request& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_by_app_version_id_app_version_platforms_request
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>(text);
}

std::string
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>::
    write_msgpack(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_request& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_by_app_version_id_app_version_platforms_request
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_request>(data);
}

std::string
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>::
    write_json(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_response& v) {
    return object::write_json(v);
}

ores::compute::messaging::list_by_app_version_id_app_version_platforms_response
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>(text);
}

std::string
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>::
    write_msgpack(
        const ores::compute::messaging::list_by_app_version_id_app_version_platforms_response& v) {
    return object::write_msgpack(v);
}

ores::compute::messaging::list_by_app_version_id_app_version_platforms_response
wire_traits<ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::compute::messaging::list_by_app_version_id_app_version_platforms_response>(data);
}

}
