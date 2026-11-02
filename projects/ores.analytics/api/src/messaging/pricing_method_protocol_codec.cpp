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
#include "ores.analytics.api/domain/pricing_method_codec.hpp"
#include "ores.analytics.api/messaging/pricing_method_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::analytics::messaging::pricing_method_key>::write_json(
    const ores::analytics::messaging::pricing_method_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_key
wire_traits<ores::analytics::messaging::pricing_method_key>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_key>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_key>::write_msgpack(
    const ores::analytics::messaging::pricing_method_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_key
wire_traits<ores::analytics::messaging::pricing_method_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_key>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_write>::write_json(
    const ores::analytics::messaging::pricing_method_write& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_write
wire_traits<ores::analytics::messaging::pricing_method_write>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_write>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_write>::write_msgpack(
    const ores::analytics::messaging::pricing_method_write& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_write
wire_traits<ores::analytics::messaging::pricing_method_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_write>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_change>::write_json(
    const ores::analytics::messaging::pricing_method_change& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_change
wire_traits<ores::analytics::messaging::pricing_method_change>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_change>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_change>::write_msgpack(
    const ores::analytics::messaging::pricing_method_change& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_change
wire_traits<ores::analytics::messaging::pricing_method_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_change>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_removal>::write_json(
    const ores::analytics::messaging::pricing_method_removal& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_removal
wire_traits<ores::analytics::messaging::pricing_method_removal>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_removal>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_removal>::write_msgpack(
    const ores::analytics::messaging::pricing_method_removal& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_removal
wire_traits<ores::analytics::messaging::pricing_method_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_removal>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_lookup>::write_json(
    const ores::analytics::messaging::pricing_method_lookup& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_lookup
wire_traits<ores::analytics::messaging::pricing_method_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_lookup>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_lookup>::write_msgpack(
    const ores::analytics::messaging::pricing_method_lookup& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_lookup
wire_traits<ores::analytics::messaging::pricing_method_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_lookup>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_methods_filter>::write_json(
    const ores::analytics::messaging::pricing_methods_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_methods_filter
wire_traits<ores::analytics::messaging::pricing_methods_filter>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_methods_filter>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_methods_filter>::write_msgpack(
    const ores::analytics::messaging::pricing_methods_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_methods_filter
wire_traits<ores::analytics::messaging::pricing_methods_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_methods_filter>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_event>::write_json(
    const ores::analytics::messaging::pricing_method_event& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_event
wire_traits<ores::analytics::messaging::pricing_method_event>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_event>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_event>::write_msgpack(
    const ores::analytics::messaging::pricing_method_event& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_event
wire_traits<ores::analytics::messaging::pricing_method_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_event>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_version_key>::write_json(
    const ores::analytics::messaging::pricing_method_version_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_version_key
wire_traits<ores::analytics::messaging::pricing_method_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_version_key>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_version_key>::write_msgpack(
    const ores::analytics::messaging::pricing_method_version_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_version_key
wire_traits<ores::analytics::messaging::pricing_method_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_version_key>(data);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_versions_filter>::write_json(
    const ores::analytics::messaging::pricing_method_versions_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::pricing_method_versions_filter
wire_traits<ores::analytics::messaging::pricing_method_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::pricing_method_versions_filter>(text);
}

std::string wire_traits<ores::analytics::messaging::pricing_method_versions_filter>::write_msgpack(
    const ores::analytics::messaging::pricing_method_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::pricing_method_versions_filter
wire_traits<ores::analytics::messaging::pricing_method_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::pricing_method_versions_filter>(data);
}

std::string wire_traits<ores::analytics::messaging::list_pricing_methods_request>::write_json(
    const ores::analytics::messaging::list_pricing_methods_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_pricing_methods_request
wire_traits<ores::analytics::messaging::list_pricing_methods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_pricing_methods_request>(text);
}

std::string wire_traits<ores::analytics::messaging::list_pricing_methods_request>::write_msgpack(
    const ores::analytics::messaging::list_pricing_methods_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_pricing_methods_request
wire_traits<ores::analytics::messaging::list_pricing_methods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_pricing_methods_request>(data);
}

std::string wire_traits<ores::analytics::messaging::list_pricing_methods_response>::write_json(
    const ores::analytics::messaging::list_pricing_methods_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_pricing_methods_response
wire_traits<ores::analytics::messaging::list_pricing_methods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_pricing_methods_response>(text);
}

std::string wire_traits<ores::analytics::messaging::list_pricing_methods_response>::write_msgpack(
    const ores::analytics::messaging::list_pricing_methods_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_pricing_methods_response
wire_traits<ores::analytics::messaging::list_pricing_methods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_pricing_methods_response>(data);
}

std::string wire_traits<ores::analytics::messaging::get_pricing_method_request>::write_json(
    const ores::analytics::messaging::get_pricing_method_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_method_request
wire_traits<ores::analytics::messaging::get_pricing_method_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_method_request>(text);
}

std::string wire_traits<ores::analytics::messaging::get_pricing_method_request>::write_msgpack(
    const ores::analytics::messaging::get_pricing_method_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_method_request
wire_traits<ores::analytics::messaging::get_pricing_method_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_method_request>(data);
}

std::string wire_traits<ores::analytics::messaging::get_pricing_method_response>::write_json(
    const ores::analytics::messaging::get_pricing_method_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_method_response
wire_traits<ores::analytics::messaging::get_pricing_method_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_method_response>(text);
}

std::string wire_traits<ores::analytics::messaging::get_pricing_method_response>::write_msgpack(
    const ores::analytics::messaging::get_pricing_method_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_method_response
wire_traits<ores::analytics::messaging::get_pricing_method_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_method_response>(data);
}

std::string wire_traits<ores::analytics::messaging::get_many_pricing_methods_request>::write_json(
    const ores::analytics::messaging::get_many_pricing_methods_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_pricing_methods_request
wire_traits<ores::analytics::messaging::get_many_pricing_methods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_many_pricing_methods_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_pricing_methods_request>::write_msgpack(
    const ores::analytics::messaging::get_many_pricing_methods_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_pricing_methods_request
wire_traits<ores::analytics::messaging::get_many_pricing_methods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_many_pricing_methods_request>(data);
}

std::string wire_traits<ores::analytics::messaging::get_many_pricing_methods_response>::write_json(
    const ores::analytics::messaging::get_many_pricing_methods_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_pricing_methods_response
wire_traits<ores::analytics::messaging::get_many_pricing_methods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_many_pricing_methods_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_pricing_methods_response>::write_msgpack(
    const ores::analytics::messaging::get_many_pricing_methods_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_pricing_methods_response
wire_traits<ores::analytics::messaging::get_many_pricing_methods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_many_pricing_methods_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::put_pricing_method_request>::write_json(
    const ores::analytics::messaging::put_pricing_method_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_pricing_method_request
wire_traits<ores::analytics::messaging::put_pricing_method_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_pricing_method_request>(text);
}

std::string wire_traits<ores::analytics::messaging::put_pricing_method_request>::write_msgpack(
    const ores::analytics::messaging::put_pricing_method_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_pricing_method_request
wire_traits<ores::analytics::messaging::put_pricing_method_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_pricing_method_request>(data);
}

std::string wire_traits<ores::analytics::messaging::put_pricing_method_response>::write_json(
    const ores::analytics::messaging::put_pricing_method_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_pricing_method_response
wire_traits<ores::analytics::messaging::put_pricing_method_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_pricing_method_response>(text);
}

std::string wire_traits<ores::analytics::messaging::put_pricing_method_response>::write_msgpack(
    const ores::analytics::messaging::put_pricing_method_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_pricing_method_response
wire_traits<ores::analytics::messaging::put_pricing_method_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_pricing_method_response>(data);
}

std::string wire_traits<ores::analytics::messaging::put_many_pricing_methods_request>::write_json(
    const ores::analytics::messaging::put_many_pricing_methods_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_pricing_methods_request
wire_traits<ores::analytics::messaging::put_many_pricing_methods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_many_pricing_methods_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_pricing_methods_request>::write_msgpack(
    const ores::analytics::messaging::put_many_pricing_methods_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_pricing_methods_request
wire_traits<ores::analytics::messaging::put_many_pricing_methods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_many_pricing_methods_request>(data);
}

std::string wire_traits<ores::analytics::messaging::put_many_pricing_methods_response>::write_json(
    const ores::analytics::messaging::put_many_pricing_methods_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_pricing_methods_response
wire_traits<ores::analytics::messaging::put_many_pricing_methods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_many_pricing_methods_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_pricing_methods_response>::write_msgpack(
    const ores::analytics::messaging::put_many_pricing_methods_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_pricing_methods_response
wire_traits<ores::analytics::messaging::put_many_pricing_methods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_many_pricing_methods_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::delete_pricing_method_request>::write_json(
    const ores::analytics::messaging::delete_pricing_method_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_pricing_method_request
wire_traits<ores::analytics::messaging::delete_pricing_method_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_pricing_method_request>(text);
}

std::string wire_traits<ores::analytics::messaging::delete_pricing_method_request>::write_msgpack(
    const ores::analytics::messaging::delete_pricing_method_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_pricing_method_request
wire_traits<ores::analytics::messaging::delete_pricing_method_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_pricing_method_request>(data);
}

std::string wire_traits<ores::analytics::messaging::delete_pricing_method_response>::write_json(
    const ores::analytics::messaging::delete_pricing_method_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_pricing_method_response
wire_traits<ores::analytics::messaging::delete_pricing_method_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_pricing_method_response>(text);
}

std::string wire_traits<ores::analytics::messaging::delete_pricing_method_response>::write_msgpack(
    const ores::analytics::messaging::delete_pricing_method_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_pricing_method_response
wire_traits<ores::analytics::messaging::delete_pricing_method_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_pricing_method_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_request>::write_json(
    const ores::analytics::messaging::delete_many_pricing_methods_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_pricing_methods_request
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_many_pricing_methods_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_request>::write_msgpack(
    const ores::analytics::messaging::delete_many_pricing_methods_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_pricing_methods_request
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_many_pricing_methods_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_response>::write_json(
    const ores::analytics::messaging::delete_many_pricing_methods_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_pricing_methods_response
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_many_pricing_methods_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_response>::write_msgpack(
    const ores::analytics::messaging::delete_many_pricing_methods_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_pricing_methods_response
wire_traits<ores::analytics::messaging::delete_many_pricing_methods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_many_pricing_methods_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::list_pricing_method_versions_request>::write_json(
    const ores::analytics::messaging::list_pricing_method_versions_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_pricing_method_versions_request
wire_traits<ores::analytics::messaging::list_pricing_method_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_pricing_method_versions_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::list_pricing_method_versions_request>::write_msgpack(
    const ores::analytics::messaging::list_pricing_method_versions_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_pricing_method_versions_request
wire_traits<ores::analytics::messaging::list_pricing_method_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_pricing_method_versions_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::list_pricing_method_versions_response>::write_json(
    const ores::analytics::messaging::list_pricing_method_versions_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_pricing_method_versions_response
wire_traits<ores::analytics::messaging::list_pricing_method_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_pricing_method_versions_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::list_pricing_method_versions_response>::write_msgpack(
    const ores::analytics::messaging::list_pricing_method_versions_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_pricing_method_versions_response
wire_traits<ores::analytics::messaging::list_pricing_method_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_pricing_method_versions_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::get_pricing_method_version_request>::write_json(
    const ores::analytics::messaging::get_pricing_method_version_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_method_version_request
wire_traits<ores::analytics::messaging::get_pricing_method_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_method_version_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_method_version_request>::write_msgpack(
    const ores::analytics::messaging::get_pricing_method_version_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_method_version_request
wire_traits<ores::analytics::messaging::get_pricing_method_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_method_version_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_method_version_response>::write_json(
    const ores::analytics::messaging::get_pricing_method_version_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_pricing_method_version_response
wire_traits<ores::analytics::messaging::get_pricing_method_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_pricing_method_version_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_pricing_method_version_response>::write_msgpack(
    const ores::analytics::messaging::get_pricing_method_version_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_pricing_method_version_response
wire_traits<ores::analytics::messaging::get_pricing_method_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_pricing_method_version_response>(
        data);
}

}
