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
#include "ores.dq.api/domain/methodology_codec.hpp"
#include "ores.dq.api/messaging/methodology_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::methodology_key>::write_json(
    const ores::dq::messaging::methodology_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_key
wire_traits<ores::dq::messaging::methodology_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_key>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_key>::write_msgpack(
    const ores::dq::messaging::methodology_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_key
wire_traits<ores::dq::messaging::methodology_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_key>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_write>::write_json(
    const ores::dq::messaging::methodology_write& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_write
wire_traits<ores::dq::messaging::methodology_write>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_write>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_write>::write_msgpack(
    const ores::dq::messaging::methodology_write& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_write
wire_traits<ores::dq::messaging::methodology_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_write>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_change>::write_json(
    const ores::dq::messaging::methodology_change& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_change
wire_traits<ores::dq::messaging::methodology_change>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_change>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_change>::write_msgpack(
    const ores::dq::messaging::methodology_change& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_change
wire_traits<ores::dq::messaging::methodology_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_change>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_removal>::write_json(
    const ores::dq::messaging::methodology_removal& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_removal
wire_traits<ores::dq::messaging::methodology_removal>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_removal>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_removal>::write_msgpack(
    const ores::dq::messaging::methodology_removal& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_removal
wire_traits<ores::dq::messaging::methodology_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_removal>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_lookup>::write_json(
    const ores::dq::messaging::methodology_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_lookup
wire_traits<ores::dq::messaging::methodology_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_lookup>::write_msgpack(
    const ores::dq::messaging::methodology_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_lookup
wire_traits<ores::dq::messaging::methodology_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::methodologies_filter>::write_json(
    const ores::dq::messaging::methodologies_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodologies_filter
wire_traits<ores::dq::messaging::methodologies_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodologies_filter>(text);
}

std::string wire_traits<ores::dq::messaging::methodologies_filter>::write_msgpack(
    const ores::dq::messaging::methodologies_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodologies_filter
wire_traits<ores::dq::messaging::methodologies_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodologies_filter>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_event>::write_json(
    const ores::dq::messaging::methodology_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_event
wire_traits<ores::dq::messaging::methodology_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_event>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_event>::write_msgpack(
    const ores::dq::messaging::methodology_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_event
wire_traits<ores::dq::messaging::methodology_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_event>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_version_key>::write_json(
    const ores::dq::messaging::methodology_version_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_version_key
wire_traits<ores::dq::messaging::methodology_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_version_key>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_version_key>::write_msgpack(
    const ores::dq::messaging::methodology_version_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_version_key
wire_traits<ores::dq::messaging::methodology_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_version_key>(data);
}

std::string wire_traits<ores::dq::messaging::methodology_versions_filter>::write_json(
    const ores::dq::messaging::methodology_versions_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::methodology_versions_filter
wire_traits<ores::dq::messaging::methodology_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::methodology_versions_filter>(text);
}

std::string wire_traits<ores::dq::messaging::methodology_versions_filter>::write_msgpack(
    const ores::dq::messaging::methodology_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::methodology_versions_filter
wire_traits<ores::dq::messaging::methodology_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::methodology_versions_filter>(data);
}

std::string wire_traits<ores::dq::messaging::list_methodologies_request>::write_json(
    const ores::dq::messaging::list_methodologies_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_methodologies_request
wire_traits<ores::dq::messaging::list_methodologies_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_methodologies_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_methodologies_request>::write_msgpack(
    const ores::dq::messaging::list_methodologies_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_methodologies_request
wire_traits<ores::dq::messaging::list_methodologies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_methodologies_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_methodologies_response>::write_json(
    const ores::dq::messaging::list_methodologies_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_methodologies_response
wire_traits<ores::dq::messaging::list_methodologies_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_methodologies_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_methodologies_response>::write_msgpack(
    const ores::dq::messaging::list_methodologies_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_methodologies_response
wire_traits<ores::dq::messaging::list_methodologies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_methodologies_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_methodology_request>::write_json(
    const ores::dq::messaging::get_methodology_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_methodology_request
wire_traits<ores::dq::messaging::get_methodology_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_methodology_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_methodology_request>::write_msgpack(
    const ores::dq::messaging::get_methodology_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_methodology_request
wire_traits<ores::dq::messaging::get_methodology_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_methodology_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_methodology_response>::write_json(
    const ores::dq::messaging::get_methodology_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_methodology_response
wire_traits<ores::dq::messaging::get_methodology_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_methodology_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_methodology_response>::write_msgpack(
    const ores::dq::messaging::get_methodology_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_methodology_response
wire_traits<ores::dq::messaging::get_methodology_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_methodology_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_methodologies_request>::write_json(
    const ores::dq::messaging::get_many_methodologies_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_methodologies_request
wire_traits<ores::dq::messaging::get_many_methodologies_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_methodologies_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_methodologies_request>::write_msgpack(
    const ores::dq::messaging::get_many_methodologies_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_methodologies_request
wire_traits<ores::dq::messaging::get_many_methodologies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_methodologies_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_methodologies_response>::write_json(
    const ores::dq::messaging::get_many_methodologies_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_methodologies_response
wire_traits<ores::dq::messaging::get_many_methodologies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_methodologies_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_methodologies_response>::write_msgpack(
    const ores::dq::messaging::get_many_methodologies_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_methodologies_response
wire_traits<ores::dq::messaging::get_many_methodologies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_methodologies_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_methodology_request>::write_json(
    const ores::dq::messaging::put_methodology_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_methodology_request
wire_traits<ores::dq::messaging::put_methodology_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_methodology_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_methodology_request>::write_msgpack(
    const ores::dq::messaging::put_methodology_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_methodology_request
wire_traits<ores::dq::messaging::put_methodology_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_methodology_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_methodology_response>::write_json(
    const ores::dq::messaging::put_methodology_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_methodology_response
wire_traits<ores::dq::messaging::put_methodology_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_methodology_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_methodology_response>::write_msgpack(
    const ores::dq::messaging::put_methodology_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_methodology_response
wire_traits<ores::dq::messaging::put_methodology_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_methodology_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_methodologies_request>::write_json(
    const ores::dq::messaging::put_many_methodologies_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_methodologies_request
wire_traits<ores::dq::messaging::put_many_methodologies_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_methodologies_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_methodologies_request>::write_msgpack(
    const ores::dq::messaging::put_many_methodologies_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_methodologies_request
wire_traits<ores::dq::messaging::put_many_methodologies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_methodologies_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_methodologies_response>::write_json(
    const ores::dq::messaging::put_many_methodologies_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_methodologies_response
wire_traits<ores::dq::messaging::put_many_methodologies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_methodologies_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_methodologies_response>::write_msgpack(
    const ores::dq::messaging::put_many_methodologies_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_methodologies_response
wire_traits<ores::dq::messaging::put_many_methodologies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_methodologies_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_methodology_request>::write_json(
    const ores::dq::messaging::delete_methodology_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_methodology_request
wire_traits<ores::dq::messaging::delete_methodology_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_methodology_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_methodology_request>::write_msgpack(
    const ores::dq::messaging::delete_methodology_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_methodology_request
wire_traits<ores::dq::messaging::delete_methodology_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_methodology_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_methodology_response>::write_json(
    const ores::dq::messaging::delete_methodology_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_methodology_response
wire_traits<ores::dq::messaging::delete_methodology_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_methodology_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_methodology_response>::write_msgpack(
    const ores::dq::messaging::delete_methodology_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_methodology_response
wire_traits<ores::dq::messaging::delete_methodology_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_methodology_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_methodologies_request>::write_json(
    const ores::dq::messaging::delete_many_methodologies_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_methodologies_request
wire_traits<ores::dq::messaging::delete_many_methodologies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_methodologies_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_methodologies_request>::write_msgpack(
    const ores::dq::messaging::delete_many_methodologies_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_methodologies_request
wire_traits<ores::dq::messaging::delete_many_methodologies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_methodologies_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_methodologies_response>::write_json(
    const ores::dq::messaging::delete_many_methodologies_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_methodologies_response
wire_traits<ores::dq::messaging::delete_many_methodologies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_methodologies_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_methodologies_response>::write_msgpack(
    const ores::dq::messaging::delete_many_methodologies_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_methodologies_response
wire_traits<ores::dq::messaging::delete_many_methodologies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_methodologies_response>(data);
}

std::string wire_traits<ores::dq::messaging::list_methodology_versions_request>::write_json(
    const ores::dq::messaging::list_methodology_versions_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_methodology_versions_request
wire_traits<ores::dq::messaging::list_methodology_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_methodology_versions_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_methodology_versions_request>::write_msgpack(
    const ores::dq::messaging::list_methodology_versions_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_methodology_versions_request
wire_traits<ores::dq::messaging::list_methodology_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_methodology_versions_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_methodology_versions_response>::write_json(
    const ores::dq::messaging::list_methodology_versions_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_methodology_versions_response
wire_traits<ores::dq::messaging::list_methodology_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_methodology_versions_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_methodology_versions_response>::write_msgpack(
    const ores::dq::messaging::list_methodology_versions_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_methodology_versions_response
wire_traits<ores::dq::messaging::list_methodology_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_methodology_versions_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_methodology_version_request>::write_json(
    const ores::dq::messaging::get_methodology_version_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_methodology_version_request
wire_traits<ores::dq::messaging::get_methodology_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_methodology_version_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_methodology_version_request>::write_msgpack(
    const ores::dq::messaging::get_methodology_version_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_methodology_version_request
wire_traits<ores::dq::messaging::get_methodology_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_methodology_version_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_methodology_version_response>::write_json(
    const ores::dq::messaging::get_methodology_version_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_methodology_version_response
wire_traits<ores::dq::messaging::get_methodology_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_methodology_version_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_methodology_version_response>::write_msgpack(
    const ores::dq::messaging::get_methodology_version_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_methodology_version_response
wire_traits<ores::dq::messaging::get_methodology_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_methodology_version_response>(data);
}

}
