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
#include "ores.reporting.api/domain/cut_family_codec.hpp"
#include "ores.reporting.api/messaging/cut_family_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::cut_family_key>::write_json(
    const ores::reporting::messaging::cut_family_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_key
wire_traits<ores::reporting::messaging::cut_family_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_key>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_key>::write_msgpack(
    const ores::reporting::messaging::cut_family_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_key
wire_traits<ores::reporting::messaging::cut_family_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_key>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_write>::write_json(
    const ores::reporting::messaging::cut_family_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_write
wire_traits<ores::reporting::messaging::cut_family_write>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_write>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_write>::write_msgpack(
    const ores::reporting::messaging::cut_family_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_write
wire_traits<ores::reporting::messaging::cut_family_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_write>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_change>::write_json(
    const ores::reporting::messaging::cut_family_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_change
wire_traits<ores::reporting::messaging::cut_family_change>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_change>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_change>::write_msgpack(
    const ores::reporting::messaging::cut_family_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_change
wire_traits<ores::reporting::messaging::cut_family_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_change>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_removal>::write_json(
    const ores::reporting::messaging::cut_family_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_removal
wire_traits<ores::reporting::messaging::cut_family_removal>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_removal>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_removal>::write_msgpack(
    const ores::reporting::messaging::cut_family_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_removal
wire_traits<ores::reporting::messaging::cut_family_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_removal>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_lookup>::write_json(
    const ores::reporting::messaging::cut_family_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_lookup
wire_traits<ores::reporting::messaging::cut_family_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_lookup>::write_msgpack(
    const ores::reporting::messaging::cut_family_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_lookup
wire_traits<ores::reporting::messaging::cut_family_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_families_filter>::write_json(
    const ores::reporting::messaging::cut_families_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_families_filter
wire_traits<ores::reporting::messaging::cut_families_filter>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_families_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_families_filter>::write_msgpack(
    const ores::reporting::messaging::cut_families_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_families_filter
wire_traits<ores::reporting::messaging::cut_families_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_families_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_event>::write_json(
    const ores::reporting::messaging::cut_family_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_event
wire_traits<ores::reporting::messaging::cut_family_event>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_event>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_event>::write_msgpack(
    const ores::reporting::messaging::cut_family_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_event
wire_traits<ores::reporting::messaging::cut_family_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_event>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_version_key>::write_json(
    const ores::reporting::messaging::cut_family_version_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_version_key
wire_traits<ores::reporting::messaging::cut_family_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_version_key>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_version_key>::write_msgpack(
    const ores::reporting::messaging::cut_family_version_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_version_key
wire_traits<ores::reporting::messaging::cut_family_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_version_key>(data);
}

std::string wire_traits<ores::reporting::messaging::cut_family_versions_filter>::write_json(
    const ores::reporting::messaging::cut_family_versions_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::cut_family_versions_filter
wire_traits<ores::reporting::messaging::cut_family_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::cut_family_versions_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::cut_family_versions_filter>::write_msgpack(
    const ores::reporting::messaging::cut_family_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::cut_family_versions_filter
wire_traits<ores::reporting::messaging::cut_family_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::cut_family_versions_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_families_request>::write_json(
    const ores::reporting::messaging::list_cut_families_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_families_request
wire_traits<ores::reporting::messaging::list_cut_families_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_families_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_cut_families_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_families_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_families_request
wire_traits<ores::reporting::messaging::list_cut_families_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_families_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_families_response>::write_json(
    const ores::reporting::messaging::list_cut_families_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_families_response
wire_traits<ores::reporting::messaging::list_cut_families_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_families_response>(text);
}

std::string wire_traits<ores::reporting::messaging::list_cut_families_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_families_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_families_response
wire_traits<ores::reporting::messaging::list_cut_families_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_families_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_request>::write_json(
    const ores::reporting::messaging::get_cut_family_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_family_request
wire_traits<ores::reporting::messaging::get_cut_family_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_family_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_request>::write_msgpack(
    const ores::reporting::messaging::get_cut_family_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_family_request
wire_traits<ores::reporting::messaging::get_cut_family_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_family_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_response>::write_json(
    const ores::reporting::messaging::get_cut_family_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_family_response
wire_traits<ores::reporting::messaging::get_cut_family_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_family_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_response>::write_msgpack(
    const ores::reporting::messaging::get_cut_family_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_family_response
wire_traits<ores::reporting::messaging::get_cut_family_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_family_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_cut_families_request>::write_json(
    const ores::reporting::messaging::get_many_cut_families_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_cut_families_request
wire_traits<ores::reporting::messaging::get_many_cut_families_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_cut_families_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_many_cut_families_request>::write_msgpack(
    const ores::reporting::messaging::get_many_cut_families_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_cut_families_request
wire_traits<ores::reporting::messaging::get_many_cut_families_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_cut_families_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_many_cut_families_response>::write_json(
    const ores::reporting::messaging::get_many_cut_families_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_cut_families_response
wire_traits<ores::reporting::messaging::get_many_cut_families_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_cut_families_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_many_cut_families_response>::write_msgpack(
    const ores::reporting::messaging::get_many_cut_families_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_cut_families_response
wire_traits<ores::reporting::messaging::get_many_cut_families_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_cut_families_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_cut_family_request>::write_json(
    const ores::reporting::messaging::put_cut_family_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_cut_family_request
wire_traits<ores::reporting::messaging::put_cut_family_request>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_cut_family_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_cut_family_request>::write_msgpack(
    const ores::reporting::messaging::put_cut_family_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_cut_family_request
wire_traits<ores::reporting::messaging::put_cut_family_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_cut_family_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_cut_family_response>::write_json(
    const ores::reporting::messaging::put_cut_family_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_cut_family_response
wire_traits<ores::reporting::messaging::put_cut_family_response>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_cut_family_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_cut_family_response>::write_msgpack(
    const ores::reporting::messaging::put_cut_family_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_cut_family_response
wire_traits<ores::reporting::messaging::put_cut_family_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_cut_family_response>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_cut_families_request>::write_json(
    const ores::reporting::messaging::put_many_cut_families_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_cut_families_request
wire_traits<ores::reporting::messaging::put_many_cut_families_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_cut_families_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_many_cut_families_request>::write_msgpack(
    const ores::reporting::messaging::put_many_cut_families_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_cut_families_request
wire_traits<ores::reporting::messaging::put_many_cut_families_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_cut_families_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_many_cut_families_response>::write_json(
    const ores::reporting::messaging::put_many_cut_families_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_cut_families_response
wire_traits<ores::reporting::messaging::put_many_cut_families_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_cut_families_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_many_cut_families_response>::write_msgpack(
    const ores::reporting::messaging::put_many_cut_families_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_cut_families_response
wire_traits<ores::reporting::messaging::put_many_cut_families_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_cut_families_response>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_cut_family_request>::write_json(
    const ores::reporting::messaging::delete_cut_family_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_cut_family_request
wire_traits<ores::reporting::messaging::delete_cut_family_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_cut_family_request>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_cut_family_request>::write_msgpack(
    const ores::reporting::messaging::delete_cut_family_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_cut_family_request
wire_traits<ores::reporting::messaging::delete_cut_family_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_cut_family_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_cut_family_response>::write_json(
    const ores::reporting::messaging::delete_cut_family_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_cut_family_response
wire_traits<ores::reporting::messaging::delete_cut_family_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_cut_family_response>(text);
}

std::string wire_traits<ores::reporting::messaging::delete_cut_family_response>::write_msgpack(
    const ores::reporting::messaging::delete_cut_family_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_cut_family_response
wire_traits<ores::reporting::messaging::delete_cut_family_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_cut_family_response>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_many_cut_families_request>::write_json(
    const ores::reporting::messaging::delete_many_cut_families_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_cut_families_request
wire_traits<ores::reporting::messaging::delete_many_cut_families_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_cut_families_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_cut_families_request>::write_msgpack(
    const ores::reporting::messaging::delete_many_cut_families_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_cut_families_request
wire_traits<ores::reporting::messaging::delete_many_cut_families_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_cut_families_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_many_cut_families_response>::write_json(
    const ores::reporting::messaging::delete_many_cut_families_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_cut_families_response
wire_traits<ores::reporting::messaging::delete_many_cut_families_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_cut_families_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_cut_families_response>::write_msgpack(
    const ores::reporting::messaging::delete_many_cut_families_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_cut_families_response
wire_traits<ores::reporting::messaging::delete_many_cut_families_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_cut_families_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_family_versions_request>::write_json(
    const ores::reporting::messaging::list_cut_family_versions_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_family_versions_request
wire_traits<ores::reporting::messaging::list_cut_family_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_family_versions_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_family_versions_request>::write_msgpack(
    const ores::reporting::messaging::list_cut_family_versions_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_family_versions_request
wire_traits<ores::reporting::messaging::list_cut_family_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_family_versions_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_cut_family_versions_response>::write_json(
    const ores::reporting::messaging::list_cut_family_versions_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_cut_family_versions_response
wire_traits<ores::reporting::messaging::list_cut_family_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_cut_family_versions_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_cut_family_versions_response>::write_msgpack(
    const ores::reporting::messaging::list_cut_family_versions_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_cut_family_versions_response
wire_traits<ores::reporting::messaging::list_cut_family_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_cut_family_versions_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_version_request>::write_json(
    const ores::reporting::messaging::get_cut_family_version_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_family_version_request
wire_traits<ores::reporting::messaging::get_cut_family_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_family_version_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_version_request>::write_msgpack(
    const ores::reporting::messaging::get_cut_family_version_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_family_version_request
wire_traits<ores::reporting::messaging::get_cut_family_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_family_version_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_version_response>::write_json(
    const ores::reporting::messaging::get_cut_family_version_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_cut_family_version_response
wire_traits<ores::reporting::messaging::get_cut_family_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_cut_family_version_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_cut_family_version_response>::write_msgpack(
    const ores::reporting::messaging::get_cut_family_version_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_cut_family_version_response
wire_traits<ores::reporting::messaging::get_cut_family_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_cut_family_version_response>(data);
}

}
