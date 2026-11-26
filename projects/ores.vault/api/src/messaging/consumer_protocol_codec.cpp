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
#include "ores.vault.api/domain/consumer_codec.hpp"
#include "ores.vault.api/messaging/consumer_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::vault::messaging::consumer_key>::write_json(
    const ores::vault::messaging::consumer_key& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_key
wire_traits<ores::vault::messaging::consumer_key>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_key>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_key>::write_msgpack(
    const ores::vault::messaging::consumer_key& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_key
wire_traits<ores::vault::messaging::consumer_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_key>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_write>::write_json(
    const ores::vault::messaging::consumer_write& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_write
wire_traits<ores::vault::messaging::consumer_write>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_write>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_write>::write_msgpack(
    const ores::vault::messaging::consumer_write& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_write
wire_traits<ores::vault::messaging::consumer_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_write>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_change>::write_json(
    const ores::vault::messaging::consumer_change& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_change
wire_traits<ores::vault::messaging::consumer_change>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_change>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_change>::write_msgpack(
    const ores::vault::messaging::consumer_change& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_change
wire_traits<ores::vault::messaging::consumer_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_change>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_removal>::write_json(
    const ores::vault::messaging::consumer_removal& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_removal
wire_traits<ores::vault::messaging::consumer_removal>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_removal>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_removal>::write_msgpack(
    const ores::vault::messaging::consumer_removal& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_removal
wire_traits<ores::vault::messaging::consumer_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_removal>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_lookup>::write_json(
    const ores::vault::messaging::consumer_lookup& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_lookup
wire_traits<ores::vault::messaging::consumer_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_lookup>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_lookup>::write_msgpack(
    const ores::vault::messaging::consumer_lookup& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_lookup
wire_traits<ores::vault::messaging::consumer_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_lookup>(data);
}

std::string wire_traits<ores::vault::messaging::consumers_filter>::write_json(
    const ores::vault::messaging::consumers_filter& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumers_filter
wire_traits<ores::vault::messaging::consumers_filter>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumers_filter>(text);
}

std::string wire_traits<ores::vault::messaging::consumers_filter>::write_msgpack(
    const ores::vault::messaging::consumers_filter& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumers_filter
wire_traits<ores::vault::messaging::consumers_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumers_filter>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_event>::write_json(
    const ores::vault::messaging::consumer_event& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_event
wire_traits<ores::vault::messaging::consumer_event>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_event>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_event>::write_msgpack(
    const ores::vault::messaging::consumer_event& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_event
wire_traits<ores::vault::messaging::consumer_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_event>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_version_key>::write_json(
    const ores::vault::messaging::consumer_version_key& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_version_key
wire_traits<ores::vault::messaging::consumer_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_version_key>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_version_key>::write_msgpack(
    const ores::vault::messaging::consumer_version_key& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_version_key
wire_traits<ores::vault::messaging::consumer_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_version_key>(data);
}

std::string wire_traits<ores::vault::messaging::consumer_versions_filter>::write_json(
    const ores::vault::messaging::consumer_versions_filter& v) {
    return object::write_json(v);
}

ores::vault::messaging::consumer_versions_filter
wire_traits<ores::vault::messaging::consumer_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::consumer_versions_filter>(text);
}

std::string wire_traits<ores::vault::messaging::consumer_versions_filter>::write_msgpack(
    const ores::vault::messaging::consumer_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::consumer_versions_filter
wire_traits<ores::vault::messaging::consumer_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::consumer_versions_filter>(data);
}

std::string wire_traits<ores::vault::messaging::list_consumers_request>::write_json(
    const ores::vault::messaging::list_consumers_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_consumers_request
wire_traits<ores::vault::messaging::list_consumers_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_consumers_request>(text);
}

std::string wire_traits<ores::vault::messaging::list_consumers_request>::write_msgpack(
    const ores::vault::messaging::list_consumers_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_consumers_request
wire_traits<ores::vault::messaging::list_consumers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_consumers_request>(data);
}

std::string wire_traits<ores::vault::messaging::list_consumers_response>::write_json(
    const ores::vault::messaging::list_consumers_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_consumers_response
wire_traits<ores::vault::messaging::list_consumers_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_consumers_response>(text);
}

std::string wire_traits<ores::vault::messaging::list_consumers_response>::write_msgpack(
    const ores::vault::messaging::list_consumers_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_consumers_response
wire_traits<ores::vault::messaging::list_consumers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_consumers_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_consumer_request>::write_json(
    const ores::vault::messaging::get_consumer_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_consumer_request
wire_traits<ores::vault::messaging::get_consumer_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_consumer_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_consumer_request>::write_msgpack(
    const ores::vault::messaging::get_consumer_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_consumer_request
wire_traits<ores::vault::messaging::get_consumer_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_consumer_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_consumer_response>::write_json(
    const ores::vault::messaging::get_consumer_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_consumer_response
wire_traits<ores::vault::messaging::get_consumer_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_consumer_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_consumer_response>::write_msgpack(
    const ores::vault::messaging::get_consumer_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_consumer_response
wire_traits<ores::vault::messaging::get_consumer_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_consumer_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_consumers_request>::write_json(
    const ores::vault::messaging::get_many_consumers_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_consumers_request
wire_traits<ores::vault::messaging::get_many_consumers_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_consumers_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_consumers_request>::write_msgpack(
    const ores::vault::messaging::get_many_consumers_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_consumers_request
wire_traits<ores::vault::messaging::get_many_consumers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_consumers_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_consumers_response>::write_json(
    const ores::vault::messaging::get_many_consumers_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_consumers_response
wire_traits<ores::vault::messaging::get_many_consumers_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_consumers_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_consumers_response>::write_msgpack(
    const ores::vault::messaging::get_many_consumers_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_consumers_response
wire_traits<ores::vault::messaging::get_many_consumers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_consumers_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_consumer_request>::write_json(
    const ores::vault::messaging::put_consumer_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_consumer_request
wire_traits<ores::vault::messaging::put_consumer_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_consumer_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_consumer_request>::write_msgpack(
    const ores::vault::messaging::put_consumer_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_consumer_request
wire_traits<ores::vault::messaging::put_consumer_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_consumer_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_consumer_response>::write_json(
    const ores::vault::messaging::put_consumer_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_consumer_response
wire_traits<ores::vault::messaging::put_consumer_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_consumer_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_consumer_response>::write_msgpack(
    const ores::vault::messaging::put_consumer_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_consumer_response
wire_traits<ores::vault::messaging::put_consumer_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_consumer_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_consumers_request>::write_json(
    const ores::vault::messaging::put_many_consumers_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_consumers_request
wire_traits<ores::vault::messaging::put_many_consumers_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_consumers_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_consumers_request>::write_msgpack(
    const ores::vault::messaging::put_many_consumers_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_consumers_request
wire_traits<ores::vault::messaging::put_many_consumers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_consumers_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_consumers_response>::write_json(
    const ores::vault::messaging::put_many_consumers_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_consumers_response
wire_traits<ores::vault::messaging::put_many_consumers_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_consumers_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_consumers_response>::write_msgpack(
    const ores::vault::messaging::put_many_consumers_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_consumers_response
wire_traits<ores::vault::messaging::put_many_consumers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_consumers_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_consumer_request>::write_json(
    const ores::vault::messaging::delete_consumer_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_consumer_request
wire_traits<ores::vault::messaging::delete_consumer_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_consumer_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_consumer_request>::write_msgpack(
    const ores::vault::messaging::delete_consumer_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_consumer_request
wire_traits<ores::vault::messaging::delete_consumer_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_consumer_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_consumer_response>::write_json(
    const ores::vault::messaging::delete_consumer_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_consumer_response
wire_traits<ores::vault::messaging::delete_consumer_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_consumer_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_consumer_response>::write_msgpack(
    const ores::vault::messaging::delete_consumer_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_consumer_response
wire_traits<ores::vault::messaging::delete_consumer_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_consumer_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_consumers_request>::write_json(
    const ores::vault::messaging::delete_many_consumers_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_consumers_request
wire_traits<ores::vault::messaging::delete_many_consumers_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_consumers_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_consumers_request>::write_msgpack(
    const ores::vault::messaging::delete_many_consumers_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_consumers_request
wire_traits<ores::vault::messaging::delete_many_consumers_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_consumers_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_consumers_response>::write_json(
    const ores::vault::messaging::delete_many_consumers_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_consumers_response
wire_traits<ores::vault::messaging::delete_many_consumers_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_consumers_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_consumers_response>::write_msgpack(
    const ores::vault::messaging::delete_many_consumers_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_consumers_response
wire_traits<ores::vault::messaging::delete_many_consumers_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_consumers_response>(data);
}

std::string wire_traits<ores::vault::messaging::list_consumer_versions_request>::write_json(
    const ores::vault::messaging::list_consumer_versions_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_consumer_versions_request
wire_traits<ores::vault::messaging::list_consumer_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::list_consumer_versions_request>(text);
}

std::string wire_traits<ores::vault::messaging::list_consumer_versions_request>::write_msgpack(
    const ores::vault::messaging::list_consumer_versions_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_consumer_versions_request
wire_traits<ores::vault::messaging::list_consumer_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_consumer_versions_request>(data);
}

std::string wire_traits<ores::vault::messaging::list_consumer_versions_response>::write_json(
    const ores::vault::messaging::list_consumer_versions_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_consumer_versions_response
wire_traits<ores::vault::messaging::list_consumer_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::list_consumer_versions_response>(text);
}

std::string wire_traits<ores::vault::messaging::list_consumer_versions_response>::write_msgpack(
    const ores::vault::messaging::list_consumer_versions_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_consumer_versions_response
wire_traits<ores::vault::messaging::list_consumer_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_consumer_versions_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_consumer_version_request>::write_json(
    const ores::vault::messaging::get_consumer_version_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_consumer_version_request
wire_traits<ores::vault::messaging::get_consumer_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::get_consumer_version_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_consumer_version_request>::write_msgpack(
    const ores::vault::messaging::get_consumer_version_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_consumer_version_request
wire_traits<ores::vault::messaging::get_consumer_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_consumer_version_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_consumer_version_response>::write_json(
    const ores::vault::messaging::get_consumer_version_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_consumer_version_response
wire_traits<ores::vault::messaging::get_consumer_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::get_consumer_version_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_consumer_version_response>::write_msgpack(
    const ores::vault::messaging::get_consumer_version_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_consumer_version_response
wire_traits<ores::vault::messaging::get_consumer_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_consumer_version_response>(data);
}

}
