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
#include "ores.vault.api/domain/secret_codec.hpp"
#include "ores.vault.api/messaging/secret_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::vault::messaging::secret_key>::write_json(
    const ores::vault::messaging::secret_key& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_key
wire_traits<ores::vault::messaging::secret_key>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_key>(text);
}

std::string wire_traits<ores::vault::messaging::secret_key>::write_msgpack(
    const ores::vault::messaging::secret_key& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_key
wire_traits<ores::vault::messaging::secret_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_key>(data);
}

std::string wire_traits<ores::vault::messaging::secret_write>::write_json(
    const ores::vault::messaging::secret_write& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_write
wire_traits<ores::vault::messaging::secret_write>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_write>(text);
}

std::string wire_traits<ores::vault::messaging::secret_write>::write_msgpack(
    const ores::vault::messaging::secret_write& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_write
wire_traits<ores::vault::messaging::secret_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_write>(data);
}

std::string wire_traits<ores::vault::messaging::secret_change>::write_json(
    const ores::vault::messaging::secret_change& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_change
wire_traits<ores::vault::messaging::secret_change>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_change>(text);
}

std::string wire_traits<ores::vault::messaging::secret_change>::write_msgpack(
    const ores::vault::messaging::secret_change& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_change
wire_traits<ores::vault::messaging::secret_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_change>(data);
}

std::string wire_traits<ores::vault::messaging::secret_removal>::write_json(
    const ores::vault::messaging::secret_removal& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_removal
wire_traits<ores::vault::messaging::secret_removal>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_removal>(text);
}

std::string wire_traits<ores::vault::messaging::secret_removal>::write_msgpack(
    const ores::vault::messaging::secret_removal& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_removal
wire_traits<ores::vault::messaging::secret_removal>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_removal>(data);
}

std::string wire_traits<ores::vault::messaging::secret_lookup>::write_json(
    const ores::vault::messaging::secret_lookup& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_lookup
wire_traits<ores::vault::messaging::secret_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_lookup>(text);
}

std::string wire_traits<ores::vault::messaging::secret_lookup>::write_msgpack(
    const ores::vault::messaging::secret_lookup& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_lookup
wire_traits<ores::vault::messaging::secret_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_lookup>(data);
}

std::string wire_traits<ores::vault::messaging::secrets_filter>::write_json(
    const ores::vault::messaging::secrets_filter& v) {
    return object::write_json(v);
}

ores::vault::messaging::secrets_filter
wire_traits<ores::vault::messaging::secrets_filter>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secrets_filter>(text);
}

std::string wire_traits<ores::vault::messaging::secrets_filter>::write_msgpack(
    const ores::vault::messaging::secrets_filter& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secrets_filter
wire_traits<ores::vault::messaging::secrets_filter>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secrets_filter>(data);
}

std::string wire_traits<ores::vault::messaging::secret_event>::write_json(
    const ores::vault::messaging::secret_event& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_event
wire_traits<ores::vault::messaging::secret_event>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_event>(text);
}

std::string wire_traits<ores::vault::messaging::secret_event>::write_msgpack(
    const ores::vault::messaging::secret_event& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_event
wire_traits<ores::vault::messaging::secret_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_event>(data);
}

std::string wire_traits<ores::vault::messaging::secret_version_key>::write_json(
    const ores::vault::messaging::secret_version_key& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_version_key
wire_traits<ores::vault::messaging::secret_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_version_key>(text);
}

std::string wire_traits<ores::vault::messaging::secret_version_key>::write_msgpack(
    const ores::vault::messaging::secret_version_key& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_version_key
wire_traits<ores::vault::messaging::secret_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_version_key>(data);
}

std::string wire_traits<ores::vault::messaging::secret_versions_filter>::write_json(
    const ores::vault::messaging::secret_versions_filter& v) {
    return object::write_json(v);
}

ores::vault::messaging::secret_versions_filter
wire_traits<ores::vault::messaging::secret_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::secret_versions_filter>(text);
}

std::string wire_traits<ores::vault::messaging::secret_versions_filter>::write_msgpack(
    const ores::vault::messaging::secret_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::secret_versions_filter
wire_traits<ores::vault::messaging::secret_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::secret_versions_filter>(data);
}

std::string wire_traits<ores::vault::messaging::list_secrets_request>::write_json(
    const ores::vault::messaging::list_secrets_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secrets_request
wire_traits<ores::vault::messaging::list_secrets_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secrets_request>(text);
}

std::string wire_traits<ores::vault::messaging::list_secrets_request>::write_msgpack(
    const ores::vault::messaging::list_secrets_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secrets_request
wire_traits<ores::vault::messaging::list_secrets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secrets_request>(data);
}

std::string wire_traits<ores::vault::messaging::list_secrets_response>::write_json(
    const ores::vault::messaging::list_secrets_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secrets_response
wire_traits<ores::vault::messaging::list_secrets_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secrets_response>(text);
}

std::string wire_traits<ores::vault::messaging::list_secrets_response>::write_msgpack(
    const ores::vault::messaging::list_secrets_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secrets_response
wire_traits<ores::vault::messaging::list_secrets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secrets_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_request>::write_json(
    const ores::vault::messaging::get_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_request
wire_traits<ores::vault::messaging::get_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_request>::write_msgpack(
    const ores::vault::messaging::get_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_request
wire_traits<ores::vault::messaging::get_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_response>::write_json(
    const ores::vault::messaging::get_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_response
wire_traits<ores::vault::messaging::get_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_response>::write_msgpack(
    const ores::vault::messaging::get_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_response
wire_traits<ores::vault::messaging::get_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_secrets_request>::write_json(
    const ores::vault::messaging::get_many_secrets_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_secrets_request
wire_traits<ores::vault::messaging::get_many_secrets_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_secrets_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_secrets_request>::write_msgpack(
    const ores::vault::messaging::get_many_secrets_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_secrets_request
wire_traits<ores::vault::messaging::get_many_secrets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_secrets_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_many_secrets_response>::write_json(
    const ores::vault::messaging::get_many_secrets_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_many_secrets_response
wire_traits<ores::vault::messaging::get_many_secrets_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_many_secrets_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_many_secrets_response>::write_msgpack(
    const ores::vault::messaging::get_many_secrets_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_many_secrets_response
wire_traits<ores::vault::messaging::get_many_secrets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_many_secrets_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_secret_request>::write_json(
    const ores::vault::messaging::put_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_secret_request
wire_traits<ores::vault::messaging::put_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_secret_request>::write_msgpack(
    const ores::vault::messaging::put_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_secret_request
wire_traits<ores::vault::messaging::put_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_secret_response>::write_json(
    const ores::vault::messaging::put_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_secret_response
wire_traits<ores::vault::messaging::put_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_secret_response>::write_msgpack(
    const ores::vault::messaging::put_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_secret_response
wire_traits<ores::vault::messaging::put_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_secrets_request>::write_json(
    const ores::vault::messaging::put_many_secrets_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_secrets_request
wire_traits<ores::vault::messaging::put_many_secrets_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_secrets_request>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_secrets_request>::write_msgpack(
    const ores::vault::messaging::put_many_secrets_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_secrets_request
wire_traits<ores::vault::messaging::put_many_secrets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_secrets_request>(data);
}

std::string wire_traits<ores::vault::messaging::put_many_secrets_response>::write_json(
    const ores::vault::messaging::put_many_secrets_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::put_many_secrets_response
wire_traits<ores::vault::messaging::put_many_secrets_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::put_many_secrets_response>(text);
}

std::string wire_traits<ores::vault::messaging::put_many_secrets_response>::write_msgpack(
    const ores::vault::messaging::put_many_secrets_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::put_many_secrets_response
wire_traits<ores::vault::messaging::put_many_secrets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::put_many_secrets_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_secret_request>::write_json(
    const ores::vault::messaging::delete_secret_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_secret_request
wire_traits<ores::vault::messaging::delete_secret_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_secret_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_secret_request>::write_msgpack(
    const ores::vault::messaging::delete_secret_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_secret_request
wire_traits<ores::vault::messaging::delete_secret_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_secret_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_secret_response>::write_json(
    const ores::vault::messaging::delete_secret_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_secret_response
wire_traits<ores::vault::messaging::delete_secret_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_secret_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_secret_response>::write_msgpack(
    const ores::vault::messaging::delete_secret_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_secret_response
wire_traits<ores::vault::messaging::delete_secret_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_secret_response>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_secrets_request>::write_json(
    const ores::vault::messaging::delete_many_secrets_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_secrets_request
wire_traits<ores::vault::messaging::delete_many_secrets_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_secrets_request>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_secrets_request>::write_msgpack(
    const ores::vault::messaging::delete_many_secrets_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_secrets_request
wire_traits<ores::vault::messaging::delete_many_secrets_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_secrets_request>(data);
}

std::string wire_traits<ores::vault::messaging::delete_many_secrets_response>::write_json(
    const ores::vault::messaging::delete_many_secrets_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::delete_many_secrets_response
wire_traits<ores::vault::messaging::delete_many_secrets_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::delete_many_secrets_response>(text);
}

std::string wire_traits<ores::vault::messaging::delete_many_secrets_response>::write_msgpack(
    const ores::vault::messaging::delete_many_secrets_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::delete_many_secrets_response
wire_traits<ores::vault::messaging::delete_many_secrets_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::delete_many_secrets_response>(data);
}

std::string wire_traits<ores::vault::messaging::list_secret_versions_request>::write_json(
    const ores::vault::messaging::list_secret_versions_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secret_versions_request
wire_traits<ores::vault::messaging::list_secret_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secret_versions_request>(text);
}

std::string wire_traits<ores::vault::messaging::list_secret_versions_request>::write_msgpack(
    const ores::vault::messaging::list_secret_versions_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secret_versions_request
wire_traits<ores::vault::messaging::list_secret_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secret_versions_request>(data);
}

std::string wire_traits<ores::vault::messaging::list_secret_versions_response>::write_json(
    const ores::vault::messaging::list_secret_versions_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::list_secret_versions_response
wire_traits<ores::vault::messaging::list_secret_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::vault::messaging::list_secret_versions_response>(text);
}

std::string wire_traits<ores::vault::messaging::list_secret_versions_response>::write_msgpack(
    const ores::vault::messaging::list_secret_versions_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::list_secret_versions_response
wire_traits<ores::vault::messaging::list_secret_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::list_secret_versions_response>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_version_request>::write_json(
    const ores::vault::messaging::get_secret_version_request& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_version_request
wire_traits<ores::vault::messaging::get_secret_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_version_request>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_version_request>::write_msgpack(
    const ores::vault::messaging::get_secret_version_request& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_version_request
wire_traits<ores::vault::messaging::get_secret_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_version_request>(data);
}

std::string wire_traits<ores::vault::messaging::get_secret_version_response>::write_json(
    const ores::vault::messaging::get_secret_version_response& v) {
    return object::write_json(v);
}

ores::vault::messaging::get_secret_version_response
wire_traits<ores::vault::messaging::get_secret_version_response>::read_json(std::string_view text) {
    return object::read_json<ores::vault::messaging::get_secret_version_response>(text);
}

std::string wire_traits<ores::vault::messaging::get_secret_version_response>::write_msgpack(
    const ores::vault::messaging::get_secret_version_response& v) {
    return object::write_msgpack(v);
}

ores::vault::messaging::get_secret_version_response
wire_traits<ores::vault::messaging::get_secret_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::vault::messaging::get_secret_version_response>(data);
}

}
