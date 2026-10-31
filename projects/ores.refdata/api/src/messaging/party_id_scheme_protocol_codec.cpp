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
#include "ores.refdata.api/domain/party_id_scheme_codec.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::party_id_scheme_key>::write_json(
    const ores::refdata::messaging::party_id_scheme_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_key
wire_traits<ores::refdata::messaging::party_id_scheme_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_key>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_key>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_key
wire_traits<ores::refdata::messaging::party_id_scheme_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_key>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_write>::write_json(
    const ores::refdata::messaging::party_id_scheme_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_write
wire_traits<ores::refdata::messaging::party_id_scheme_write>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_write>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_write>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_write
wire_traits<ores::refdata::messaging::party_id_scheme_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_write>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_change>::write_json(
    const ores::refdata::messaging::party_id_scheme_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_change
wire_traits<ores::refdata::messaging::party_id_scheme_change>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_change>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_change>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_change
wire_traits<ores::refdata::messaging::party_id_scheme_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_change>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_removal>::write_json(
    const ores::refdata::messaging::party_id_scheme_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_removal
wire_traits<ores::refdata::messaging::party_id_scheme_removal>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_removal>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_removal
wire_traits<ores::refdata::messaging::party_id_scheme_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_lookup>::write_json(
    const ores::refdata::messaging::party_id_scheme_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_lookup
wire_traits<ores::refdata::messaging::party_id_scheme_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_lookup>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_lookup
wire_traits<ores::refdata::messaging::party_id_scheme_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_schemes_filter>::write_json(
    const ores::refdata::messaging::party_id_schemes_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_schemes_filter
wire_traits<ores::refdata::messaging::party_id_schemes_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_schemes_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_schemes_filter>::write_msgpack(
    const ores::refdata::messaging::party_id_schemes_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_schemes_filter
wire_traits<ores::refdata::messaging::party_id_schemes_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_schemes_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_event>::write_json(
    const ores::refdata::messaging::party_id_scheme_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_event
wire_traits<ores::refdata::messaging::party_id_scheme_event>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_event>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_event>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_event
wire_traits<ores::refdata::messaging::party_id_scheme_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_event>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_version_key>::write_json(
    const ores::refdata::messaging::party_id_scheme_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_version_key
wire_traits<ores::refdata::messaging::party_id_scheme_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_version_key>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_version_key
wire_traits<ores::refdata::messaging::party_id_scheme_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_version_key>(data);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_versions_filter>::write_json(
    const ores::refdata::messaging::party_id_scheme_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::party_id_scheme_versions_filter
wire_traits<ores::refdata::messaging::party_id_scheme_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::party_id_scheme_versions_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::party_id_scheme_versions_filter>::write_msgpack(
    const ores::refdata::messaging::party_id_scheme_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::party_id_scheme_versions_filter
wire_traits<ores::refdata::messaging::party_id_scheme_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::party_id_scheme_versions_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_party_id_schemes_request>::write_json(
    const ores::refdata::messaging::list_party_id_schemes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_party_id_schemes_request
wire_traits<ores::refdata::messaging::list_party_id_schemes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_party_id_schemes_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_party_id_schemes_request>::write_msgpack(
    const ores::refdata::messaging::list_party_id_schemes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_party_id_schemes_request
wire_traits<ores::refdata::messaging::list_party_id_schemes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_party_id_schemes_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_party_id_schemes_response>::write_json(
    const ores::refdata::messaging::list_party_id_schemes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_party_id_schemes_response
wire_traits<ores::refdata::messaging::list_party_id_schemes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_party_id_schemes_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_party_id_schemes_response>::write_msgpack(
    const ores::refdata::messaging::list_party_id_schemes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_party_id_schemes_response
wire_traits<ores::refdata::messaging::list_party_id_schemes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_party_id_schemes_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_request>::write_json(
    const ores::refdata::messaging::get_party_id_scheme_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_party_id_scheme_request
wire_traits<ores::refdata::messaging::get_party_id_scheme_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_party_id_scheme_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_request>::write_msgpack(
    const ores::refdata::messaging::get_party_id_scheme_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_party_id_scheme_request
wire_traits<ores::refdata::messaging::get_party_id_scheme_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_party_id_scheme_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_response>::write_json(
    const ores::refdata::messaging::get_party_id_scheme_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_party_id_scheme_response
wire_traits<ores::refdata::messaging::get_party_id_scheme_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_party_id_scheme_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_response>::write_msgpack(
    const ores::refdata::messaging::get_party_id_scheme_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_party_id_scheme_response
wire_traits<ores::refdata::messaging::get_party_id_scheme_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_party_id_scheme_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_party_id_schemes_request>::write_json(
    const ores::refdata::messaging::get_many_party_id_schemes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::get_many_party_id_schemes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_party_id_schemes_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_party_id_schemes_request>::write_msgpack(
    const ores::refdata::messaging::get_many_party_id_schemes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::get_many_party_id_schemes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_party_id_schemes_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_party_id_schemes_response>::write_json(
    const ores::refdata::messaging::get_many_party_id_schemes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::get_many_party_id_schemes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_party_id_schemes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_party_id_schemes_response>::write_msgpack(
    const ores::refdata::messaging::get_many_party_id_schemes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::get_many_party_id_schemes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_party_id_schemes_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_party_id_scheme_request>::write_json(
    const ores::refdata::messaging::put_party_id_scheme_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_party_id_scheme_request
wire_traits<ores::refdata::messaging::put_party_id_scheme_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_party_id_scheme_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_party_id_scheme_request>::write_msgpack(
    const ores::refdata::messaging::put_party_id_scheme_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_party_id_scheme_request
wire_traits<ores::refdata::messaging::put_party_id_scheme_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_party_id_scheme_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_party_id_scheme_response>::write_json(
    const ores::refdata::messaging::put_party_id_scheme_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_party_id_scheme_response
wire_traits<ores::refdata::messaging::put_party_id_scheme_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_party_id_scheme_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_party_id_scheme_response>::write_msgpack(
    const ores::refdata::messaging::put_party_id_scheme_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_party_id_scheme_response
wire_traits<ores::refdata::messaging::put_party_id_scheme_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_party_id_scheme_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_party_id_schemes_request>::write_json(
    const ores::refdata::messaging::put_many_party_id_schemes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::put_many_party_id_schemes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_party_id_schemes_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_party_id_schemes_request>::write_msgpack(
    const ores::refdata::messaging::put_many_party_id_schemes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::put_many_party_id_schemes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_party_id_schemes_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_party_id_schemes_response>::write_json(
    const ores::refdata::messaging::put_many_party_id_schemes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::put_many_party_id_schemes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_party_id_schemes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_party_id_schemes_response>::write_msgpack(
    const ores::refdata::messaging::put_many_party_id_schemes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::put_many_party_id_schemes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_party_id_schemes_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_party_id_scheme_request>::write_json(
    const ores::refdata::messaging::delete_party_id_scheme_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_party_id_scheme_request
wire_traits<ores::refdata::messaging::delete_party_id_scheme_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_party_id_scheme_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_party_id_scheme_request>::write_msgpack(
    const ores::refdata::messaging::delete_party_id_scheme_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_party_id_scheme_request
wire_traits<ores::refdata::messaging::delete_party_id_scheme_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_party_id_scheme_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_party_id_scheme_response>::write_json(
    const ores::refdata::messaging::delete_party_id_scheme_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_party_id_scheme_response
wire_traits<ores::refdata::messaging::delete_party_id_scheme_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_party_id_scheme_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_party_id_scheme_response>::write_msgpack(
    const ores::refdata::messaging::delete_party_id_scheme_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_party_id_scheme_response
wire_traits<ores::refdata::messaging::delete_party_id_scheme_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_party_id_scheme_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_request>::write_json(
    const ores::refdata::messaging::delete_many_party_id_schemes_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_party_id_schemes_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_party_id_schemes_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_party_id_schemes_request
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_party_id_schemes_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_response>::write_json(
    const ores::refdata::messaging::delete_many_party_id_schemes_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_party_id_schemes_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_party_id_schemes_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_party_id_schemes_response
wire_traits<ores::refdata::messaging::delete_many_party_id_schemes_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_party_id_schemes_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_request>::write_json(
    const ores::refdata::messaging::list_party_id_scheme_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_party_id_scheme_versions_request
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_party_id_scheme_versions_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_party_id_scheme_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_party_id_scheme_versions_request
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_party_id_scheme_versions_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_response>::write_json(
    const ores::refdata::messaging::list_party_id_scheme_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_party_id_scheme_versions_response
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_party_id_scheme_versions_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_party_id_scheme_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_party_id_scheme_versions_response
wire_traits<ores::refdata::messaging::list_party_id_scheme_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_party_id_scheme_versions_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_version_request>::write_json(
    const ores::refdata::messaging::get_party_id_scheme_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_party_id_scheme_version_request
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_party_id_scheme_version_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_request>::write_msgpack(
    const ores::refdata::messaging::get_party_id_scheme_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_party_id_scheme_version_request
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_party_id_scheme_version_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::get_party_id_scheme_version_response>::write_json(
    const ores::refdata::messaging::get_party_id_scheme_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_party_id_scheme_version_response
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_party_id_scheme_version_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_response>::write_msgpack(
    const ores::refdata::messaging::get_party_id_scheme_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_party_id_scheme_version_response
wire_traits<ores::refdata::messaging::get_party_id_scheme_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_party_id_scheme_version_response>(
        data);
}

}
