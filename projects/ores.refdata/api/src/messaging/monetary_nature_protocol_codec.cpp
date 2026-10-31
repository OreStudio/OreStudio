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
#include "ores.refdata.api/domain/monetary_nature_codec.hpp"
#include "ores.refdata.api/messaging/monetary_nature_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::monetary_nature_key>::write_json(
    const ores::refdata::messaging::monetary_nature_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_key
wire_traits<ores::refdata::messaging::monetary_nature_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_key>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_key>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_key
wire_traits<ores::refdata::messaging::monetary_nature_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_key>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_write>::write_json(
    const ores::refdata::messaging::monetary_nature_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_write
wire_traits<ores::refdata::messaging::monetary_nature_write>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_write>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_write>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_write
wire_traits<ores::refdata::messaging::monetary_nature_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_write>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_change>::write_json(
    const ores::refdata::messaging::monetary_nature_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_change
wire_traits<ores::refdata::messaging::monetary_nature_change>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_change>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_change>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_change
wire_traits<ores::refdata::messaging::monetary_nature_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_change>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_removal>::write_json(
    const ores::refdata::messaging::monetary_nature_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_removal
wire_traits<ores::refdata::messaging::monetary_nature_removal>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_removal>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_removal
wire_traits<ores::refdata::messaging::monetary_nature_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_lookup>::write_json(
    const ores::refdata::messaging::monetary_nature_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_lookup
wire_traits<ores::refdata::messaging::monetary_nature_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_lookup>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_lookup
wire_traits<ores::refdata::messaging::monetary_nature_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_natures_filter>::write_json(
    const ores::refdata::messaging::monetary_natures_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_natures_filter
wire_traits<ores::refdata::messaging::monetary_natures_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_natures_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_natures_filter>::write_msgpack(
    const ores::refdata::messaging::monetary_natures_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_natures_filter
wire_traits<ores::refdata::messaging::monetary_natures_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_natures_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_event>::write_json(
    const ores::refdata::messaging::monetary_nature_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_event
wire_traits<ores::refdata::messaging::monetary_nature_event>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_event>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_event>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_event
wire_traits<ores::refdata::messaging::monetary_nature_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_event>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_version_key>::write_json(
    const ores::refdata::messaging::monetary_nature_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_version_key
wire_traits<ores::refdata::messaging::monetary_nature_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_version_key>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_version_key
wire_traits<ores::refdata::messaging::monetary_nature_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_version_key>(data);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_versions_filter>::write_json(
    const ores::refdata::messaging::monetary_nature_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::monetary_nature_versions_filter
wire_traits<ores::refdata::messaging::monetary_nature_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::monetary_nature_versions_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::monetary_nature_versions_filter>::write_msgpack(
    const ores::refdata::messaging::monetary_nature_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::monetary_nature_versions_filter
wire_traits<ores::refdata::messaging::monetary_nature_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::monetary_nature_versions_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_monetary_natures_request>::write_json(
    const ores::refdata::messaging::list_monetary_natures_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_monetary_natures_request
wire_traits<ores::refdata::messaging::list_monetary_natures_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_monetary_natures_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_monetary_natures_request>::write_msgpack(
    const ores::refdata::messaging::list_monetary_natures_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_monetary_natures_request
wire_traits<ores::refdata::messaging::list_monetary_natures_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_monetary_natures_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_monetary_natures_response>::write_json(
    const ores::refdata::messaging::list_monetary_natures_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_monetary_natures_response
wire_traits<ores::refdata::messaging::list_monetary_natures_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_monetary_natures_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_monetary_natures_response>::write_msgpack(
    const ores::refdata::messaging::list_monetary_natures_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_monetary_natures_response
wire_traits<ores::refdata::messaging::list_monetary_natures_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_monetary_natures_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_request>::write_json(
    const ores::refdata::messaging::get_monetary_nature_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_monetary_nature_request
wire_traits<ores::refdata::messaging::get_monetary_nature_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_monetary_nature_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_request>::write_msgpack(
    const ores::refdata::messaging::get_monetary_nature_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_monetary_nature_request
wire_traits<ores::refdata::messaging::get_monetary_nature_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_monetary_nature_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_response>::write_json(
    const ores::refdata::messaging::get_monetary_nature_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_monetary_nature_response
wire_traits<ores::refdata::messaging::get_monetary_nature_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_monetary_nature_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_response>::write_msgpack(
    const ores::refdata::messaging::get_monetary_nature_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_monetary_nature_response
wire_traits<ores::refdata::messaging::get_monetary_nature_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_monetary_nature_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_monetary_natures_request>::write_json(
    const ores::refdata::messaging::get_many_monetary_natures_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_monetary_natures_request
wire_traits<ores::refdata::messaging::get_many_monetary_natures_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_monetary_natures_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_monetary_natures_request>::write_msgpack(
    const ores::refdata::messaging::get_many_monetary_natures_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_monetary_natures_request
wire_traits<ores::refdata::messaging::get_many_monetary_natures_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_monetary_natures_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_monetary_natures_response>::write_json(
    const ores::refdata::messaging::get_many_monetary_natures_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_monetary_natures_response
wire_traits<ores::refdata::messaging::get_many_monetary_natures_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_monetary_natures_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_monetary_natures_response>::write_msgpack(
    const ores::refdata::messaging::get_many_monetary_natures_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_monetary_natures_response
wire_traits<ores::refdata::messaging::get_many_monetary_natures_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_monetary_natures_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_monetary_nature_request>::write_json(
    const ores::refdata::messaging::put_monetary_nature_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_monetary_nature_request
wire_traits<ores::refdata::messaging::put_monetary_nature_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_monetary_nature_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_monetary_nature_request>::write_msgpack(
    const ores::refdata::messaging::put_monetary_nature_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_monetary_nature_request
wire_traits<ores::refdata::messaging::put_monetary_nature_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_monetary_nature_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_monetary_nature_response>::write_json(
    const ores::refdata::messaging::put_monetary_nature_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_monetary_nature_response
wire_traits<ores::refdata::messaging::put_monetary_nature_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_monetary_nature_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_monetary_nature_response>::write_msgpack(
    const ores::refdata::messaging::put_monetary_nature_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_monetary_nature_response
wire_traits<ores::refdata::messaging::put_monetary_nature_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_monetary_nature_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_monetary_natures_request>::write_json(
    const ores::refdata::messaging::put_many_monetary_natures_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_monetary_natures_request
wire_traits<ores::refdata::messaging::put_many_monetary_natures_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_monetary_natures_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_monetary_natures_request>::write_msgpack(
    const ores::refdata::messaging::put_many_monetary_natures_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_monetary_natures_request
wire_traits<ores::refdata::messaging::put_many_monetary_natures_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_monetary_natures_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_monetary_natures_response>::write_json(
    const ores::refdata::messaging::put_many_monetary_natures_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_monetary_natures_response
wire_traits<ores::refdata::messaging::put_many_monetary_natures_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_monetary_natures_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_monetary_natures_response>::write_msgpack(
    const ores::refdata::messaging::put_many_monetary_natures_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_monetary_natures_response
wire_traits<ores::refdata::messaging::put_many_monetary_natures_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_monetary_natures_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_monetary_nature_request>::write_json(
    const ores::refdata::messaging::delete_monetary_nature_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_monetary_nature_request
wire_traits<ores::refdata::messaging::delete_monetary_nature_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_monetary_nature_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_monetary_nature_request>::write_msgpack(
    const ores::refdata::messaging::delete_monetary_nature_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_monetary_nature_request
wire_traits<ores::refdata::messaging::delete_monetary_nature_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_monetary_nature_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_monetary_nature_response>::write_json(
    const ores::refdata::messaging::delete_monetary_nature_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_monetary_nature_response
wire_traits<ores::refdata::messaging::delete_monetary_nature_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_monetary_nature_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_monetary_nature_response>::write_msgpack(
    const ores::refdata::messaging::delete_monetary_nature_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_monetary_nature_response
wire_traits<ores::refdata::messaging::delete_monetary_nature_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_monetary_nature_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_monetary_natures_request>::write_json(
    const ores::refdata::messaging::delete_many_monetary_natures_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_monetary_natures_request
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_monetary_natures_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_monetary_natures_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_monetary_natures_request
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_monetary_natures_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_response>::write_json(
    const ores::refdata::messaging::delete_many_monetary_natures_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_monetary_natures_response
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_monetary_natures_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_monetary_natures_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_monetary_natures_response
wire_traits<ores::refdata::messaging::delete_many_monetary_natures_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_monetary_natures_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_request>::write_json(
    const ores::refdata::messaging::list_monetary_nature_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_monetary_nature_versions_request
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_monetary_nature_versions_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_monetary_nature_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_monetary_nature_versions_request
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_monetary_nature_versions_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_response>::write_json(
    const ores::refdata::messaging::list_monetary_nature_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_monetary_nature_versions_response
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_monetary_nature_versions_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_monetary_nature_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_monetary_nature_versions_response
wire_traits<ores::refdata::messaging::list_monetary_nature_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_monetary_nature_versions_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_version_request>::write_json(
    const ores::refdata::messaging::get_monetary_nature_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_monetary_nature_version_request
wire_traits<ores::refdata::messaging::get_monetary_nature_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_monetary_nature_version_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_monetary_nature_version_request>::write_msgpack(
    const ores::refdata::messaging::get_monetary_nature_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_monetary_nature_version_request
wire_traits<ores::refdata::messaging::get_monetary_nature_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_monetary_nature_version_request>(
        data);
}

std::string wire_traits<ores::refdata::messaging::get_monetary_nature_version_response>::write_json(
    const ores::refdata::messaging::get_monetary_nature_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_monetary_nature_version_response
wire_traits<ores::refdata::messaging::get_monetary_nature_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_monetary_nature_version_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_monetary_nature_version_response>::write_msgpack(
    const ores::refdata::messaging::get_monetary_nature_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_monetary_nature_version_response
wire_traits<ores::refdata::messaging::get_monetary_nature_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_monetary_nature_version_response>(
        data);
}

}
