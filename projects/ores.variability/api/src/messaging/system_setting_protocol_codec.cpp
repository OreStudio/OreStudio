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
#include "ores.variability.api/domain/system_setting_codec.hpp"
#include "ores.variability.api/messaging/system_setting_protocol.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::variability::messaging::system_setting_key>::write_json(
    const ores::variability::messaging::system_setting_key& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_key
wire_traits<ores::variability::messaging::system_setting_key>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_key>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_key>::write_msgpack(
    const ores::variability::messaging::system_setting_key& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_key
wire_traits<ores::variability::messaging::system_setting_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_key>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_write>::write_json(
    const ores::variability::messaging::system_setting_write& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_write
wire_traits<ores::variability::messaging::system_setting_write>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_write>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_write>::write_msgpack(
    const ores::variability::messaging::system_setting_write& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_write
wire_traits<ores::variability::messaging::system_setting_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_write>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_change>::write_json(
    const ores::variability::messaging::system_setting_change& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_change
wire_traits<ores::variability::messaging::system_setting_change>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_change>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_change>::write_msgpack(
    const ores::variability::messaging::system_setting_change& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_change
wire_traits<ores::variability::messaging::system_setting_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_change>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_removal>::write_json(
    const ores::variability::messaging::system_setting_removal& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_removal
wire_traits<ores::variability::messaging::system_setting_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_removal>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_removal>::write_msgpack(
    const ores::variability::messaging::system_setting_removal& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_removal
wire_traits<ores::variability::messaging::system_setting_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_removal>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_lookup>::write_json(
    const ores::variability::messaging::system_setting_lookup& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_lookup
wire_traits<ores::variability::messaging::system_setting_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_lookup>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_lookup>::write_msgpack(
    const ores::variability::messaging::system_setting_lookup& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_lookup
wire_traits<ores::variability::messaging::system_setting_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_lookup>(data);
}

std::string wire_traits<ores::variability::messaging::system_settings_filter>::write_json(
    const ores::variability::messaging::system_settings_filter& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_settings_filter
wire_traits<ores::variability::messaging::system_settings_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::system_settings_filter>(text);
}

std::string wire_traits<ores::variability::messaging::system_settings_filter>::write_msgpack(
    const ores::variability::messaging::system_settings_filter& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_settings_filter
wire_traits<ores::variability::messaging::system_settings_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_settings_filter>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_event>::write_json(
    const ores::variability::messaging::system_setting_event& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_event
wire_traits<ores::variability::messaging::system_setting_event>::read_json(std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_event>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_event>::write_msgpack(
    const ores::variability::messaging::system_setting_event& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_event
wire_traits<ores::variability::messaging::system_setting_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_event>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_version_key>::write_json(
    const ores::variability::messaging::system_setting_version_key& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_version_key
wire_traits<ores::variability::messaging::system_setting_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_version_key>(text);
}

std::string wire_traits<ores::variability::messaging::system_setting_version_key>::write_msgpack(
    const ores::variability::messaging::system_setting_version_key& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_version_key
wire_traits<ores::variability::messaging::system_setting_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_version_key>(data);
}

std::string wire_traits<ores::variability::messaging::system_setting_versions_filter>::write_json(
    const ores::variability::messaging::system_setting_versions_filter& v) {
    return object::write_json(v);
}

ores::variability::messaging::system_setting_versions_filter
wire_traits<ores::variability::messaging::system_setting_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::system_setting_versions_filter>(text);
}

std::string
wire_traits<ores::variability::messaging::system_setting_versions_filter>::write_msgpack(
    const ores::variability::messaging::system_setting_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::system_setting_versions_filter
wire_traits<ores::variability::messaging::system_setting_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::system_setting_versions_filter>(data);
}

std::string wire_traits<ores::variability::messaging::list_system_settings_request>::write_json(
    const ores::variability::messaging::list_system_settings_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::list_system_settings_request
wire_traits<ores::variability::messaging::list_system_settings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::list_system_settings_request>(text);
}

std::string wire_traits<ores::variability::messaging::list_system_settings_request>::write_msgpack(
    const ores::variability::messaging::list_system_settings_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::list_system_settings_request
wire_traits<ores::variability::messaging::list_system_settings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::list_system_settings_request>(data);
}

std::string wire_traits<ores::variability::messaging::list_system_settings_response>::write_json(
    const ores::variability::messaging::list_system_settings_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::list_system_settings_response
wire_traits<ores::variability::messaging::list_system_settings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::list_system_settings_response>(text);
}

std::string wire_traits<ores::variability::messaging::list_system_settings_response>::write_msgpack(
    const ores::variability::messaging::list_system_settings_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::list_system_settings_response
wire_traits<ores::variability::messaging::list_system_settings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::list_system_settings_response>(data);
}

std::string wire_traits<ores::variability::messaging::get_system_setting_request>::write_json(
    const ores::variability::messaging::get_system_setting_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_system_setting_request
wire_traits<ores::variability::messaging::get_system_setting_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_system_setting_request>(text);
}

std::string wire_traits<ores::variability::messaging::get_system_setting_request>::write_msgpack(
    const ores::variability::messaging::get_system_setting_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_system_setting_request
wire_traits<ores::variability::messaging::get_system_setting_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_system_setting_request>(data);
}

std::string wire_traits<ores::variability::messaging::get_system_setting_response>::write_json(
    const ores::variability::messaging::get_system_setting_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_system_setting_response
wire_traits<ores::variability::messaging::get_system_setting_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_system_setting_response>(text);
}

std::string wire_traits<ores::variability::messaging::get_system_setting_response>::write_msgpack(
    const ores::variability::messaging::get_system_setting_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_system_setting_response
wire_traits<ores::variability::messaging::get_system_setting_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_system_setting_response>(data);
}

std::string wire_traits<ores::variability::messaging::get_many_system_settings_request>::write_json(
    const ores::variability::messaging::get_many_system_settings_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_many_system_settings_request
wire_traits<ores::variability::messaging::get_many_system_settings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_many_system_settings_request>(text);
}

std::string
wire_traits<ores::variability::messaging::get_many_system_settings_request>::write_msgpack(
    const ores::variability::messaging::get_many_system_settings_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_many_system_settings_request
wire_traits<ores::variability::messaging::get_many_system_settings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_many_system_settings_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::get_many_system_settings_response>::write_json(
    const ores::variability::messaging::get_many_system_settings_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_many_system_settings_response
wire_traits<ores::variability::messaging::get_many_system_settings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_many_system_settings_response>(text);
}

std::string
wire_traits<ores::variability::messaging::get_many_system_settings_response>::write_msgpack(
    const ores::variability::messaging::get_many_system_settings_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_many_system_settings_response
wire_traits<ores::variability::messaging::get_many_system_settings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_many_system_settings_response>(
        data);
}

std::string wire_traits<ores::variability::messaging::put_system_setting_request>::write_json(
    const ores::variability::messaging::put_system_setting_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::put_system_setting_request
wire_traits<ores::variability::messaging::put_system_setting_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::put_system_setting_request>(text);
}

std::string wire_traits<ores::variability::messaging::put_system_setting_request>::write_msgpack(
    const ores::variability::messaging::put_system_setting_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::put_system_setting_request
wire_traits<ores::variability::messaging::put_system_setting_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::put_system_setting_request>(data);
}

std::string wire_traits<ores::variability::messaging::put_system_setting_response>::write_json(
    const ores::variability::messaging::put_system_setting_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::put_system_setting_response
wire_traits<ores::variability::messaging::put_system_setting_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::put_system_setting_response>(text);
}

std::string wire_traits<ores::variability::messaging::put_system_setting_response>::write_msgpack(
    const ores::variability::messaging::put_system_setting_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::put_system_setting_response
wire_traits<ores::variability::messaging::put_system_setting_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::put_system_setting_response>(data);
}

std::string wire_traits<ores::variability::messaging::put_many_system_settings_request>::write_json(
    const ores::variability::messaging::put_many_system_settings_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::put_many_system_settings_request
wire_traits<ores::variability::messaging::put_many_system_settings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::put_many_system_settings_request>(text);
}

std::string
wire_traits<ores::variability::messaging::put_many_system_settings_request>::write_msgpack(
    const ores::variability::messaging::put_many_system_settings_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::put_many_system_settings_request
wire_traits<ores::variability::messaging::put_many_system_settings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::put_many_system_settings_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::put_many_system_settings_response>::write_json(
    const ores::variability::messaging::put_many_system_settings_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::put_many_system_settings_response
wire_traits<ores::variability::messaging::put_many_system_settings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::put_many_system_settings_response>(text);
}

std::string
wire_traits<ores::variability::messaging::put_many_system_settings_response>::write_msgpack(
    const ores::variability::messaging::put_many_system_settings_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::put_many_system_settings_response
wire_traits<ores::variability::messaging::put_many_system_settings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::put_many_system_settings_response>(
        data);
}

std::string wire_traits<ores::variability::messaging::delete_system_setting_request>::write_json(
    const ores::variability::messaging::delete_system_setting_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::delete_system_setting_request
wire_traits<ores::variability::messaging::delete_system_setting_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::delete_system_setting_request>(text);
}

std::string wire_traits<ores::variability::messaging::delete_system_setting_request>::write_msgpack(
    const ores::variability::messaging::delete_system_setting_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::delete_system_setting_request
wire_traits<ores::variability::messaging::delete_system_setting_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::delete_system_setting_request>(data);
}

std::string wire_traits<ores::variability::messaging::delete_system_setting_response>::write_json(
    const ores::variability::messaging::delete_system_setting_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::delete_system_setting_response
wire_traits<ores::variability::messaging::delete_system_setting_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::delete_system_setting_response>(text);
}

std::string
wire_traits<ores::variability::messaging::delete_system_setting_response>::write_msgpack(
    const ores::variability::messaging::delete_system_setting_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::delete_system_setting_response
wire_traits<ores::variability::messaging::delete_system_setting_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::delete_system_setting_response>(data);
}

std::string
wire_traits<ores::variability::messaging::delete_many_system_settings_request>::write_json(
    const ores::variability::messaging::delete_many_system_settings_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::delete_many_system_settings_request
wire_traits<ores::variability::messaging::delete_many_system_settings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::delete_many_system_settings_request>(
        text);
}

std::string
wire_traits<ores::variability::messaging::delete_many_system_settings_request>::write_msgpack(
    const ores::variability::messaging::delete_many_system_settings_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::delete_many_system_settings_request
wire_traits<ores::variability::messaging::delete_many_system_settings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::delete_many_system_settings_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::delete_many_system_settings_response>::write_json(
    const ores::variability::messaging::delete_many_system_settings_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::delete_many_system_settings_response
wire_traits<ores::variability::messaging::delete_many_system_settings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::delete_many_system_settings_response>(
        text);
}

std::string
wire_traits<ores::variability::messaging::delete_many_system_settings_response>::write_msgpack(
    const ores::variability::messaging::delete_many_system_settings_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::delete_many_system_settings_response
wire_traits<ores::variability::messaging::delete_many_system_settings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::delete_many_system_settings_response>(
        data);
}

std::string
wire_traits<ores::variability::messaging::list_system_setting_versions_request>::write_json(
    const ores::variability::messaging::list_system_setting_versions_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::list_system_setting_versions_request
wire_traits<ores::variability::messaging::list_system_setting_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::list_system_setting_versions_request>(
        text);
}

std::string
wire_traits<ores::variability::messaging::list_system_setting_versions_request>::write_msgpack(
    const ores::variability::messaging::list_system_setting_versions_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::list_system_setting_versions_request
wire_traits<ores::variability::messaging::list_system_setting_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::list_system_setting_versions_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::list_system_setting_versions_response>::write_json(
    const ores::variability::messaging::list_system_setting_versions_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::list_system_setting_versions_response
wire_traits<ores::variability::messaging::list_system_setting_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::list_system_setting_versions_response>(
        text);
}

std::string
wire_traits<ores::variability::messaging::list_system_setting_versions_response>::write_msgpack(
    const ores::variability::messaging::list_system_setting_versions_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::list_system_setting_versions_response
wire_traits<ores::variability::messaging::list_system_setting_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::variability::messaging::list_system_setting_versions_response>(data);
}

std::string
wire_traits<ores::variability::messaging::get_system_setting_version_request>::write_json(
    const ores::variability::messaging::get_system_setting_version_request& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_system_setting_version_request
wire_traits<ores::variability::messaging::get_system_setting_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_system_setting_version_request>(
        text);
}

std::string
wire_traits<ores::variability::messaging::get_system_setting_version_request>::write_msgpack(
    const ores::variability::messaging::get_system_setting_version_request& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_system_setting_version_request
wire_traits<ores::variability::messaging::get_system_setting_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_system_setting_version_request>(
        data);
}

std::string
wire_traits<ores::variability::messaging::get_system_setting_version_response>::write_json(
    const ores::variability::messaging::get_system_setting_version_response& v) {
    return object::write_json(v);
}

ores::variability::messaging::get_system_setting_version_response
wire_traits<ores::variability::messaging::get_system_setting_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::variability::messaging::get_system_setting_version_response>(
        text);
}

std::string
wire_traits<ores::variability::messaging::get_system_setting_version_response>::write_msgpack(
    const ores::variability::messaging::get_system_setting_version_response& v) {
    return object::write_msgpack(v);
}

ores::variability::messaging::get_system_setting_version_response
wire_traits<ores::variability::messaging::get_system_setting_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::variability::messaging::get_system_setting_version_response>(
        data);
}

}
