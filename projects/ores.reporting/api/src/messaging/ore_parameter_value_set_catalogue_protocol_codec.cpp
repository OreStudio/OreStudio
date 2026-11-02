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
#include "ores.reporting.api/domain/ore_parameter_value_set_catalogue_codec.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_catalogue_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_key
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_key
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::ore_parameter_value_set_catalogue_key>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_write>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_write
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_write>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_write>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_write
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_write>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_change>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_change
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_change>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_change>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_change
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_change>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_removal
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_removal
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_removal>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_lookup>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogues_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogues_filter
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogues_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogues_filter
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogues_filter>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_event>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_event
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::ore_parameter_value_set_catalogue_event>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_event>::write_msgpack(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_event
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_event>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>::write_json(
    const ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>(text);
}

std::string wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>::
    write_msgpack(
        const ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_version_key>(data);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>::
    write_json(
        const ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>(text);
}

std::string
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>::
    write_msgpack(
        const ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter
wire_traits<ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::ore_parameter_value_set_catalogue_versions_filter>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>::
    write_json(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>::
    write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogues_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>::
    write_json(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>::
    write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogues_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>::write_json(
    const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>::
    write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>::write_json(
    const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>::
    write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>::
    write_json(
        const ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>::
    write_msgpack(
        const ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>::
    write_json(
        const ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>::
    write_msgpack(
        const ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_many_ore_parameter_value_set_catalogues_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>::write_json(
    const ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>::
    write_msgpack(
        const ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::put_ore_parameter_value_set_catalogue_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>::write_json(
    const ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>::
    write_msgpack(
        const ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::put_ore_parameter_value_set_catalogue_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>::
    write_json(
        const ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>::
    write_msgpack(
        const ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>::
    write_json(
        const ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>::
    write_msgpack(
        const ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::put_many_ore_parameter_value_set_catalogues_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>::
    write_json(
        const ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>::
    write_msgpack(
        const ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>::
    write_json(
        const ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>::
    write_msgpack(
        const ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response
wire_traits<ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::delete_ore_parameter_value_set_catalogue_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>::
    write_json(
        const ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>::
    write_msgpack(
        const ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>::
    write_json(
        const ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>::
    write_msgpack(
        const ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response
wire_traits<ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::delete_many_ore_parameter_value_set_catalogues_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>::
    write_json(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>::
    write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>::
    write_json(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>::
    write_msgpack(
        const ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response
wire_traits<ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::list_ore_parameter_value_set_catalogue_versions_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>::
    write_json(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>::
    write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_request>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>::
    write_json(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response&
            v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>::
    write_msgpack(
        const ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response&
            v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response
wire_traits<ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::get_ore_parameter_value_set_catalogue_version_response>(data);
}

}
