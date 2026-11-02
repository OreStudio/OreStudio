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
#include "ores.analytics.api/domain/todays_market_configuration_binding_codec.hpp"
#include "ores.analytics.api/messaging/todays_market_configuration_binding_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_key>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_key
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::todays_market_configuration_binding_key>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_key>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_key
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_key>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_write>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_write& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_write
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::todays_market_configuration_binding_write>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_write>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_write& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_write
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_write>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_change>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_change& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_change
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_change>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_binding_change>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_change>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_change& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_change
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_change>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_removal>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_removal& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_removal
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_removal>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_binding_removal>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_removal>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_removal& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_removal
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_removal>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_lookup>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_lookup& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_lookup
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_lookup>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_binding_lookup>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_lookup>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_lookup& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_lookup
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_lookup>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_bindings_filter>::write_json(
    const ores::analytics::messaging::todays_market_configuration_bindings_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_bindings_filter
wire_traits<ores::analytics::messaging::todays_market_configuration_bindings_filter>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_bindings_filter>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_bindings_filter>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_bindings_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_bindings_filter
wire_traits<ores::analytics::messaging::todays_market_configuration_bindings_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_bindings_filter>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_event>::write_json(
    const ores::analytics::messaging::todays_market_configuration_binding_event& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_event
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::todays_market_configuration_binding_event>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_event>::write_msgpack(
    const ores::analytics::messaging::todays_market_configuration_binding_event& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_event
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_event>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_version_key>::
    write_json(
        const ores::analytics::messaging::todays_market_configuration_binding_version_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_version_key
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_version_key>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_binding_version_key>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_version_key>::
    write_msgpack(
        const ores::analytics::messaging::todays_market_configuration_binding_version_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_version_key
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_version_key>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_version_key>(data);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_versions_filter>::
    write_json(
        const ores::analytics::messaging::todays_market_configuration_binding_versions_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::todays_market_configuration_binding_versions_filter
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_versions_filter>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::todays_market_configuration_binding_versions_filter>(text);
}

std::string
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_versions_filter>::
    write_msgpack(
        const ores::analytics::messaging::todays_market_configuration_binding_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::todays_market_configuration_binding_versions_filter
wire_traits<ores::analytics::messaging::todays_market_configuration_binding_versions_filter>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::todays_market_configuration_binding_versions_filter>(data);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_request>::
    write_json(
        const ores::analytics::messaging::list_todays_market_configuration_bindings_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::list_todays_market_configuration_bindings_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_request>::
    write_msgpack(
        const ores::analytics::messaging::list_todays_market_configuration_bindings_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_todays_market_configuration_bindings_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_response>::
    write_json(
        const ores::analytics::messaging::list_todays_market_configuration_bindings_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::list_todays_market_configuration_bindings_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_response>::
    write_msgpack(
        const ores::analytics::messaging::list_todays_market_configuration_bindings_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::list_todays_market_configuration_bindings_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_todays_market_configuration_bindings_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_request>::
    write_json(
        const ores::analytics::messaging::get_todays_market_configuration_binding_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_todays_market_configuration_binding_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_request>::
    write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_binding_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_todays_market_configuration_binding_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_response>::
    write_json(
        const ores::analytics::messaging::get_todays_market_configuration_binding_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_todays_market_configuration_binding_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_response>::
    write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_binding_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_todays_market_configuration_binding_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>::
    write_json(
        const ores::analytics::messaging::get_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>::
    write_msgpack(
        const ores::analytics::messaging::get_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_many_todays_market_configuration_bindings_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>::
    write_json(
        const ores::analytics::messaging::get_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>::
    write_msgpack(
        const ores::analytics::messaging::get_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_many_todays_market_configuration_bindings_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_request>::
    write_json(
        const ores::analytics::messaging::put_todays_market_configuration_binding_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::put_todays_market_configuration_binding_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_request>::
    write_msgpack(
        const ores::analytics::messaging::put_todays_market_configuration_binding_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::put_todays_market_configuration_binding_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_response>::
    write_json(
        const ores::analytics::messaging::put_todays_market_configuration_binding_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::put_todays_market_configuration_binding_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_response>::
    write_msgpack(
        const ores::analytics::messaging::put_todays_market_configuration_binding_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::put_todays_market_configuration_binding_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::put_todays_market_configuration_binding_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>::
    write_json(
        const ores::analytics::messaging::put_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>::
    write_msgpack(
        const ores::analytics::messaging::put_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::put_many_todays_market_configuration_bindings_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>::
    write_json(
        const ores::analytics::messaging::put_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>::
    write_msgpack(
        const ores::analytics::messaging::put_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::put_many_todays_market_configuration_bindings_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_request>::
    write_json(
        const ores::analytics::messaging::delete_todays_market_configuration_binding_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::delete_todays_market_configuration_binding_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_request>::
    write_msgpack(
        const ores::analytics::messaging::delete_todays_market_configuration_binding_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_todays_market_configuration_binding_request
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_todays_market_configuration_binding_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_response>::
    write_json(
        const ores::analytics::messaging::delete_todays_market_configuration_binding_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::delete_todays_market_configuration_binding_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_response>::
    write_msgpack(
        const ores::analytics::messaging::delete_todays_market_configuration_binding_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_todays_market_configuration_binding_response
wire_traits<ores::analytics::messaging::delete_todays_market_configuration_binding_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_todays_market_configuration_binding_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>::
    write_json(
        const ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>::
    write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_many_todays_market_configuration_bindings_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>::
    write_json(
        const ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>::
    write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response
wire_traits<ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_many_todays_market_configuration_bindings_response>(
        data);
}

std::string wire_traits<
    ores::analytics::messaging::
        list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>::
    write_json(
        const ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request&
                v) {
    return object::write_json(v);
}

ores::analytics::messaging::
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_request
    wire_traits<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>::
        read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>(
        text);
}

std::string wire_traits<
    ores::analytics::messaging::
        list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>::
    write_msgpack(
        const ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request&
                v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_request
    wire_traits<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>::
        read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_request>(
        data);
}

std::string wire_traits<
    ores::analytics::messaging::
        list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>::
    write_json(
        const ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response&
                v) {
    return object::write_json(v);
}

ores::analytics::messaging::
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_response
    wire_traits<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>::
        read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>(
        text);
}

std::string wire_traits<
    ores::analytics::messaging::
        list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>::
    write_msgpack(
        const ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response&
                v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::
    list_by_todays_market_configuration_id_todays_market_configuration_bindings_response
    wire_traits<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>::
        read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::
            list_by_todays_market_configuration_id_todays_market_configuration_bindings_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>::
    write_json(
        const ores::analytics::messaging::list_todays_market_configuration_binding_versions_request&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_todays_market_configuration_binding_versions_request
wire_traits<ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>::
    write_msgpack(
        const ores::analytics::messaging::list_todays_market_configuration_binding_versions_request&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_todays_market_configuration_binding_versions_request
wire_traits<ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_todays_market_configuration_binding_versions_request>(
        data);
}

std::string wire_traits<
    ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>::
    write_json(const ores::analytics::messaging::
                   list_todays_market_configuration_binding_versions_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_todays_market_configuration_binding_versions_response wire_traits<
    ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>(
        text);
}

std::string wire_traits<
    ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>::
    write_msgpack(const ores::analytics::messaging::
                      list_todays_market_configuration_binding_versions_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_todays_market_configuration_binding_versions_response wire_traits<
    ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_todays_market_configuration_binding_versions_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_request>::
    write_json(
        const ores::analytics::messaging::get_todays_market_configuration_binding_version_request&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_version_request
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_todays_market_configuration_binding_version_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_request>::
    write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_binding_version_request&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_version_request
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_todays_market_configuration_binding_version_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_response>::
    write_json(
        const ores::analytics::messaging::get_todays_market_configuration_binding_version_response&
            v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_version_response
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::analytics::messaging::get_todays_market_configuration_binding_version_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_response>::
    write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_binding_version_response&
            v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_todays_market_configuration_binding_version_response
wire_traits<ores::analytics::messaging::get_todays_market_configuration_binding_version_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::get_todays_market_configuration_binding_version_response>(data);
}

}
