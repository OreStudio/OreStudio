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
#include "ores.analytics.api/domain/stress_test_shift_codec.hpp"
#include "ores.analytics.api/messaging/stress_test_shift_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::analytics::messaging::stress_test_shift_key>::write_json(
    const ores::analytics::messaging::stress_test_shift_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_key
wire_traits<ores::analytics::messaging::stress_test_shift_key>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_key>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_key>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_key
wire_traits<ores::analytics::messaging::stress_test_shift_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_key>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_write>::write_json(
    const ores::analytics::messaging::stress_test_shift_write& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_write
wire_traits<ores::analytics::messaging::stress_test_shift_write>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_write>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_write>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_write& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_write
wire_traits<ores::analytics::messaging::stress_test_shift_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_write>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_change>::write_json(
    const ores::analytics::messaging::stress_test_shift_change& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_change
wire_traits<ores::analytics::messaging::stress_test_shift_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_change>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_change>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_change& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_change
wire_traits<ores::analytics::messaging::stress_test_shift_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_change>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_removal>::write_json(
    const ores::analytics::messaging::stress_test_shift_removal& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_removal
wire_traits<ores::analytics::messaging::stress_test_shift_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_removal>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_removal>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_removal& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_removal
wire_traits<ores::analytics::messaging::stress_test_shift_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_removal>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_lookup>::write_json(
    const ores::analytics::messaging::stress_test_shift_lookup& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_lookup
wire_traits<ores::analytics::messaging::stress_test_shift_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_lookup>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_lookup>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_lookup& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_lookup
wire_traits<ores::analytics::messaging::stress_test_shift_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_lookup>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shifts_filter>::write_json(
    const ores::analytics::messaging::stress_test_shifts_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shifts_filter
wire_traits<ores::analytics::messaging::stress_test_shifts_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shifts_filter>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shifts_filter>::write_msgpack(
    const ores::analytics::messaging::stress_test_shifts_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shifts_filter
wire_traits<ores::analytics::messaging::stress_test_shifts_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shifts_filter>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_event>::write_json(
    const ores::analytics::messaging::stress_test_shift_event& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_event
wire_traits<ores::analytics::messaging::stress_test_shift_event>::read_json(std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_event>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_event>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_event& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_event
wire_traits<ores::analytics::messaging::stress_test_shift_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_event>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_version_key>::write_json(
    const ores::analytics::messaging::stress_test_shift_version_key& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_version_key
wire_traits<ores::analytics::messaging::stress_test_shift_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_version_key>(text);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_version_key>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_version_key& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_version_key
wire_traits<ores::analytics::messaging::stress_test_shift_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_version_key>(data);
}

std::string wire_traits<ores::analytics::messaging::stress_test_shift_versions_filter>::write_json(
    const ores::analytics::messaging::stress_test_shift_versions_filter& v) {
    return object::write_json(v);
}

ores::analytics::messaging::stress_test_shift_versions_filter
wire_traits<ores::analytics::messaging::stress_test_shift_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::stress_test_shift_versions_filter>(text);
}

std::string
wire_traits<ores::analytics::messaging::stress_test_shift_versions_filter>::write_msgpack(
    const ores::analytics::messaging::stress_test_shift_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::stress_test_shift_versions_filter
wire_traits<ores::analytics::messaging::stress_test_shift_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::stress_test_shift_versions_filter>(
        data);
}

std::string wire_traits<ores::analytics::messaging::list_stress_test_shifts_request>::write_json(
    const ores::analytics::messaging::list_stress_test_shifts_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_stress_test_shifts_request
wire_traits<ores::analytics::messaging::list_stress_test_shifts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_stress_test_shifts_request>(text);
}

std::string wire_traits<ores::analytics::messaging::list_stress_test_shifts_request>::write_msgpack(
    const ores::analytics::messaging::list_stress_test_shifts_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_stress_test_shifts_request
wire_traits<ores::analytics::messaging::list_stress_test_shifts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_stress_test_shifts_request>(data);
}

std::string wire_traits<ores::analytics::messaging::list_stress_test_shifts_response>::write_json(
    const ores::analytics::messaging::list_stress_test_shifts_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_stress_test_shifts_response
wire_traits<ores::analytics::messaging::list_stress_test_shifts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_stress_test_shifts_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::list_stress_test_shifts_response>::write_msgpack(
    const ores::analytics::messaging::list_stress_test_shifts_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_stress_test_shifts_response
wire_traits<ores::analytics::messaging::list_stress_test_shifts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::list_stress_test_shifts_response>(data);
}

std::string wire_traits<ores::analytics::messaging::get_stress_test_shift_request>::write_json(
    const ores::analytics::messaging::get_stress_test_shift_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_stress_test_shift_request
wire_traits<ores::analytics::messaging::get_stress_test_shift_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_stress_test_shift_request>(text);
}

std::string wire_traits<ores::analytics::messaging::get_stress_test_shift_request>::write_msgpack(
    const ores::analytics::messaging::get_stress_test_shift_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_stress_test_shift_request
wire_traits<ores::analytics::messaging::get_stress_test_shift_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_stress_test_shift_request>(data);
}

std::string wire_traits<ores::analytics::messaging::get_stress_test_shift_response>::write_json(
    const ores::analytics::messaging::get_stress_test_shift_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_stress_test_shift_response
wire_traits<ores::analytics::messaging::get_stress_test_shift_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_stress_test_shift_response>(text);
}

std::string wire_traits<ores::analytics::messaging::get_stress_test_shift_response>::write_msgpack(
    const ores::analytics::messaging::get_stress_test_shift_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_stress_test_shift_response
wire_traits<ores::analytics::messaging::get_stress_test_shift_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_stress_test_shift_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_request>::write_json(
    const ores::analytics::messaging::get_many_stress_test_shifts_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_many_stress_test_shifts_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_request>::write_msgpack(
    const ores::analytics::messaging::get_many_stress_test_shifts_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_many_stress_test_shifts_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_response>::write_json(
    const ores::analytics::messaging::get_many_stress_test_shifts_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_many_stress_test_shifts_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_response>::write_msgpack(
    const ores::analytics::messaging::get_many_stress_test_shifts_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::get_many_stress_test_shifts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_many_stress_test_shifts_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::put_stress_test_shift_request>::write_json(
    const ores::analytics::messaging::put_stress_test_shift_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_stress_test_shift_request
wire_traits<ores::analytics::messaging::put_stress_test_shift_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_stress_test_shift_request>(text);
}

std::string wire_traits<ores::analytics::messaging::put_stress_test_shift_request>::write_msgpack(
    const ores::analytics::messaging::put_stress_test_shift_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_stress_test_shift_request
wire_traits<ores::analytics::messaging::put_stress_test_shift_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_stress_test_shift_request>(data);
}

std::string wire_traits<ores::analytics::messaging::put_stress_test_shift_response>::write_json(
    const ores::analytics::messaging::put_stress_test_shift_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_stress_test_shift_response
wire_traits<ores::analytics::messaging::put_stress_test_shift_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_stress_test_shift_response>(text);
}

std::string wire_traits<ores::analytics::messaging::put_stress_test_shift_response>::write_msgpack(
    const ores::analytics::messaging::put_stress_test_shift_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_stress_test_shift_response
wire_traits<ores::analytics::messaging::put_stress_test_shift_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_stress_test_shift_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_request>::write_json(
    const ores::analytics::messaging::put_many_stress_test_shifts_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_many_stress_test_shifts_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_request>::write_msgpack(
    const ores::analytics::messaging::put_many_stress_test_shifts_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_many_stress_test_shifts_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_response>::write_json(
    const ores::analytics::messaging::put_many_stress_test_shifts_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::put_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::put_many_stress_test_shifts_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_response>::write_msgpack(
    const ores::analytics::messaging::put_many_stress_test_shifts_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::put_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::put_many_stress_test_shifts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::put_many_stress_test_shifts_response>(
        data);
}

std::string wire_traits<ores::analytics::messaging::delete_stress_test_shift_request>::write_json(
    const ores::analytics::messaging::delete_stress_test_shift_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_stress_test_shift_request
wire_traits<ores::analytics::messaging::delete_stress_test_shift_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_stress_test_shift_request>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_stress_test_shift_request>::write_msgpack(
    const ores::analytics::messaging::delete_stress_test_shift_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_stress_test_shift_request
wire_traits<ores::analytics::messaging::delete_stress_test_shift_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_stress_test_shift_request>(data);
}

std::string wire_traits<ores::analytics::messaging::delete_stress_test_shift_response>::write_json(
    const ores::analytics::messaging::delete_stress_test_shift_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_stress_test_shift_response
wire_traits<ores::analytics::messaging::delete_stress_test_shift_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_stress_test_shift_response>(text);
}

std::string
wire_traits<ores::analytics::messaging::delete_stress_test_shift_response>::write_msgpack(
    const ores::analytics::messaging::delete_stress_test_shift_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_stress_test_shift_response
wire_traits<ores::analytics::messaging::delete_stress_test_shift_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_stress_test_shift_response>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_request>::write_json(
    const ores::analytics::messaging::delete_many_stress_test_shifts_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_many_stress_test_shifts_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_request>::write_msgpack(
    const ores::analytics::messaging::delete_many_stress_test_shifts_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_stress_test_shifts_request
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::delete_many_stress_test_shifts_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_response>::write_json(
    const ores::analytics::messaging::delete_many_stress_test_shifts_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::delete_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::delete_many_stress_test_shifts_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_response>::write_msgpack(
    const ores::analytics::messaging::delete_many_stress_test_shifts_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::delete_many_stress_test_shifts_response
wire_traits<ores::analytics::messaging::delete_many_stress_test_shifts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::delete_many_stress_test_shifts_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_request>::write_json(
    const ores::analytics::messaging::list_stress_test_shift_versions_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_stress_test_shift_versions_request
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_stress_test_shift_versions_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_request>::write_msgpack(
    const ores::analytics::messaging::list_stress_test_shift_versions_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_stress_test_shift_versions_request
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_stress_test_shift_versions_request>(data);
}

std::string
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_response>::write_json(
    const ores::analytics::messaging::list_stress_test_shift_versions_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::list_stress_test_shift_versions_response
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::list_stress_test_shift_versions_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_response>::write_msgpack(
    const ores::analytics::messaging::list_stress_test_shift_versions_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::list_stress_test_shift_versions_response
wire_traits<ores::analytics::messaging::list_stress_test_shift_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::analytics::messaging::list_stress_test_shift_versions_response>(data);
}

std::string
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_request>::write_json(
    const ores::analytics::messaging::get_stress_test_shift_version_request& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_stress_test_shift_version_request
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_stress_test_shift_version_request>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_request>::write_msgpack(
    const ores::analytics::messaging::get_stress_test_shift_version_request& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_stress_test_shift_version_request
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_stress_test_shift_version_request>(
        data);
}

std::string
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_response>::write_json(
    const ores::analytics::messaging::get_stress_test_shift_version_response& v) {
    return object::write_json(v);
}

ores::analytics::messaging::get_stress_test_shift_version_response
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::analytics::messaging::get_stress_test_shift_version_response>(
        text);
}

std::string
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_response>::write_msgpack(
    const ores::analytics::messaging::get_stress_test_shift_version_response& v) {
    return object::write_msgpack(v);
}

ores::analytics::messaging::get_stress_test_shift_version_response
wire_traits<ores::analytics::messaging::get_stress_test_shift_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::analytics::messaging::get_stress_test_shift_version_response>(
        data);
}

}
