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
#include "ores.synthetic.api/domain/calibration_policy_codec.hpp"
#include "ores.synthetic.api/messaging/calibration_policy_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::calibration_policy_key>::write_json(
    const ores::synthetic::messaging::calibration_policy_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_key
wire_traits<ores::synthetic::messaging::calibration_policy_key>::read_json(std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_key>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_key
wire_traits<ores::synthetic::messaging::calibration_policy_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_write>::write_json(
    const ores::synthetic::messaging::calibration_policy_write& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_write
wire_traits<ores::synthetic::messaging::calibration_policy_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_write>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_write>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_write& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_write
wire_traits<ores::synthetic::messaging::calibration_policy_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_write>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_change>::write_json(
    const ores::synthetic::messaging::calibration_policy_change& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_change
wire_traits<ores::synthetic::messaging::calibration_policy_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_change>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_change>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_change& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_change
wire_traits<ores::synthetic::messaging::calibration_policy_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_change>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_removal>::write_json(
    const ores::synthetic::messaging::calibration_policy_removal& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_removal
wire_traits<ores::synthetic::messaging::calibration_policy_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_removal>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_removal>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_removal& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_removal
wire_traits<ores::synthetic::messaging::calibration_policy_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_removal>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_lookup>::write_json(
    const ores::synthetic::messaging::calibration_policy_lookup& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_lookup
wire_traits<ores::synthetic::messaging::calibration_policy_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_lookup>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_lookup>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_lookup& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_lookup
wire_traits<ores::synthetic::messaging::calibration_policy_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_lookup>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policies_filter>::write_json(
    const ores::synthetic::messaging::calibration_policies_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policies_filter
wire_traits<ores::synthetic::messaging::calibration_policies_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policies_filter>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policies_filter>::write_msgpack(
    const ores::synthetic::messaging::calibration_policies_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policies_filter
wire_traits<ores::synthetic::messaging::calibration_policies_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policies_filter>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_event>::write_json(
    const ores::synthetic::messaging::calibration_policy_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_event
wire_traits<ores::synthetic::messaging::calibration_policy_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_event>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_event>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_event
wire_traits<ores::synthetic::messaging::calibration_policy_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_event>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_version_key>::write_json(
    const ores::synthetic::messaging::calibration_policy_version_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_version_key
wire_traits<ores::synthetic::messaging::calibration_policy_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_version_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_version_key>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_version_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_version_key
wire_traits<ores::synthetic::messaging::calibration_policy_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_version_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::calibration_policy_versions_filter>::write_json(
    const ores::synthetic::messaging::calibration_policy_versions_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::calibration_policy_versions_filter
wire_traits<ores::synthetic::messaging::calibration_policy_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::calibration_policy_versions_filter>(text);
}

std::string
wire_traits<ores::synthetic::messaging::calibration_policy_versions_filter>::write_msgpack(
    const ores::synthetic::messaging::calibration_policy_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::calibration_policy_versions_filter
wire_traits<ores::synthetic::messaging::calibration_policy_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::calibration_policy_versions_filter>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::list_calibration_policies_request>::write_json(
    const ores::synthetic::messaging::list_calibration_policies_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_calibration_policies_request
wire_traits<ores::synthetic::messaging::list_calibration_policies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_calibration_policies_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policies_request>::write_msgpack(
    const ores::synthetic::messaging::list_calibration_policies_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_calibration_policies_request
wire_traits<ores::synthetic::messaging::list_calibration_policies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_calibration_policies_request>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::list_calibration_policies_response>::write_json(
    const ores::synthetic::messaging::list_calibration_policies_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_calibration_policies_response
wire_traits<ores::synthetic::messaging::list_calibration_policies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_calibration_policies_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policies_response>::write_msgpack(
    const ores::synthetic::messaging::list_calibration_policies_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_calibration_policies_response
wire_traits<ores::synthetic::messaging::list_calibration_policies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::list_calibration_policies_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::get_calibration_policy_request>::write_json(
    const ores::synthetic::messaging::get_calibration_policy_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_calibration_policy_request
wire_traits<ores::synthetic::messaging::get_calibration_policy_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_calibration_policy_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_calibration_policy_request>::write_msgpack(
    const ores::synthetic::messaging::get_calibration_policy_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_calibration_policy_request
wire_traits<ores::synthetic::messaging::get_calibration_policy_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_calibration_policy_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::get_calibration_policy_response>::write_json(
    const ores::synthetic::messaging::get_calibration_policy_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_calibration_policy_response
wire_traits<ores::synthetic::messaging::get_calibration_policy_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_calibration_policy_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_calibration_policy_response>::write_msgpack(
    const ores::synthetic::messaging::get_calibration_policy_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_calibration_policy_response
wire_traits<ores::synthetic::messaging::get_calibration_policy_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_calibration_policy_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_request>::write_json(
    const ores::synthetic::messaging::get_many_calibration_policies_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_calibration_policies_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_request>::write_msgpack(
    const ores::synthetic::messaging::get_many_calibration_policies_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_calibration_policies_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_response>::write_json(
    const ores::synthetic::messaging::get_many_calibration_policies_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_many_calibration_policies_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_response>::write_msgpack(
    const ores::synthetic::messaging::get_many_calibration_policies_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::get_many_calibration_policies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_many_calibration_policies_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::put_calibration_policy_request>::write_json(
    const ores::synthetic::messaging::put_calibration_policy_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_calibration_policy_request
wire_traits<ores::synthetic::messaging::put_calibration_policy_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_calibration_policy_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_calibration_policy_request>::write_msgpack(
    const ores::synthetic::messaging::put_calibration_policy_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_calibration_policy_request
wire_traits<ores::synthetic::messaging::put_calibration_policy_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_calibration_policy_request>(data);
}

std::string wire_traits<ores::synthetic::messaging::put_calibration_policy_response>::write_json(
    const ores::synthetic::messaging::put_calibration_policy_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_calibration_policy_response
wire_traits<ores::synthetic::messaging::put_calibration_policy_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_calibration_policy_response>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_calibration_policy_response>::write_msgpack(
    const ores::synthetic::messaging::put_calibration_policy_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_calibration_policy_response
wire_traits<ores::synthetic::messaging::put_calibration_policy_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_calibration_policy_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_request>::write_json(
    const ores::synthetic::messaging::put_many_calibration_policies_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_calibration_policies_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_request>::write_msgpack(
    const ores::synthetic::messaging::put_many_calibration_policies_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_calibration_policies_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_response>::write_json(
    const ores::synthetic::messaging::put_many_calibration_policies_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_many_calibration_policies_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_response>::write_msgpack(
    const ores::synthetic::messaging::put_many_calibration_policies_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::put_many_calibration_policies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_many_calibration_policies_response>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::delete_calibration_policy_request>::write_json(
    const ores::synthetic::messaging::delete_calibration_policy_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_calibration_policy_request
wire_traits<ores::synthetic::messaging::delete_calibration_policy_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_calibration_policy_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_calibration_policy_request>::write_msgpack(
    const ores::synthetic::messaging::delete_calibration_policy_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_calibration_policy_request
wire_traits<ores::synthetic::messaging::delete_calibration_policy_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_calibration_policy_request>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::delete_calibration_policy_response>::write_json(
    const ores::synthetic::messaging::delete_calibration_policy_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_calibration_policy_response
wire_traits<ores::synthetic::messaging::delete_calibration_policy_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_calibration_policy_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_calibration_policy_response>::write_msgpack(
    const ores::synthetic::messaging::delete_calibration_policy_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_calibration_policy_response
wire_traits<ores::synthetic::messaging::delete_calibration_policy_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::delete_calibration_policy_response>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_request>::write_json(
    const ores::synthetic::messaging::delete_many_calibration_policies_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_calibration_policies_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_request>::write_msgpack(
    const ores::synthetic::messaging::delete_many_calibration_policies_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_calibration_policies_request
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_calibration_policies_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_response>::write_json(
    const ores::synthetic::messaging::delete_many_calibration_policies_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_many_calibration_policies_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_response>::write_msgpack(
    const ores::synthetic::messaging::delete_many_calibration_policies_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_calibration_policies_response
wire_traits<ores::synthetic::messaging::delete_many_calibration_policies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_calibration_policies_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_request>::write_json(
    const ores::synthetic::messaging::list_calibration_policy_versions_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_calibration_policy_versions_request
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_calibration_policy_versions_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_request>::write_msgpack(
    const ores::synthetic::messaging::list_calibration_policy_versions_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_calibration_policy_versions_request
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_calibration_policy_versions_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_response>::write_json(
    const ores::synthetic::messaging::list_calibration_policy_versions_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_calibration_policy_versions_response
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_calibration_policy_versions_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_response>::write_msgpack(
    const ores::synthetic::messaging::list_calibration_policy_versions_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_calibration_policy_versions_response
wire_traits<ores::synthetic::messaging::list_calibration_policy_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_calibration_policy_versions_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_request>::write_json(
    const ores::synthetic::messaging::get_calibration_policy_version_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_calibration_policy_version_request
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_calibration_policy_version_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_request>::write_msgpack(
    const ores::synthetic::messaging::get_calibration_policy_version_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_calibration_policy_version_request
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_calibration_policy_version_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_response>::write_json(
    const ores::synthetic::messaging::get_calibration_policy_version_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_calibration_policy_version_response
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_calibration_policy_version_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_response>::write_msgpack(
    const ores::synthetic::messaging::get_calibration_policy_version_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_calibration_policy_version_response
wire_traits<ores::synthetic::messaging::get_calibration_policy_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_calibration_policy_version_response>(data);
}

}
