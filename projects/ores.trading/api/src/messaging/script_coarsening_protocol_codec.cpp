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
#include "ores.trading.api/domain/script_coarsening_codec.hpp"
#include "ores.trading.api/messaging/script_coarsening_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::script_coarsening_key>::write_json(
    const ores::trading::messaging::script_coarsening_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_key
wire_traits<ores::trading::messaging::script_coarsening_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_key>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_key>::write_msgpack(
    const ores::trading::messaging::script_coarsening_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_key
wire_traits<ores::trading::messaging::script_coarsening_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_key>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_write>::write_json(
    const ores::trading::messaging::script_coarsening_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_write
wire_traits<ores::trading::messaging::script_coarsening_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_write>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_write>::write_msgpack(
    const ores::trading::messaging::script_coarsening_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_write
wire_traits<ores::trading::messaging::script_coarsening_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_write>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_change>::write_json(
    const ores::trading::messaging::script_coarsening_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_change
wire_traits<ores::trading::messaging::script_coarsening_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_change>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_change>::write_msgpack(
    const ores::trading::messaging::script_coarsening_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_change
wire_traits<ores::trading::messaging::script_coarsening_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_change>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_removal>::write_json(
    const ores::trading::messaging::script_coarsening_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_removal
wire_traits<ores::trading::messaging::script_coarsening_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_removal>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_removal>::write_msgpack(
    const ores::trading::messaging::script_coarsening_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_removal
wire_traits<ores::trading::messaging::script_coarsening_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_removal>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_lookup>::write_json(
    const ores::trading::messaging::script_coarsening_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_lookup
wire_traits<ores::trading::messaging::script_coarsening_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_lookup>::write_msgpack(
    const ores::trading::messaging::script_coarsening_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_lookup
wire_traits<ores::trading::messaging::script_coarsening_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_event>::write_json(
    const ores::trading::messaging::script_coarsening_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_event
wire_traits<ores::trading::messaging::script_coarsening_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_event>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_event>::write_msgpack(
    const ores::trading::messaging::script_coarsening_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_event
wire_traits<ores::trading::messaging::script_coarsening_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_event>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_version_key>::write_json(
    const ores::trading::messaging::script_coarsening_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_version_key
wire_traits<ores::trading::messaging::script_coarsening_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_version_key>::write_msgpack(
    const ores::trading::messaging::script_coarsening_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_version_key
wire_traits<ores::trading::messaging::script_coarsening_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_versions_filter>::write_json(
    const ores::trading::messaging::script_coarsening_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::script_coarsening_versions_filter
wire_traits<ores::trading::messaging::script_coarsening_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::script_coarsening_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::script_coarsening_versions_filter>::write_msgpack(
    const ores::trading::messaging::script_coarsening_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::script_coarsening_versions_filter
wire_traits<ores::trading::messaging::script_coarsening_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::script_coarsening_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_script_coarsenings_request>::write_json(
    const ores::trading::messaging::list_script_coarsenings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_script_coarsenings_request
wire_traits<ores::trading::messaging::list_script_coarsenings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_script_coarsenings_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_script_coarsenings_request>::write_msgpack(
    const ores::trading::messaging::list_script_coarsenings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_script_coarsenings_request
wire_traits<ores::trading::messaging::list_script_coarsenings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_script_coarsenings_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_script_coarsenings_response>::write_json(
    const ores::trading::messaging::list_script_coarsenings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_script_coarsenings_response
wire_traits<ores::trading::messaging::list_script_coarsenings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_script_coarsenings_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_script_coarsenings_response>::write_msgpack(
    const ores::trading::messaging::list_script_coarsenings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_script_coarsenings_response
wire_traits<ores::trading::messaging::list_script_coarsenings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_script_coarsenings_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_script_coarsening_request>::write_json(
    const ores::trading::messaging::get_script_coarsening_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_script_coarsening_request
wire_traits<ores::trading::messaging::get_script_coarsening_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_script_coarsening_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_script_coarsening_request>::write_msgpack(
    const ores::trading::messaging::get_script_coarsening_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_script_coarsening_request
wire_traits<ores::trading::messaging::get_script_coarsening_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_script_coarsening_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_script_coarsening_response>::write_json(
    const ores::trading::messaging::get_script_coarsening_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_script_coarsening_response
wire_traits<ores::trading::messaging::get_script_coarsening_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_script_coarsening_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_script_coarsening_response>::write_msgpack(
    const ores::trading::messaging::get_script_coarsening_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_script_coarsening_response
wire_traits<ores::trading::messaging::get_script_coarsening_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_script_coarsening_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_script_coarsenings_request>::write_json(
    const ores::trading::messaging::get_many_script_coarsenings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_script_coarsenings_request
wire_traits<ores::trading::messaging::get_many_script_coarsenings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_script_coarsenings_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_script_coarsenings_request>::write_msgpack(
    const ores::trading::messaging::get_many_script_coarsenings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_script_coarsenings_request
wire_traits<ores::trading::messaging::get_many_script_coarsenings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_script_coarsenings_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::get_many_script_coarsenings_response>::write_json(
    const ores::trading::messaging::get_many_script_coarsenings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_script_coarsenings_response
wire_traits<ores::trading::messaging::get_many_script_coarsenings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_script_coarsenings_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_script_coarsenings_response>::write_msgpack(
    const ores::trading::messaging::get_many_script_coarsenings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_script_coarsenings_response
wire_traits<ores::trading::messaging::get_many_script_coarsenings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_script_coarsenings_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::put_script_coarsening_request>::write_json(
    const ores::trading::messaging::put_script_coarsening_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_script_coarsening_request
wire_traits<ores::trading::messaging::put_script_coarsening_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_script_coarsening_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_script_coarsening_request>::write_msgpack(
    const ores::trading::messaging::put_script_coarsening_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_script_coarsening_request
wire_traits<ores::trading::messaging::put_script_coarsening_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_script_coarsening_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_script_coarsening_response>::write_json(
    const ores::trading::messaging::put_script_coarsening_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_script_coarsening_response
wire_traits<ores::trading::messaging::put_script_coarsening_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_script_coarsening_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_script_coarsening_response>::write_msgpack(
    const ores::trading::messaging::put_script_coarsening_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_script_coarsening_response
wire_traits<ores::trading::messaging::put_script_coarsening_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_script_coarsening_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_script_coarsenings_request>::write_json(
    const ores::trading::messaging::put_many_script_coarsenings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_script_coarsenings_request
wire_traits<ores::trading::messaging::put_many_script_coarsenings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_script_coarsenings_request>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_script_coarsenings_request>::write_msgpack(
    const ores::trading::messaging::put_many_script_coarsenings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_script_coarsenings_request
wire_traits<ores::trading::messaging::put_many_script_coarsenings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_script_coarsenings_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::put_many_script_coarsenings_response>::write_json(
    const ores::trading::messaging::put_many_script_coarsenings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_script_coarsenings_response
wire_traits<ores::trading::messaging::put_many_script_coarsenings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_script_coarsenings_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_script_coarsenings_response>::write_msgpack(
    const ores::trading::messaging::put_many_script_coarsenings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_script_coarsenings_response
wire_traits<ores::trading::messaging::put_many_script_coarsenings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_script_coarsenings_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::delete_script_coarsening_request>::write_json(
    const ores::trading::messaging::delete_script_coarsening_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_script_coarsening_request
wire_traits<ores::trading::messaging::delete_script_coarsening_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_script_coarsening_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_script_coarsening_request>::write_msgpack(
    const ores::trading::messaging::delete_script_coarsening_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_script_coarsening_request
wire_traits<ores::trading::messaging::delete_script_coarsening_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_script_coarsening_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_script_coarsening_response>::write_json(
    const ores::trading::messaging::delete_script_coarsening_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_script_coarsening_response
wire_traits<ores::trading::messaging::delete_script_coarsening_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_script_coarsening_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_script_coarsening_response>::write_msgpack(
    const ores::trading::messaging::delete_script_coarsening_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_script_coarsening_response
wire_traits<ores::trading::messaging::delete_script_coarsening_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_script_coarsening_response>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_request>::write_json(
    const ores::trading::messaging::delete_many_script_coarsenings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_script_coarsenings_request
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_script_coarsenings_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_request>::write_msgpack(
    const ores::trading::messaging::delete_many_script_coarsenings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_script_coarsenings_request
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_script_coarsenings_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_response>::write_json(
    const ores::trading::messaging::delete_many_script_coarsenings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_script_coarsenings_response
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_script_coarsenings_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_response>::write_msgpack(
    const ores::trading::messaging::delete_many_script_coarsenings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_script_coarsenings_response
wire_traits<ores::trading::messaging::delete_many_script_coarsenings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_script_coarsenings_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_script_coarsening_versions_request>::write_json(
    const ores::trading::messaging::list_script_coarsening_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_script_coarsening_versions_request
wire_traits<ores::trading::messaging::list_script_coarsening_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_script_coarsening_versions_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_script_coarsening_versions_request>::write_msgpack(
    const ores::trading::messaging::list_script_coarsening_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_script_coarsening_versions_request
wire_traits<ores::trading::messaging::list_script_coarsening_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_script_coarsening_versions_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_script_coarsening_versions_response>::write_json(
    const ores::trading::messaging::list_script_coarsening_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_script_coarsening_versions_response
wire_traits<ores::trading::messaging::list_script_coarsening_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_script_coarsening_versions_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_script_coarsening_versions_response>::write_msgpack(
    const ores::trading::messaging::list_script_coarsening_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_script_coarsening_versions_response
wire_traits<ores::trading::messaging::list_script_coarsening_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_script_coarsening_versions_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_script_coarsening_version_request>::write_json(
    const ores::trading::messaging::get_script_coarsening_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_script_coarsening_version_request
wire_traits<ores::trading::messaging::get_script_coarsening_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_script_coarsening_version_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_script_coarsening_version_request>::write_msgpack(
    const ores::trading::messaging::get_script_coarsening_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_script_coarsening_version_request
wire_traits<ores::trading::messaging::get_script_coarsening_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_script_coarsening_version_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_script_coarsening_version_response>::write_json(
    const ores::trading::messaging::get_script_coarsening_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_script_coarsening_version_response
wire_traits<ores::trading::messaging::get_script_coarsening_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_script_coarsening_version_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::get_script_coarsening_version_response>::write_msgpack(
    const ores::trading::messaging::get_script_coarsening_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_script_coarsening_version_response
wire_traits<ores::trading::messaging::get_script_coarsening_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_script_coarsening_version_response>(
        data);
}

}
