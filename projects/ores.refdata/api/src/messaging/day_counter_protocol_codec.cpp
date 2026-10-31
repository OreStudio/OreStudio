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
#include "ores.refdata.api/domain/day_counter_codec.hpp"
#include "ores.refdata.api/messaging/day_counter_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::day_counter_key>::write_json(
    const ores::refdata::messaging::day_counter_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_key
wire_traits<ores::refdata::messaging::day_counter_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_key>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_key>::write_msgpack(
    const ores::refdata::messaging::day_counter_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_key
wire_traits<ores::refdata::messaging::day_counter_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_key>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_write>::write_json(
    const ores::refdata::messaging::day_counter_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_write
wire_traits<ores::refdata::messaging::day_counter_write>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_write>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_write>::write_msgpack(
    const ores::refdata::messaging::day_counter_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_write
wire_traits<ores::refdata::messaging::day_counter_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_write>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_change>::write_json(
    const ores::refdata::messaging::day_counter_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_change
wire_traits<ores::refdata::messaging::day_counter_change>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_change>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_change>::write_msgpack(
    const ores::refdata::messaging::day_counter_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_change
wire_traits<ores::refdata::messaging::day_counter_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_change>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_removal>::write_json(
    const ores::refdata::messaging::day_counter_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_removal
wire_traits<ores::refdata::messaging::day_counter_removal>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_removal>::write_msgpack(
    const ores::refdata::messaging::day_counter_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_removal
wire_traits<ores::refdata::messaging::day_counter_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_lookup>::write_json(
    const ores::refdata::messaging::day_counter_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_lookup
wire_traits<ores::refdata::messaging::day_counter_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_lookup>::write_msgpack(
    const ores::refdata::messaging::day_counter_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_lookup
wire_traits<ores::refdata::messaging::day_counter_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counters_filter>::write_json(
    const ores::refdata::messaging::day_counters_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counters_filter
wire_traits<ores::refdata::messaging::day_counters_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counters_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counters_filter>::write_msgpack(
    const ores::refdata::messaging::day_counters_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counters_filter
wire_traits<ores::refdata::messaging::day_counters_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counters_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_event>::write_json(
    const ores::refdata::messaging::day_counter_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_event
wire_traits<ores::refdata::messaging::day_counter_event>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_event>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_event>::write_msgpack(
    const ores::refdata::messaging::day_counter_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_event
wire_traits<ores::refdata::messaging::day_counter_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_event>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_version_key>::write_json(
    const ores::refdata::messaging::day_counter_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_version_key
wire_traits<ores::refdata::messaging::day_counter_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_version_key>::write_msgpack(
    const ores::refdata::messaging::day_counter_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_version_key
wire_traits<ores::refdata::messaging::day_counter_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_version_key>(data);
}

std::string wire_traits<ores::refdata::messaging::day_counter_versions_filter>::write_json(
    const ores::refdata::messaging::day_counter_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::day_counter_versions_filter
wire_traits<ores::refdata::messaging::day_counter_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::day_counter_versions_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::day_counter_versions_filter>::write_msgpack(
    const ores::refdata::messaging::day_counter_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::day_counter_versions_filter
wire_traits<ores::refdata::messaging::day_counter_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::day_counter_versions_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_day_counters_request>::write_json(
    const ores::refdata::messaging::list_day_counters_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_day_counters_request
wire_traits<ores::refdata::messaging::list_day_counters_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_day_counters_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_day_counters_request>::write_msgpack(
    const ores::refdata::messaging::list_day_counters_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_day_counters_request
wire_traits<ores::refdata::messaging::list_day_counters_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_day_counters_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_day_counters_response>::write_json(
    const ores::refdata::messaging::list_day_counters_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_day_counters_response
wire_traits<ores::refdata::messaging::list_day_counters_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_day_counters_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_day_counters_response>::write_msgpack(
    const ores::refdata::messaging::list_day_counters_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_day_counters_response
wire_traits<ores::refdata::messaging::list_day_counters_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_day_counters_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_request>::write_json(
    const ores::refdata::messaging::get_day_counter_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_day_counter_request
wire_traits<ores::refdata::messaging::get_day_counter_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_day_counter_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_request>::write_msgpack(
    const ores::refdata::messaging::get_day_counter_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_day_counter_request
wire_traits<ores::refdata::messaging::get_day_counter_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_day_counter_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_response>::write_json(
    const ores::refdata::messaging::get_day_counter_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_day_counter_response
wire_traits<ores::refdata::messaging::get_day_counter_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_day_counter_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_response>::write_msgpack(
    const ores::refdata::messaging::get_day_counter_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_day_counter_response
wire_traits<ores::refdata::messaging::get_day_counter_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_day_counter_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_day_counters_request>::write_json(
    const ores::refdata::messaging::get_many_day_counters_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_day_counters_request
wire_traits<ores::refdata::messaging::get_many_day_counters_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_day_counters_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_day_counters_request>::write_msgpack(
    const ores::refdata::messaging::get_many_day_counters_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_day_counters_request
wire_traits<ores::refdata::messaging::get_many_day_counters_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_day_counters_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_day_counters_response>::write_json(
    const ores::refdata::messaging::get_many_day_counters_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_day_counters_response
wire_traits<ores::refdata::messaging::get_many_day_counters_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_day_counters_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_day_counters_response>::write_msgpack(
    const ores::refdata::messaging::get_many_day_counters_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_day_counters_response
wire_traits<ores::refdata::messaging::get_many_day_counters_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_day_counters_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_day_counter_request>::write_json(
    const ores::refdata::messaging::put_day_counter_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_day_counter_request
wire_traits<ores::refdata::messaging::put_day_counter_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_day_counter_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_day_counter_request>::write_msgpack(
    const ores::refdata::messaging::put_day_counter_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_day_counter_request
wire_traits<ores::refdata::messaging::put_day_counter_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_day_counter_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_day_counter_response>::write_json(
    const ores::refdata::messaging::put_day_counter_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_day_counter_response
wire_traits<ores::refdata::messaging::put_day_counter_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_day_counter_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_day_counter_response>::write_msgpack(
    const ores::refdata::messaging::put_day_counter_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_day_counter_response
wire_traits<ores::refdata::messaging::put_day_counter_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_day_counter_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_day_counters_request>::write_json(
    const ores::refdata::messaging::put_many_day_counters_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_day_counters_request
wire_traits<ores::refdata::messaging::put_many_day_counters_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_day_counters_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_day_counters_request>::write_msgpack(
    const ores::refdata::messaging::put_many_day_counters_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_day_counters_request
wire_traits<ores::refdata::messaging::put_many_day_counters_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_day_counters_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_day_counters_response>::write_json(
    const ores::refdata::messaging::put_many_day_counters_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_day_counters_response
wire_traits<ores::refdata::messaging::put_many_day_counters_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_day_counters_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_day_counters_response>::write_msgpack(
    const ores::refdata::messaging::put_many_day_counters_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_day_counters_response
wire_traits<ores::refdata::messaging::put_many_day_counters_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_day_counters_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_day_counter_request>::write_json(
    const ores::refdata::messaging::delete_day_counter_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_day_counter_request
wire_traits<ores::refdata::messaging::delete_day_counter_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_day_counter_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_day_counter_request>::write_msgpack(
    const ores::refdata::messaging::delete_day_counter_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_day_counter_request
wire_traits<ores::refdata::messaging::delete_day_counter_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_day_counter_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_day_counter_response>::write_json(
    const ores::refdata::messaging::delete_day_counter_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_day_counter_response
wire_traits<ores::refdata::messaging::delete_day_counter_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_day_counter_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_day_counter_response>::write_msgpack(
    const ores::refdata::messaging::delete_day_counter_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_day_counter_response
wire_traits<ores::refdata::messaging::delete_day_counter_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_day_counter_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_day_counters_request>::write_json(
    const ores::refdata::messaging::delete_many_day_counters_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_day_counters_request
wire_traits<ores::refdata::messaging::delete_many_day_counters_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_day_counters_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_many_day_counters_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_day_counters_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_day_counters_request
wire_traits<ores::refdata::messaging::delete_many_day_counters_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_day_counters_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_day_counters_response>::write_json(
    const ores::refdata::messaging::delete_many_day_counters_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_day_counters_response
wire_traits<ores::refdata::messaging::delete_many_day_counters_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_day_counters_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_many_day_counters_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_day_counters_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_day_counters_response
wire_traits<ores::refdata::messaging::delete_many_day_counters_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_day_counters_response>(data);
}

std::string wire_traits<ores::refdata::messaging::list_day_counter_versions_request>::write_json(
    const ores::refdata::messaging::list_day_counter_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_day_counter_versions_request
wire_traits<ores::refdata::messaging::list_day_counter_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_day_counter_versions_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_day_counter_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_day_counter_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_day_counter_versions_request
wire_traits<ores::refdata::messaging::list_day_counter_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_day_counter_versions_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_day_counter_versions_response>::write_json(
    const ores::refdata::messaging::list_day_counter_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_day_counter_versions_response
wire_traits<ores::refdata::messaging::list_day_counter_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_day_counter_versions_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_day_counter_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_day_counter_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_day_counter_versions_response
wire_traits<ores::refdata::messaging::list_day_counter_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_day_counter_versions_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_version_request>::write_json(
    const ores::refdata::messaging::get_day_counter_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_day_counter_version_request
wire_traits<ores::refdata::messaging::get_day_counter_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_day_counter_version_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_version_request>::write_msgpack(
    const ores::refdata::messaging::get_day_counter_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_day_counter_version_request
wire_traits<ores::refdata::messaging::get_day_counter_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_day_counter_version_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_version_response>::write_json(
    const ores::refdata::messaging::get_day_counter_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_day_counter_version_response
wire_traits<ores::refdata::messaging::get_day_counter_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_day_counter_version_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_day_counter_version_response>::write_msgpack(
    const ores::refdata::messaging::get_day_counter_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_day_counter_version_response
wire_traits<ores::refdata::messaging::get_day_counter_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_day_counter_version_response>(data);
}

}
