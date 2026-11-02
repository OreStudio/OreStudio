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
#include "ores.reporting.api/domain/report_text_value_codec.hpp"
#include "ores.reporting.api/messaging/report_text_value_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::reporting::messaging::report_text_value_key>::write_json(
    const ores::reporting::messaging::report_text_value_key& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_key
wire_traits<ores::reporting::messaging::report_text_value_key>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_key>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_key>::write_msgpack(
    const ores::reporting::messaging::report_text_value_key& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_key
wire_traits<ores::reporting::messaging::report_text_value_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_key>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_write>::write_json(
    const ores::reporting::messaging::report_text_value_write& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_write
wire_traits<ores::reporting::messaging::report_text_value_write>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_write>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_write>::write_msgpack(
    const ores::reporting::messaging::report_text_value_write& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_write
wire_traits<ores::reporting::messaging::report_text_value_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_write>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_change>::write_json(
    const ores::reporting::messaging::report_text_value_change& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_change
wire_traits<ores::reporting::messaging::report_text_value_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_change>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_change>::write_msgpack(
    const ores::reporting::messaging::report_text_value_change& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_change
wire_traits<ores::reporting::messaging::report_text_value_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_change>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_removal>::write_json(
    const ores::reporting::messaging::report_text_value_removal& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_removal
wire_traits<ores::reporting::messaging::report_text_value_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_removal>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_removal>::write_msgpack(
    const ores::reporting::messaging::report_text_value_removal& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_removal
wire_traits<ores::reporting::messaging::report_text_value_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_removal>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_lookup>::write_json(
    const ores::reporting::messaging::report_text_value_lookup& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_lookup
wire_traits<ores::reporting::messaging::report_text_value_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_lookup>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_lookup>::write_msgpack(
    const ores::reporting::messaging::report_text_value_lookup& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_lookup
wire_traits<ores::reporting::messaging::report_text_value_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_lookup>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_values_filter>::write_json(
    const ores::reporting::messaging::report_text_values_filter& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_values_filter
wire_traits<ores::reporting::messaging::report_text_values_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_values_filter>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_values_filter>::write_msgpack(
    const ores::reporting::messaging::report_text_values_filter& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_values_filter
wire_traits<ores::reporting::messaging::report_text_values_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_values_filter>(data);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_event>::write_json(
    const ores::reporting::messaging::report_text_value_event& v) {
    return object::write_json(v);
}

ores::reporting::messaging::report_text_value_event
wire_traits<ores::reporting::messaging::report_text_value_event>::read_json(std::string_view text) {
    return object::read_json<ores::reporting::messaging::report_text_value_event>(text);
}

std::string wire_traits<ores::reporting::messaging::report_text_value_event>::write_msgpack(
    const ores::reporting::messaging::report_text_value_event& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::report_text_value_event
wire_traits<ores::reporting::messaging::report_text_value_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::report_text_value_event>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_text_values_request>::write_json(
    const ores::reporting::messaging::list_report_text_values_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_text_values_request
wire_traits<ores::reporting::messaging::list_report_text_values_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_text_values_request>(text);
}

std::string wire_traits<ores::reporting::messaging::list_report_text_values_request>::write_msgpack(
    const ores::reporting::messaging::list_report_text_values_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_text_values_request
wire_traits<ores::reporting::messaging::list_report_text_values_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_text_values_request>(data);
}

std::string wire_traits<ores::reporting::messaging::list_report_text_values_response>::write_json(
    const ores::reporting::messaging::list_report_text_values_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::list_report_text_values_response
wire_traits<ores::reporting::messaging::list_report_text_values_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::list_report_text_values_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::list_report_text_values_response>::write_msgpack(
    const ores::reporting::messaging::list_report_text_values_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::list_report_text_values_response
wire_traits<ores::reporting::messaging::list_report_text_values_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::list_report_text_values_response>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_text_value_request>::write_json(
    const ores::reporting::messaging::get_report_text_value_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_text_value_request
wire_traits<ores::reporting::messaging::get_report_text_value_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_text_value_request>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_text_value_request>::write_msgpack(
    const ores::reporting::messaging::get_report_text_value_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_text_value_request
wire_traits<ores::reporting::messaging::get_report_text_value_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_text_value_request>(data);
}

std::string wire_traits<ores::reporting::messaging::get_report_text_value_response>::write_json(
    const ores::reporting::messaging::get_report_text_value_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_report_text_value_response
wire_traits<ores::reporting::messaging::get_report_text_value_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_report_text_value_response>(text);
}

std::string wire_traits<ores::reporting::messaging::get_report_text_value_response>::write_msgpack(
    const ores::reporting::messaging::get_report_text_value_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_report_text_value_response
wire_traits<ores::reporting::messaging::get_report_text_value_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_report_text_value_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_text_values_request>::write_json(
    const ores::reporting::messaging::get_many_report_text_values_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_text_values_request
wire_traits<ores::reporting::messaging::get_many_report_text_values_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_text_values_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_text_values_request>::write_msgpack(
    const ores::reporting::messaging::get_many_report_text_values_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_text_values_request
wire_traits<ores::reporting::messaging::get_many_report_text_values_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_text_values_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_text_values_response>::write_json(
    const ores::reporting::messaging::get_many_report_text_values_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::get_many_report_text_values_response
wire_traits<ores::reporting::messaging::get_many_report_text_values_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::get_many_report_text_values_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::get_many_report_text_values_response>::write_msgpack(
    const ores::reporting::messaging::get_many_report_text_values_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::get_many_report_text_values_response
wire_traits<ores::reporting::messaging::get_many_report_text_values_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::get_many_report_text_values_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::put_report_text_value_request>::write_json(
    const ores::reporting::messaging::put_report_text_value_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_text_value_request
wire_traits<ores::reporting::messaging::put_report_text_value_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_text_value_request>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_text_value_request>::write_msgpack(
    const ores::reporting::messaging::put_report_text_value_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_text_value_request
wire_traits<ores::reporting::messaging::put_report_text_value_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_text_value_request>(data);
}

std::string wire_traits<ores::reporting::messaging::put_report_text_value_response>::write_json(
    const ores::reporting::messaging::put_report_text_value_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_report_text_value_response
wire_traits<ores::reporting::messaging::put_report_text_value_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_report_text_value_response>(text);
}

std::string wire_traits<ores::reporting::messaging::put_report_text_value_response>::write_msgpack(
    const ores::reporting::messaging::put_report_text_value_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_report_text_value_response
wire_traits<ores::reporting::messaging::put_report_text_value_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_report_text_value_response>(data);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_text_values_request>::write_json(
    const ores::reporting::messaging::put_many_report_text_values_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_text_values_request
wire_traits<ores::reporting::messaging::put_many_report_text_values_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_text_values_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_text_values_request>::write_msgpack(
    const ores::reporting::messaging::put_many_report_text_values_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_text_values_request
wire_traits<ores::reporting::messaging::put_many_report_text_values_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_text_values_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_text_values_response>::write_json(
    const ores::reporting::messaging::put_many_report_text_values_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::put_many_report_text_values_response
wire_traits<ores::reporting::messaging::put_many_report_text_values_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::put_many_report_text_values_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::put_many_report_text_values_response>::write_msgpack(
    const ores::reporting::messaging::put_many_report_text_values_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::put_many_report_text_values_response
wire_traits<ores::reporting::messaging::put_many_report_text_values_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::put_many_report_text_values_response>(
        data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_text_value_request>::write_json(
    const ores::reporting::messaging::delete_report_text_value_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_text_value_request
wire_traits<ores::reporting::messaging::delete_report_text_value_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_text_value_request>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_report_text_value_request>::write_msgpack(
    const ores::reporting::messaging::delete_report_text_value_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_text_value_request
wire_traits<ores::reporting::messaging::delete_report_text_value_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_text_value_request>(data);
}

std::string wire_traits<ores::reporting::messaging::delete_report_text_value_response>::write_json(
    const ores::reporting::messaging::delete_report_text_value_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_report_text_value_response
wire_traits<ores::reporting::messaging::delete_report_text_value_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_report_text_value_response>(text);
}

std::string
wire_traits<ores::reporting::messaging::delete_report_text_value_response>::write_msgpack(
    const ores::reporting::messaging::delete_report_text_value_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_report_text_value_response
wire_traits<ores::reporting::messaging::delete_report_text_value_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_report_text_value_response>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_text_values_request>::write_json(
    const ores::reporting::messaging::delete_many_report_text_values_request& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_text_values_request
wire_traits<ores::reporting::messaging::delete_many_report_text_values_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_text_values_request>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_text_values_request>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_text_values_request& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_text_values_request
wire_traits<ores::reporting::messaging::delete_many_report_text_values_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::reporting::messaging::delete_many_report_text_values_request>(
        data);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_text_values_response>::write_json(
    const ores::reporting::messaging::delete_many_report_text_values_response& v) {
    return object::write_json(v);
}

ores::reporting::messaging::delete_many_report_text_values_response
wire_traits<ores::reporting::messaging::delete_many_report_text_values_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::reporting::messaging::delete_many_report_text_values_response>(
        text);
}

std::string
wire_traits<ores::reporting::messaging::delete_many_report_text_values_response>::write_msgpack(
    const ores::reporting::messaging::delete_many_report_text_values_response& v) {
    return object::write_msgpack(v);
}

ores::reporting::messaging::delete_many_report_text_values_response
wire_traits<ores::reporting::messaging::delete_many_report_text_values_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::reporting::messaging::delete_many_report_text_values_response>(data);
}

}
