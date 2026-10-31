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
#include "ores.refdata.api/domain/tenor_convention_resolution_codec.hpp"
#include "ores.refdata.api/messaging/tenor_convention_resolution_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_key>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_key
wire_traits<ores::refdata::messaging::tenor_convention_resolution_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_key>(text);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_key>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_key
wire_traits<ores::refdata::messaging::tenor_convention_resolution_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_key>(data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_write>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_write
wire_traits<ores::refdata::messaging::tenor_convention_resolution_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_write>(text);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_write>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_write
wire_traits<ores::refdata::messaging::tenor_convention_resolution_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_write>(data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_change>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_change
wire_traits<ores::refdata::messaging::tenor_convention_resolution_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_change>(text);
}

std::string
wire_traits<ores::refdata::messaging::tenor_convention_resolution_change>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_change
wire_traits<ores::refdata::messaging::tenor_convention_resolution_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_change>(data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_removal>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_removal
wire_traits<ores::refdata::messaging::tenor_convention_resolution_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_removal>(text);
}

std::string
wire_traits<ores::refdata::messaging::tenor_convention_resolution_removal>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_removal
wire_traits<ores::refdata::messaging::tenor_convention_resolution_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_removal>(
        data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_lookup>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_lookup
wire_traits<ores::refdata::messaging::tenor_convention_resolution_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_lookup>(text);
}

std::string
wire_traits<ores::refdata::messaging::tenor_convention_resolution_lookup>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_lookup
wire_traits<ores::refdata::messaging::tenor_convention_resolution_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolutions_filter>::write_json(
    const ores::refdata::messaging::tenor_convention_resolutions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolutions_filter
wire_traits<ores::refdata::messaging::tenor_convention_resolutions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolutions_filter>(text);
}

std::string
wire_traits<ores::refdata::messaging::tenor_convention_resolutions_filter>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolutions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolutions_filter
wire_traits<ores::refdata::messaging::tenor_convention_resolutions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolutions_filter>(
        data);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_event>::write_json(
    const ores::refdata::messaging::tenor_convention_resolution_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::tenor_convention_resolution_event
wire_traits<ores::refdata::messaging::tenor_convention_resolution_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::tenor_convention_resolution_event>(text);
}

std::string wire_traits<ores::refdata::messaging::tenor_convention_resolution_event>::write_msgpack(
    const ores::refdata::messaging::tenor_convention_resolution_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::tenor_convention_resolution_event
wire_traits<ores::refdata::messaging::tenor_convention_resolution_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::tenor_convention_resolution_event>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_request>::write_json(
    const ores::refdata::messaging::list_tenor_convention_resolutions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_tenor_convention_resolutions_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_request>::write_msgpack(
    const ores::refdata::messaging::list_tenor_convention_resolutions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_tenor_convention_resolutions_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_response>::write_json(
    const ores::refdata::messaging::list_tenor_convention_resolutions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_tenor_convention_resolutions_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_response>::write_msgpack(
    const ores::refdata::messaging::list_tenor_convention_resolutions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::list_tenor_convention_resolutions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_tenor_convention_resolutions_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_request>::write_json(
    const ores::refdata::messaging::get_tenor_convention_resolution_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_tenor_convention_resolution_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_request>::write_msgpack(
    const ores::refdata::messaging::get_tenor_convention_resolution_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_tenor_convention_resolution_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_response>::write_json(
    const ores::refdata::messaging::get_tenor_convention_resolution_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_tenor_convention_resolution_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_response>::write_msgpack(
    const ores::refdata::messaging::get_tenor_convention_resolution_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::get_tenor_convention_resolution_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_tenor_convention_resolution_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_request>::write_json(
    const ores::refdata::messaging::get_many_tenor_convention_resolutions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::get_many_tenor_convention_resolutions_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_request>::write_msgpack(
    const ores::refdata::messaging::get_many_tenor_convention_resolutions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_many_tenor_convention_resolutions_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_response>::write_json(
    const ores::refdata::messaging::get_many_tenor_convention_resolutions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::get_many_tenor_convention_resolutions_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_response>::
    write_msgpack(
        const ores::refdata::messaging::get_many_tenor_convention_resolutions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::get_many_tenor_convention_resolutions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_many_tenor_convention_resolutions_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_request>::write_json(
    const ores::refdata::messaging::put_tenor_convention_resolution_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_tenor_convention_resolution_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_request>::write_msgpack(
    const ores::refdata::messaging::put_tenor_convention_resolution_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_tenor_convention_resolution_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_response>::write_json(
    const ores::refdata::messaging::put_tenor_convention_resolution_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_tenor_convention_resolution_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_response>::write_msgpack(
    const ores::refdata::messaging::put_tenor_convention_resolution_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::put_tenor_convention_resolution_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_tenor_convention_resolution_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_request>::write_json(
    const ores::refdata::messaging::put_many_tenor_convention_resolutions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::put_many_tenor_convention_resolutions_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_request>::write_msgpack(
    const ores::refdata::messaging::put_many_tenor_convention_resolutions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::put_many_tenor_convention_resolutions_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_response>::write_json(
    const ores::refdata::messaging::put_many_tenor_convention_resolutions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::put_many_tenor_convention_resolutions_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_response>::
    write_msgpack(
        const ores::refdata::messaging::put_many_tenor_convention_resolutions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::put_many_tenor_convention_resolutions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::put_many_tenor_convention_resolutions_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_request>::write_json(
    const ores::refdata::messaging::delete_tenor_convention_resolution_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_tenor_convention_resolution_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_request>::write_msgpack(
    const ores::refdata::messaging::delete_tenor_convention_resolution_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_tenor_convention_resolution_request
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_tenor_convention_resolution_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_response>::write_json(
    const ores::refdata::messaging::delete_tenor_convention_resolution_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_tenor_convention_resolution_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_response>::write_msgpack(
    const ores::refdata::messaging::delete_tenor_convention_resolution_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_tenor_convention_resolution_response
wire_traits<ores::refdata::messaging::delete_tenor_convention_resolution_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_tenor_convention_resolution_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>::write_json(
    const ores::refdata::messaging::delete_many_tenor_convention_resolutions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>::
    write_msgpack(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_tenor_convention_resolutions_request
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_many_tenor_convention_resolutions_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>::
    write_json(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>::
    write_msgpack(
        const ores::refdata::messaging::delete_many_tenor_convention_resolutions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_tenor_convention_resolutions_response
wire_traits<ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_many_tenor_convention_resolutions_response>(data);
}

std::string wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>::
    write_json(const ores::refdata::messaging::
                   list_by_convention_code_tenor_convention_resolutions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>(
        text);
}

std::string wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>::
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_code_tenor_convention_resolutions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_request>(
        data);
}

std::string wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>::
    write_json(const ores::refdata::messaging::
                   list_by_convention_code_tenor_convention_resolutions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>(
        text);
}

std::string wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>::
    write_msgpack(const ores::refdata::messaging::
                      list_by_convention_code_tenor_convention_resolutions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response wire_traits<
    ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_convention_code_tenor_convention_resolutions_response>(
        data);
}

}
