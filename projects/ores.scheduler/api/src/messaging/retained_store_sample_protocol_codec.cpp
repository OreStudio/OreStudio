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
#include "ores.scheduler.api/domain/retained_store_sample_codec.hpp"
#include "ores.scheduler.api/messaging/retained_store_sample_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_key>::write_json(
    const ores::scheduler::messaging::retained_store_sample_key& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_key
wire_traits<ores::scheduler::messaging::retained_store_sample_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_key>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_key>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_key& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_key
wire_traits<ores::scheduler::messaging::retained_store_sample_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_key>(data);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_write>::write_json(
    const ores::scheduler::messaging::retained_store_sample_write& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_write
wire_traits<ores::scheduler::messaging::retained_store_sample_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_write>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_write>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_write& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_write
wire_traits<ores::scheduler::messaging::retained_store_sample_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_write>(data);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_change>::write_json(
    const ores::scheduler::messaging::retained_store_sample_change& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_change
wire_traits<ores::scheduler::messaging::retained_store_sample_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_change>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_change>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_change& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_change
wire_traits<ores::scheduler::messaging::retained_store_sample_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_change>(data);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_removal>::write_json(
    const ores::scheduler::messaging::retained_store_sample_removal& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_removal
wire_traits<ores::scheduler::messaging::retained_store_sample_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_removal>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_removal>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_removal& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_removal
wire_traits<ores::scheduler::messaging::retained_store_sample_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_removal>(data);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_lookup>::write_json(
    const ores::scheduler::messaging::retained_store_sample_lookup& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_lookup
wire_traits<ores::scheduler::messaging::retained_store_sample_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_lookup>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_lookup>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_lookup& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_lookup
wire_traits<ores::scheduler::messaging::retained_store_sample_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_lookup>(data);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_event>::write_json(
    const ores::scheduler::messaging::retained_store_sample_event& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::retained_store_sample_event
wire_traits<ores::scheduler::messaging::retained_store_sample_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::retained_store_sample_event>(text);
}

std::string wire_traits<ores::scheduler::messaging::retained_store_sample_event>::write_msgpack(
    const ores::scheduler::messaging::retained_store_sample_event& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::retained_store_sample_event
wire_traits<ores::scheduler::messaging::retained_store_sample_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::retained_store_sample_event>(data);
}

std::string
wire_traits<ores::scheduler::messaging::list_retained_store_samples_request>::write_json(
    const ores::scheduler::messaging::list_retained_store_samples_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::list_retained_store_samples_request
wire_traits<ores::scheduler::messaging::list_retained_store_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::list_retained_store_samples_request>(text);
}

std::string
wire_traits<ores::scheduler::messaging::list_retained_store_samples_request>::write_msgpack(
    const ores::scheduler::messaging::list_retained_store_samples_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::list_retained_store_samples_request
wire_traits<ores::scheduler::messaging::list_retained_store_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::list_retained_store_samples_request>(
        data);
}

std::string
wire_traits<ores::scheduler::messaging::list_retained_store_samples_response>::write_json(
    const ores::scheduler::messaging::list_retained_store_samples_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::list_retained_store_samples_response
wire_traits<ores::scheduler::messaging::list_retained_store_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::list_retained_store_samples_response>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::list_retained_store_samples_response>::write_msgpack(
    const ores::scheduler::messaging::list_retained_store_samples_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::list_retained_store_samples_response
wire_traits<ores::scheduler::messaging::list_retained_store_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::list_retained_store_samples_response>(
        data);
}

std::string wire_traits<ores::scheduler::messaging::get_retained_store_sample_request>::write_json(
    const ores::scheduler::messaging::get_retained_store_sample_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_retained_store_sample_request
wire_traits<ores::scheduler::messaging::get_retained_store_sample_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_retained_store_sample_request>(text);
}

std::string
wire_traits<ores::scheduler::messaging::get_retained_store_sample_request>::write_msgpack(
    const ores::scheduler::messaging::get_retained_store_sample_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_retained_store_sample_request
wire_traits<ores::scheduler::messaging::get_retained_store_sample_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_retained_store_sample_request>(
        data);
}

std::string wire_traits<ores::scheduler::messaging::get_retained_store_sample_response>::write_json(
    const ores::scheduler::messaging::get_retained_store_sample_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_retained_store_sample_response
wire_traits<ores::scheduler::messaging::get_retained_store_sample_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_retained_store_sample_response>(text);
}

std::string
wire_traits<ores::scheduler::messaging::get_retained_store_sample_response>::write_msgpack(
    const ores::scheduler::messaging::get_retained_store_sample_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_retained_store_sample_response
wire_traits<ores::scheduler::messaging::get_retained_store_sample_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::get_retained_store_sample_response>(
        data);
}

std::string
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_request>::write_json(
    const ores::scheduler::messaging::get_many_retained_store_samples_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_many_retained_store_samples_request>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_request>::write_msgpack(
    const ores::scheduler::messaging::get_many_retained_store_samples_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::get_many_retained_store_samples_request>(data);
}

std::string
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_response>::write_json(
    const ores::scheduler::messaging::get_many_retained_store_samples_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::get_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::get_many_retained_store_samples_response>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_response>::write_msgpack(
    const ores::scheduler::messaging::get_many_retained_store_samples_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::get_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::get_many_retained_store_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::get_many_retained_store_samples_response>(data);
}

std::string wire_traits<ores::scheduler::messaging::put_retained_store_sample_request>::write_json(
    const ores::scheduler::messaging::put_retained_store_sample_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::put_retained_store_sample_request
wire_traits<ores::scheduler::messaging::put_retained_store_sample_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::put_retained_store_sample_request>(text);
}

std::string
wire_traits<ores::scheduler::messaging::put_retained_store_sample_request>::write_msgpack(
    const ores::scheduler::messaging::put_retained_store_sample_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::put_retained_store_sample_request
wire_traits<ores::scheduler::messaging::put_retained_store_sample_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::put_retained_store_sample_request>(
        data);
}

std::string wire_traits<ores::scheduler::messaging::put_retained_store_sample_response>::write_json(
    const ores::scheduler::messaging::put_retained_store_sample_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::put_retained_store_sample_response
wire_traits<ores::scheduler::messaging::put_retained_store_sample_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::put_retained_store_sample_response>(text);
}

std::string
wire_traits<ores::scheduler::messaging::put_retained_store_sample_response>::write_msgpack(
    const ores::scheduler::messaging::put_retained_store_sample_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::put_retained_store_sample_response
wire_traits<ores::scheduler::messaging::put_retained_store_sample_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::put_retained_store_sample_response>(
        data);
}

std::string
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_request>::write_json(
    const ores::scheduler::messaging::put_many_retained_store_samples_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::put_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::put_many_retained_store_samples_request>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_request>::write_msgpack(
    const ores::scheduler::messaging::put_many_retained_store_samples_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::put_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::put_many_retained_store_samples_request>(data);
}

std::string
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_response>::write_json(
    const ores::scheduler::messaging::put_many_retained_store_samples_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::put_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::put_many_retained_store_samples_response>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_response>::write_msgpack(
    const ores::scheduler::messaging::put_many_retained_store_samples_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::put_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::put_many_retained_store_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::put_many_retained_store_samples_response>(data);
}

std::string
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_request>::write_json(
    const ores::scheduler::messaging::delete_retained_store_sample_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::delete_retained_store_sample_request
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::delete_retained_store_sample_request>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_request>::write_msgpack(
    const ores::scheduler::messaging::delete_retained_store_sample_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::delete_retained_store_sample_request
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::delete_retained_store_sample_request>(
        data);
}

std::string
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_response>::write_json(
    const ores::scheduler::messaging::delete_retained_store_sample_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::delete_retained_store_sample_response
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::scheduler::messaging::delete_retained_store_sample_response>(
        text);
}

std::string
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_response>::write_msgpack(
    const ores::scheduler::messaging::delete_retained_store_sample_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::delete_retained_store_sample_response
wire_traits<ores::scheduler::messaging::delete_retained_store_sample_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::scheduler::messaging::delete_retained_store_sample_response>(
        data);
}

std::string
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_request>::write_json(
    const ores::scheduler::messaging::delete_many_retained_store_samples_request& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::delete_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::scheduler::messaging::delete_many_retained_store_samples_request>(text);
}

std::string
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_request>::write_msgpack(
    const ores::scheduler::messaging::delete_many_retained_store_samples_request& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::delete_many_retained_store_samples_request
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::delete_many_retained_store_samples_request>(data);
}

std::string
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_response>::write_json(
    const ores::scheduler::messaging::delete_many_retained_store_samples_response& v) {
    return object::write_json(v);
}

ores::scheduler::messaging::delete_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::scheduler::messaging::delete_many_retained_store_samples_response>(text);
}

std::string
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_response>::write_msgpack(
    const ores::scheduler::messaging::delete_many_retained_store_samples_response& v) {
    return object::write_msgpack(v);
}

ores::scheduler::messaging::delete_many_retained_store_samples_response
wire_traits<ores::scheduler::messaging::delete_many_retained_store_samples_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::scheduler::messaging::delete_many_retained_store_samples_response>(data);
}

}
