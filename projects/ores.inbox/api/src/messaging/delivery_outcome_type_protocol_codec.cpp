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
#include "ores.inbox.api/domain/delivery_outcome_type_codec.hpp"
#include "ores.inbox.api/messaging/delivery_outcome_type_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_key>::write_json(
    const ores::inbox::messaging::delivery_outcome_type_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delivery_outcome_type_key
wire_traits<ores::inbox::messaging::delivery_outcome_type_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::delivery_outcome_type_key>(text);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_key>::write_msgpack(
    const ores::inbox::messaging::delivery_outcome_type_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delivery_outcome_type_key
wire_traits<ores::inbox::messaging::delivery_outcome_type_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delivery_outcome_type_key>(data);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_lookup>::write_json(
    const ores::inbox::messaging::delivery_outcome_type_lookup& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delivery_outcome_type_lookup
wire_traits<ores::inbox::messaging::delivery_outcome_type_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delivery_outcome_type_lookup>(text);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_lookup>::write_msgpack(
    const ores::inbox::messaging::delivery_outcome_type_lookup& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delivery_outcome_type_lookup
wire_traits<ores::inbox::messaging::delivery_outcome_type_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delivery_outcome_type_lookup>(data);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_types_filter>::write_json(
    const ores::inbox::messaging::delivery_outcome_types_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delivery_outcome_types_filter
wire_traits<ores::inbox::messaging::delivery_outcome_types_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delivery_outcome_types_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_types_filter>::write_msgpack(
    const ores::inbox::messaging::delivery_outcome_types_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delivery_outcome_types_filter
wire_traits<ores::inbox::messaging::delivery_outcome_types_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delivery_outcome_types_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_event>::write_json(
    const ores::inbox::messaging::delivery_outcome_type_event& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delivery_outcome_type_event
wire_traits<ores::inbox::messaging::delivery_outcome_type_event>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::delivery_outcome_type_event>(text);
}

std::string wire_traits<ores::inbox::messaging::delivery_outcome_type_event>::write_msgpack(
    const ores::inbox::messaging::delivery_outcome_type_event& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delivery_outcome_type_event
wire_traits<ores::inbox::messaging::delivery_outcome_type_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delivery_outcome_type_event>(data);
}

std::string wire_traits<ores::inbox::messaging::list_delivery_outcome_types_request>::write_json(
    const ores::inbox::messaging::list_delivery_outcome_types_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_delivery_outcome_types_request
wire_traits<ores::inbox::messaging::list_delivery_outcome_types_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_delivery_outcome_types_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_delivery_outcome_types_request>::write_msgpack(
    const ores::inbox::messaging::list_delivery_outcome_types_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_delivery_outcome_types_request
wire_traits<ores::inbox::messaging::list_delivery_outcome_types_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_delivery_outcome_types_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_delivery_outcome_types_response>::write_json(
    const ores::inbox::messaging::list_delivery_outcome_types_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_delivery_outcome_types_response
wire_traits<ores::inbox::messaging::list_delivery_outcome_types_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_delivery_outcome_types_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_delivery_outcome_types_response>::write_msgpack(
    const ores::inbox::messaging::list_delivery_outcome_types_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_delivery_outcome_types_response
wire_traits<ores::inbox::messaging::list_delivery_outcome_types_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_delivery_outcome_types_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_delivery_outcome_type_request>::write_json(
    const ores::inbox::messaging::get_delivery_outcome_type_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_delivery_outcome_type_request
wire_traits<ores::inbox::messaging::get_delivery_outcome_type_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_delivery_outcome_type_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_delivery_outcome_type_request>::write_msgpack(
    const ores::inbox::messaging::get_delivery_outcome_type_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_delivery_outcome_type_request
wire_traits<ores::inbox::messaging::get_delivery_outcome_type_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_delivery_outcome_type_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_delivery_outcome_type_response>::write_json(
    const ores::inbox::messaging::get_delivery_outcome_type_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_delivery_outcome_type_response
wire_traits<ores::inbox::messaging::get_delivery_outcome_type_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_delivery_outcome_type_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_delivery_outcome_type_response>::write_msgpack(
    const ores::inbox::messaging::get_delivery_outcome_type_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_delivery_outcome_type_response
wire_traits<ores::inbox::messaging::get_delivery_outcome_type_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_delivery_outcome_type_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_request>::write_json(
    const ores::inbox::messaging::get_many_delivery_outcome_types_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_delivery_outcome_types_request
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_delivery_outcome_types_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_request>::write_msgpack(
    const ores::inbox::messaging::get_many_delivery_outcome_types_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_delivery_outcome_types_request
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_delivery_outcome_types_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_response>::write_json(
    const ores::inbox::messaging::get_many_delivery_outcome_types_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_delivery_outcome_types_response
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_delivery_outcome_types_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_response>::write_msgpack(
    const ores::inbox::messaging::get_many_delivery_outcome_types_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_delivery_outcome_types_response
wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_delivery_outcome_types_response>(
        data);
}

}
