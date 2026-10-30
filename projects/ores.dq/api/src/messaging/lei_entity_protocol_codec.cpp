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
#include "ores.dq.api/domain/lei_entity_codec.hpp"
#include "ores.dq.api/messaging/lei_entity_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::lei_entity_key>::write_json(
    const ores::dq::messaging::lei_entity_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::lei_entity_key
wire_traits<ores::dq::messaging::lei_entity_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::lei_entity_key>(text);
}

std::string wire_traits<ores::dq::messaging::lei_entity_key>::write_msgpack(
    const ores::dq::messaging::lei_entity_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::lei_entity_key
wire_traits<ores::dq::messaging::lei_entity_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::lei_entity_key>(data);
}

std::string wire_traits<ores::dq::messaging::lei_entity_lookup>::write_json(
    const ores::dq::messaging::lei_entity_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::lei_entity_lookup
wire_traits<ores::dq::messaging::lei_entity_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::lei_entity_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::lei_entity_lookup>::write_msgpack(
    const ores::dq::messaging::lei_entity_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::lei_entity_lookup
wire_traits<ores::dq::messaging::lei_entity_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::lei_entity_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::lei_entities_filter>::write_json(
    const ores::dq::messaging::lei_entities_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::lei_entities_filter
wire_traits<ores::dq::messaging::lei_entities_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::lei_entities_filter>(text);
}

std::string wire_traits<ores::dq::messaging::lei_entities_filter>::write_msgpack(
    const ores::dq::messaging::lei_entities_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::lei_entities_filter
wire_traits<ores::dq::messaging::lei_entities_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::lei_entities_filter>(data);
}

std::string wire_traits<ores::dq::messaging::lei_entity_event>::write_json(
    const ores::dq::messaging::lei_entity_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::lei_entity_event
wire_traits<ores::dq::messaging::lei_entity_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::lei_entity_event>(text);
}

std::string wire_traits<ores::dq::messaging::lei_entity_event>::write_msgpack(
    const ores::dq::messaging::lei_entity_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::lei_entity_event
wire_traits<ores::dq::messaging::lei_entity_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::lei_entity_event>(data);
}

std::string wire_traits<ores::dq::messaging::list_lei_entities_request>::write_json(
    const ores::dq::messaging::list_lei_entities_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_lei_entities_request
wire_traits<ores::dq::messaging::list_lei_entities_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_lei_entities_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_lei_entities_request>::write_msgpack(
    const ores::dq::messaging::list_lei_entities_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_lei_entities_request
wire_traits<ores::dq::messaging::list_lei_entities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_lei_entities_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_lei_entities_response>::write_json(
    const ores::dq::messaging::list_lei_entities_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_lei_entities_response
wire_traits<ores::dq::messaging::list_lei_entities_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_lei_entities_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_lei_entities_response>::write_msgpack(
    const ores::dq::messaging::list_lei_entities_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_lei_entities_response
wire_traits<ores::dq::messaging::list_lei_entities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_lei_entities_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_lei_entity_request>::write_json(
    const ores::dq::messaging::get_lei_entity_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_lei_entity_request
wire_traits<ores::dq::messaging::get_lei_entity_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_lei_entity_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_lei_entity_request>::write_msgpack(
    const ores::dq::messaging::get_lei_entity_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_lei_entity_request
wire_traits<ores::dq::messaging::get_lei_entity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_lei_entity_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_lei_entity_response>::write_json(
    const ores::dq::messaging::get_lei_entity_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_lei_entity_response
wire_traits<ores::dq::messaging::get_lei_entity_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_lei_entity_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_lei_entity_response>::write_msgpack(
    const ores::dq::messaging::get_lei_entity_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_lei_entity_response
wire_traits<ores::dq::messaging::get_lei_entity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_lei_entity_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_lei_entities_request>::write_json(
    const ores::dq::messaging::get_many_lei_entities_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_lei_entities_request
wire_traits<ores::dq::messaging::get_many_lei_entities_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_lei_entities_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_lei_entities_request>::write_msgpack(
    const ores::dq::messaging::get_many_lei_entities_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_lei_entities_request
wire_traits<ores::dq::messaging::get_many_lei_entities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_lei_entities_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_lei_entities_response>::write_json(
    const ores::dq::messaging::get_many_lei_entities_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_lei_entities_response
wire_traits<ores::dq::messaging::get_many_lei_entities_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_lei_entities_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_lei_entities_response>::write_msgpack(
    const ores::dq::messaging::get_many_lei_entities_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_lei_entities_response
wire_traits<ores::dq::messaging::get_many_lei_entities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_lei_entities_response>(data);
}

}
