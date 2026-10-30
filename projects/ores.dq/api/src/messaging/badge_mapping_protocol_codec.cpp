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
#include "ores.dq.api/domain/badge_mapping_codec.hpp"
#include "ores.dq.api/messaging/badge_mapping_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::badge_mapping_key>::write_json(
    const ores::dq::messaging::badge_mapping_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_key
wire_traits<ores::dq::messaging::badge_mapping_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_key>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_key>::write_msgpack(
    const ores::dq::messaging::badge_mapping_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_key
wire_traits<ores::dq::messaging::badge_mapping_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_key>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_write>::write_json(
    const ores::dq::messaging::badge_mapping_write& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_write
wire_traits<ores::dq::messaging::badge_mapping_write>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_write>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_write>::write_msgpack(
    const ores::dq::messaging::badge_mapping_write& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_write
wire_traits<ores::dq::messaging::badge_mapping_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_write>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_change>::write_json(
    const ores::dq::messaging::badge_mapping_change& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_change
wire_traits<ores::dq::messaging::badge_mapping_change>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_change>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_change>::write_msgpack(
    const ores::dq::messaging::badge_mapping_change& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_change
wire_traits<ores::dq::messaging::badge_mapping_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_change>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_removal>::write_json(
    const ores::dq::messaging::badge_mapping_removal& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_removal
wire_traits<ores::dq::messaging::badge_mapping_removal>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_removal>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_removal>::write_msgpack(
    const ores::dq::messaging::badge_mapping_removal& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_removal
wire_traits<ores::dq::messaging::badge_mapping_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_removal>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_lookup>::write_json(
    const ores::dq::messaging::badge_mapping_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_lookup
wire_traits<ores::dq::messaging::badge_mapping_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_lookup>::write_msgpack(
    const ores::dq::messaging::badge_mapping_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_lookup
wire_traits<ores::dq::messaging::badge_mapping_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mappings_filter>::write_json(
    const ores::dq::messaging::badge_mappings_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mappings_filter
wire_traits<ores::dq::messaging::badge_mappings_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mappings_filter>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mappings_filter>::write_msgpack(
    const ores::dq::messaging::badge_mappings_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mappings_filter
wire_traits<ores::dq::messaging::badge_mappings_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mappings_filter>(data);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_event>::write_json(
    const ores::dq::messaging::badge_mapping_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::badge_mapping_event
wire_traits<ores::dq::messaging::badge_mapping_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::badge_mapping_event>(text);
}

std::string wire_traits<ores::dq::messaging::badge_mapping_event>::write_msgpack(
    const ores::dq::messaging::badge_mapping_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::badge_mapping_event
wire_traits<ores::dq::messaging::badge_mapping_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::badge_mapping_event>(data);
}

std::string wire_traits<ores::dq::messaging::list_badge_mappings_request>::write_json(
    const ores::dq::messaging::list_badge_mappings_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_badge_mappings_request
wire_traits<ores::dq::messaging::list_badge_mappings_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_badge_mappings_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_badge_mappings_request>::write_msgpack(
    const ores::dq::messaging::list_badge_mappings_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_badge_mappings_request
wire_traits<ores::dq::messaging::list_badge_mappings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_badge_mappings_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_badge_mappings_response>::write_json(
    const ores::dq::messaging::list_badge_mappings_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_badge_mappings_response
wire_traits<ores::dq::messaging::list_badge_mappings_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_badge_mappings_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_badge_mappings_response>::write_msgpack(
    const ores::dq::messaging::list_badge_mappings_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_badge_mappings_response
wire_traits<ores::dq::messaging::list_badge_mappings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_badge_mappings_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_badge_mapping_request>::write_json(
    const ores::dq::messaging::get_badge_mapping_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_badge_mapping_request
wire_traits<ores::dq::messaging::get_badge_mapping_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_badge_mapping_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_badge_mapping_request>::write_msgpack(
    const ores::dq::messaging::get_badge_mapping_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_badge_mapping_request
wire_traits<ores::dq::messaging::get_badge_mapping_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_badge_mapping_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_badge_mapping_response>::write_json(
    const ores::dq::messaging::get_badge_mapping_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_badge_mapping_response
wire_traits<ores::dq::messaging::get_badge_mapping_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_badge_mapping_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_badge_mapping_response>::write_msgpack(
    const ores::dq::messaging::get_badge_mapping_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_badge_mapping_response
wire_traits<ores::dq::messaging::get_badge_mapping_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_badge_mapping_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_badge_mappings_request>::write_json(
    const ores::dq::messaging::get_many_badge_mappings_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_badge_mappings_request
wire_traits<ores::dq::messaging::get_many_badge_mappings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_badge_mappings_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_badge_mappings_request>::write_msgpack(
    const ores::dq::messaging::get_many_badge_mappings_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_badge_mappings_request
wire_traits<ores::dq::messaging::get_many_badge_mappings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_badge_mappings_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_badge_mappings_response>::write_json(
    const ores::dq::messaging::get_many_badge_mappings_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_badge_mappings_response
wire_traits<ores::dq::messaging::get_many_badge_mappings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_badge_mappings_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_badge_mappings_response>::write_msgpack(
    const ores::dq::messaging::get_many_badge_mappings_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_badge_mappings_response
wire_traits<ores::dq::messaging::get_many_badge_mappings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_badge_mappings_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_badge_mapping_request>::write_json(
    const ores::dq::messaging::put_badge_mapping_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_badge_mapping_request
wire_traits<ores::dq::messaging::put_badge_mapping_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_badge_mapping_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_badge_mapping_request>::write_msgpack(
    const ores::dq::messaging::put_badge_mapping_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_badge_mapping_request
wire_traits<ores::dq::messaging::put_badge_mapping_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_badge_mapping_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_badge_mapping_response>::write_json(
    const ores::dq::messaging::put_badge_mapping_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_badge_mapping_response
wire_traits<ores::dq::messaging::put_badge_mapping_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_badge_mapping_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_badge_mapping_response>::write_msgpack(
    const ores::dq::messaging::put_badge_mapping_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_badge_mapping_response
wire_traits<ores::dq::messaging::put_badge_mapping_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_badge_mapping_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_badge_mappings_request>::write_json(
    const ores::dq::messaging::put_many_badge_mappings_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_badge_mappings_request
wire_traits<ores::dq::messaging::put_many_badge_mappings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_badge_mappings_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_badge_mappings_request>::write_msgpack(
    const ores::dq::messaging::put_many_badge_mappings_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_badge_mappings_request
wire_traits<ores::dq::messaging::put_many_badge_mappings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_badge_mappings_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_badge_mappings_response>::write_json(
    const ores::dq::messaging::put_many_badge_mappings_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_badge_mappings_response
wire_traits<ores::dq::messaging::put_many_badge_mappings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_badge_mappings_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_badge_mappings_response>::write_msgpack(
    const ores::dq::messaging::put_many_badge_mappings_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_badge_mappings_response
wire_traits<ores::dq::messaging::put_many_badge_mappings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_badge_mappings_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_badge_mapping_request>::write_json(
    const ores::dq::messaging::delete_badge_mapping_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_badge_mapping_request
wire_traits<ores::dq::messaging::delete_badge_mapping_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_badge_mapping_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_badge_mapping_request>::write_msgpack(
    const ores::dq::messaging::delete_badge_mapping_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_badge_mapping_request
wire_traits<ores::dq::messaging::delete_badge_mapping_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_badge_mapping_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_badge_mapping_response>::write_json(
    const ores::dq::messaging::delete_badge_mapping_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_badge_mapping_response
wire_traits<ores::dq::messaging::delete_badge_mapping_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_badge_mapping_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_badge_mapping_response>::write_msgpack(
    const ores::dq::messaging::delete_badge_mapping_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_badge_mapping_response
wire_traits<ores::dq::messaging::delete_badge_mapping_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_badge_mapping_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_badge_mappings_request>::write_json(
    const ores::dq::messaging::delete_many_badge_mappings_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_badge_mappings_request
wire_traits<ores::dq::messaging::delete_many_badge_mappings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_badge_mappings_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_badge_mappings_request>::write_msgpack(
    const ores::dq::messaging::delete_many_badge_mappings_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_badge_mappings_request
wire_traits<ores::dq::messaging::delete_many_badge_mappings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_badge_mappings_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_badge_mappings_response>::write_json(
    const ores::dq::messaging::delete_many_badge_mappings_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_badge_mappings_response
wire_traits<ores::dq::messaging::delete_many_badge_mappings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_badge_mappings_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_badge_mappings_response>::write_msgpack(
    const ores::dq::messaging::delete_many_badge_mappings_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_badge_mappings_response
wire_traits<ores::dq::messaging::delete_many_badge_mappings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_badge_mappings_response>(data);
}

std::string
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>::write_json(
    const ores::dq::messaging::list_by_code_domain_code_badge_mappings_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_by_code_domain_code_badge_mappings_request
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>(
        text);
}

std::string
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>::write_msgpack(
    const ores::dq::messaging::list_by_code_domain_code_badge_mappings_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_by_code_domain_code_badge_mappings_request
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::dq::messaging::list_by_code_domain_code_badge_mappings_request>(data);
}

std::string
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>::write_json(
    const ores::dq::messaging::list_by_code_domain_code_badge_mappings_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_by_code_domain_code_badge_mappings_response
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>(
        text);
}

std::string
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>::write_msgpack(
    const ores::dq::messaging::list_by_code_domain_code_badge_mappings_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_by_code_domain_code_badge_mappings_response
wire_traits<ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::dq::messaging::list_by_code_domain_code_badge_mappings_response>(data);
}

}
