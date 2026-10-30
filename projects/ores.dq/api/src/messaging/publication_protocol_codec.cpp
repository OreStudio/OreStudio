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
#include "ores.dq.api/domain/publication_codec.hpp"
#include "ores.dq.api/messaging/publication_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::dq::messaging::publication_key>::write_json(
    const ores::dq::messaging::publication_key& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_key
wire_traits<ores::dq::messaging::publication_key>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_key>(text);
}

std::string wire_traits<ores::dq::messaging::publication_key>::write_msgpack(
    const ores::dq::messaging::publication_key& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_key
wire_traits<ores::dq::messaging::publication_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_key>(data);
}

std::string wire_traits<ores::dq::messaging::publication_write>::write_json(
    const ores::dq::messaging::publication_write& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_write
wire_traits<ores::dq::messaging::publication_write>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_write>(text);
}

std::string wire_traits<ores::dq::messaging::publication_write>::write_msgpack(
    const ores::dq::messaging::publication_write& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_write
wire_traits<ores::dq::messaging::publication_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_write>(data);
}

std::string wire_traits<ores::dq::messaging::publication_change>::write_json(
    const ores::dq::messaging::publication_change& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_change
wire_traits<ores::dq::messaging::publication_change>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_change>(text);
}

std::string wire_traits<ores::dq::messaging::publication_change>::write_msgpack(
    const ores::dq::messaging::publication_change& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_change
wire_traits<ores::dq::messaging::publication_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_change>(data);
}

std::string wire_traits<ores::dq::messaging::publication_removal>::write_json(
    const ores::dq::messaging::publication_removal& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_removal
wire_traits<ores::dq::messaging::publication_removal>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_removal>(text);
}

std::string wire_traits<ores::dq::messaging::publication_removal>::write_msgpack(
    const ores::dq::messaging::publication_removal& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_removal
wire_traits<ores::dq::messaging::publication_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_removal>(data);
}

std::string wire_traits<ores::dq::messaging::publication_lookup>::write_json(
    const ores::dq::messaging::publication_lookup& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_lookup
wire_traits<ores::dq::messaging::publication_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_lookup>(text);
}

std::string wire_traits<ores::dq::messaging::publication_lookup>::write_msgpack(
    const ores::dq::messaging::publication_lookup& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_lookup
wire_traits<ores::dq::messaging::publication_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_lookup>(data);
}

std::string wire_traits<ores::dq::messaging::publications_filter>::write_json(
    const ores::dq::messaging::publications_filter& v) {
    return object::write_json(v);
}

ores::dq::messaging::publications_filter
wire_traits<ores::dq::messaging::publications_filter>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publications_filter>(text);
}

std::string wire_traits<ores::dq::messaging::publications_filter>::write_msgpack(
    const ores::dq::messaging::publications_filter& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publications_filter
wire_traits<ores::dq::messaging::publications_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publications_filter>(data);
}

std::string wire_traits<ores::dq::messaging::publication_event>::write_json(
    const ores::dq::messaging::publication_event& v) {
    return object::write_json(v);
}

ores::dq::messaging::publication_event
wire_traits<ores::dq::messaging::publication_event>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::publication_event>(text);
}

std::string wire_traits<ores::dq::messaging::publication_event>::write_msgpack(
    const ores::dq::messaging::publication_event& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::publication_event
wire_traits<ores::dq::messaging::publication_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::publication_event>(data);
}

std::string wire_traits<ores::dq::messaging::list_publications_request>::write_json(
    const ores::dq::messaging::list_publications_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_publications_request
wire_traits<ores::dq::messaging::list_publications_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_publications_request>(text);
}

std::string wire_traits<ores::dq::messaging::list_publications_request>::write_msgpack(
    const ores::dq::messaging::list_publications_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_publications_request
wire_traits<ores::dq::messaging::list_publications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_publications_request>(data);
}

std::string wire_traits<ores::dq::messaging::list_publications_response>::write_json(
    const ores::dq::messaging::list_publications_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::list_publications_response
wire_traits<ores::dq::messaging::list_publications_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::list_publications_response>(text);
}

std::string wire_traits<ores::dq::messaging::list_publications_response>::write_msgpack(
    const ores::dq::messaging::list_publications_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::list_publications_response
wire_traits<ores::dq::messaging::list_publications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::list_publications_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_publication_request>::write_json(
    const ores::dq::messaging::get_publication_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_publication_request
wire_traits<ores::dq::messaging::get_publication_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_publication_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_publication_request>::write_msgpack(
    const ores::dq::messaging::get_publication_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_publication_request
wire_traits<ores::dq::messaging::get_publication_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_publication_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_publication_response>::write_json(
    const ores::dq::messaging::get_publication_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_publication_response
wire_traits<ores::dq::messaging::get_publication_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_publication_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_publication_response>::write_msgpack(
    const ores::dq::messaging::get_publication_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_publication_response
wire_traits<ores::dq::messaging::get_publication_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_publication_response>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_publications_request>::write_json(
    const ores::dq::messaging::get_many_publications_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_publications_request
wire_traits<ores::dq::messaging::get_many_publications_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_publications_request>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_publications_request>::write_msgpack(
    const ores::dq::messaging::get_many_publications_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_publications_request
wire_traits<ores::dq::messaging::get_many_publications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_publications_request>(data);
}

std::string wire_traits<ores::dq::messaging::get_many_publications_response>::write_json(
    const ores::dq::messaging::get_many_publications_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::get_many_publications_response
wire_traits<ores::dq::messaging::get_many_publications_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::get_many_publications_response>(text);
}

std::string wire_traits<ores::dq::messaging::get_many_publications_response>::write_msgpack(
    const ores::dq::messaging::get_many_publications_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::get_many_publications_response
wire_traits<ores::dq::messaging::get_many_publications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::get_many_publications_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_publication_request>::write_json(
    const ores::dq::messaging::put_publication_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_publication_request
wire_traits<ores::dq::messaging::put_publication_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_publication_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_publication_request>::write_msgpack(
    const ores::dq::messaging::put_publication_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_publication_request
wire_traits<ores::dq::messaging::put_publication_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_publication_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_publication_response>::write_json(
    const ores::dq::messaging::put_publication_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_publication_response
wire_traits<ores::dq::messaging::put_publication_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_publication_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_publication_response>::write_msgpack(
    const ores::dq::messaging::put_publication_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_publication_response
wire_traits<ores::dq::messaging::put_publication_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_publication_response>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_publications_request>::write_json(
    const ores::dq::messaging::put_many_publications_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_publications_request
wire_traits<ores::dq::messaging::put_many_publications_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_publications_request>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_publications_request>::write_msgpack(
    const ores::dq::messaging::put_many_publications_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_publications_request
wire_traits<ores::dq::messaging::put_many_publications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_publications_request>(data);
}

std::string wire_traits<ores::dq::messaging::put_many_publications_response>::write_json(
    const ores::dq::messaging::put_many_publications_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::put_many_publications_response
wire_traits<ores::dq::messaging::put_many_publications_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::put_many_publications_response>(text);
}

std::string wire_traits<ores::dq::messaging::put_many_publications_response>::write_msgpack(
    const ores::dq::messaging::put_many_publications_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::put_many_publications_response
wire_traits<ores::dq::messaging::put_many_publications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::put_many_publications_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_publication_request>::write_json(
    const ores::dq::messaging::delete_publication_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_publication_request
wire_traits<ores::dq::messaging::delete_publication_request>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_publication_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_publication_request>::write_msgpack(
    const ores::dq::messaging::delete_publication_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_publication_request
wire_traits<ores::dq::messaging::delete_publication_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_publication_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_publication_response>::write_json(
    const ores::dq::messaging::delete_publication_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_publication_response
wire_traits<ores::dq::messaging::delete_publication_response>::read_json(std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_publication_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_publication_response>::write_msgpack(
    const ores::dq::messaging::delete_publication_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_publication_response
wire_traits<ores::dq::messaging::delete_publication_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_publication_response>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_publications_request>::write_json(
    const ores::dq::messaging::delete_many_publications_request& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_publications_request
wire_traits<ores::dq::messaging::delete_many_publications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_publications_request>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_publications_request>::write_msgpack(
    const ores::dq::messaging::delete_many_publications_request& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_publications_request
wire_traits<ores::dq::messaging::delete_many_publications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_publications_request>(data);
}

std::string wire_traits<ores::dq::messaging::delete_many_publications_response>::write_json(
    const ores::dq::messaging::delete_many_publications_response& v) {
    return object::write_json(v);
}

ores::dq::messaging::delete_many_publications_response
wire_traits<ores::dq::messaging::delete_many_publications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::dq::messaging::delete_many_publications_response>(text);
}

std::string wire_traits<ores::dq::messaging::delete_many_publications_response>::write_msgpack(
    const ores::dq::messaging::delete_many_publications_response& v) {
    return object::write_msgpack(v);
}

ores::dq::messaging::delete_many_publications_response
wire_traits<ores::dq::messaging::delete_many_publications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::dq::messaging::delete_many_publications_response>(data);
}

}
