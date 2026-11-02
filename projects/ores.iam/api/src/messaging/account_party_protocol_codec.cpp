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
#include "ores.iam.api/domain/account_party_codec.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::account_party_key>::write_json(
    const ores::iam::messaging::account_party_key& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_key
wire_traits<ores::iam::messaging::account_party_key>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_key>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_key>::write_msgpack(
    const ores::iam::messaging::account_party_key& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_key
wire_traits<ores::iam::messaging::account_party_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_key>(data);
}

std::string wire_traits<ores::iam::messaging::account_party_write>::write_json(
    const ores::iam::messaging::account_party_write& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_write
wire_traits<ores::iam::messaging::account_party_write>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_write>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_write>::write_msgpack(
    const ores::iam::messaging::account_party_write& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_write
wire_traits<ores::iam::messaging::account_party_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_write>(data);
}

std::string wire_traits<ores::iam::messaging::account_party_change>::write_json(
    const ores::iam::messaging::account_party_change& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_change
wire_traits<ores::iam::messaging::account_party_change>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_change>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_change>::write_msgpack(
    const ores::iam::messaging::account_party_change& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_change
wire_traits<ores::iam::messaging::account_party_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_change>(data);
}

std::string wire_traits<ores::iam::messaging::account_party_removal>::write_json(
    const ores::iam::messaging::account_party_removal& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_removal
wire_traits<ores::iam::messaging::account_party_removal>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_removal>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_removal>::write_msgpack(
    const ores::iam::messaging::account_party_removal& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_removal
wire_traits<ores::iam::messaging::account_party_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_removal>(data);
}

std::string wire_traits<ores::iam::messaging::account_party_lookup>::write_json(
    const ores::iam::messaging::account_party_lookup& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_lookup
wire_traits<ores::iam::messaging::account_party_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_lookup>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_lookup>::write_msgpack(
    const ores::iam::messaging::account_party_lookup& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_lookup
wire_traits<ores::iam::messaging::account_party_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_lookup>(data);
}

std::string wire_traits<ores::iam::messaging::account_parties_filter>::write_json(
    const ores::iam::messaging::account_parties_filter& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_parties_filter
wire_traits<ores::iam::messaging::account_parties_filter>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_parties_filter>(text);
}

std::string wire_traits<ores::iam::messaging::account_parties_filter>::write_msgpack(
    const ores::iam::messaging::account_parties_filter& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_parties_filter
wire_traits<ores::iam::messaging::account_parties_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_parties_filter>(data);
}

std::string wire_traits<ores::iam::messaging::account_party_event>::write_json(
    const ores::iam::messaging::account_party_event& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_party_event
wire_traits<ores::iam::messaging::account_party_event>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_party_event>(text);
}

std::string wire_traits<ores::iam::messaging::account_party_event>::write_msgpack(
    const ores::iam::messaging::account_party_event& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_party_event
wire_traits<ores::iam::messaging::account_party_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_party_event>(data);
}

std::string wire_traits<ores::iam::messaging::list_account_parties_request>::write_json(
    const ores::iam::messaging::list_account_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_account_parties_request
wire_traits<ores::iam::messaging::list_account_parties_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_account_parties_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_account_parties_request>::write_msgpack(
    const ores::iam::messaging::list_account_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_account_parties_request
wire_traits<ores::iam::messaging::list_account_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_account_parties_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_account_parties_response>::write_json(
    const ores::iam::messaging::list_account_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_account_parties_response
wire_traits<ores::iam::messaging::list_account_parties_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_account_parties_response>(text);
}

std::string wire_traits<ores::iam::messaging::list_account_parties_response>::write_msgpack(
    const ores::iam::messaging::list_account_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_account_parties_response
wire_traits<ores::iam::messaging::list_account_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_account_parties_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_account_party_request>::write_json(
    const ores::iam::messaging::get_account_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_account_party_request
wire_traits<ores::iam::messaging::get_account_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_account_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_account_party_request>::write_msgpack(
    const ores::iam::messaging::get_account_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_account_party_request
wire_traits<ores::iam::messaging::get_account_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_account_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_account_party_response>::write_json(
    const ores::iam::messaging::get_account_party_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_account_party_response
wire_traits<ores::iam::messaging::get_account_party_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_account_party_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_account_party_response>::write_msgpack(
    const ores::iam::messaging::get_account_party_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_account_party_response
wire_traits<ores::iam::messaging::get_account_party_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_account_party_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_account_parties_request>::write_json(
    const ores::iam::messaging::get_many_account_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_account_parties_request
wire_traits<ores::iam::messaging::get_many_account_parties_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_account_parties_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_account_parties_request>::write_msgpack(
    const ores::iam::messaging::get_many_account_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_account_parties_request
wire_traits<ores::iam::messaging::get_many_account_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_account_parties_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_many_account_parties_response>::write_json(
    const ores::iam::messaging::get_many_account_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_many_account_parties_response
wire_traits<ores::iam::messaging::get_many_account_parties_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_many_account_parties_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_many_account_parties_response>::write_msgpack(
    const ores::iam::messaging::get_many_account_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_many_account_parties_response
wire_traits<ores::iam::messaging::get_many_account_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_many_account_parties_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_account_party_request>::write_json(
    const ores::iam::messaging::put_account_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_account_party_request
wire_traits<ores::iam::messaging::put_account_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_account_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_account_party_request>::write_msgpack(
    const ores::iam::messaging::put_account_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_account_party_request
wire_traits<ores::iam::messaging::put_account_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_account_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_account_party_response>::write_json(
    const ores::iam::messaging::put_account_party_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_account_party_response
wire_traits<ores::iam::messaging::put_account_party_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_account_party_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_account_party_response>::write_msgpack(
    const ores::iam::messaging::put_account_party_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_account_party_response
wire_traits<ores::iam::messaging::put_account_party_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_account_party_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_account_parties_request>::write_json(
    const ores::iam::messaging::put_many_account_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_account_parties_request
wire_traits<ores::iam::messaging::put_many_account_parties_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_account_parties_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_account_parties_request>::write_msgpack(
    const ores::iam::messaging::put_many_account_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_account_parties_request
wire_traits<ores::iam::messaging::put_many_account_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_account_parties_request>(data);
}

std::string wire_traits<ores::iam::messaging::put_many_account_parties_response>::write_json(
    const ores::iam::messaging::put_many_account_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_many_account_parties_response
wire_traits<ores::iam::messaging::put_many_account_parties_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::put_many_account_parties_response>(text);
}

std::string wire_traits<ores::iam::messaging::put_many_account_parties_response>::write_msgpack(
    const ores::iam::messaging::put_many_account_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_many_account_parties_response
wire_traits<ores::iam::messaging::put_many_account_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_many_account_parties_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_account_party_request>::write_json(
    const ores::iam::messaging::delete_account_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_account_party_request
wire_traits<ores::iam::messaging::delete_account_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_account_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_account_party_request>::write_msgpack(
    const ores::iam::messaging::delete_account_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_account_party_request
wire_traits<ores::iam::messaging::delete_account_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_account_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_account_party_response>::write_json(
    const ores::iam::messaging::delete_account_party_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_account_party_response
wire_traits<ores::iam::messaging::delete_account_party_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_account_party_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_account_party_response>::write_msgpack(
    const ores::iam::messaging::delete_account_party_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_account_party_response
wire_traits<ores::iam::messaging::delete_account_party_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_account_party_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_account_parties_request>::write_json(
    const ores::iam::messaging::delete_many_account_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_account_parties_request
wire_traits<ores::iam::messaging::delete_many_account_parties_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_account_parties_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_account_parties_request>::write_msgpack(
    const ores::iam::messaging::delete_many_account_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_account_parties_request
wire_traits<ores::iam::messaging::delete_many_account_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_account_parties_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_many_account_parties_response>::write_json(
    const ores::iam::messaging::delete_many_account_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_many_account_parties_response
wire_traits<ores::iam::messaging::delete_many_account_parties_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_many_account_parties_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_many_account_parties_response>::write_msgpack(
    const ores::iam::messaging::delete_many_account_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_many_account_parties_response
wire_traits<ores::iam::messaging::delete_many_account_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_many_account_parties_response>(data);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_request>::write_json(
    const ores::iam::messaging::list_by_account_id_account_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_account_id_account_parties_request
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_account_id_account_parties_request>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_request>::write_msgpack(
    const ores::iam::messaging::list_by_account_id_account_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_account_id_account_parties_request
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_account_id_account_parties_request>(
        data);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_response>::write_json(
    const ores::iam::messaging::list_by_account_id_account_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_by_account_id_account_parties_response
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_by_account_id_account_parties_response>(
        text);
}

std::string
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_response>::write_msgpack(
    const ores::iam::messaging::list_by_account_id_account_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_by_account_id_account_parties_response
wire_traits<ores::iam::messaging::list_by_account_id_account_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_by_account_id_account_parties_response>(
        data);
}

}
