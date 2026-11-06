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
#include "ores.trading.api/domain/swap_leg_indexing_codec.hpp"
#include "ores.trading.api/messaging/swap_leg_indexing_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_key>::write_json(
    const ores::trading::messaging::swap_leg_indexing_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_key
wire_traits<ores::trading::messaging::swap_leg_indexing_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_key>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_key>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_key
wire_traits<ores::trading::messaging::swap_leg_indexing_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_key>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_write>::write_json(
    const ores::trading::messaging::swap_leg_indexing_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_write
wire_traits<ores::trading::messaging::swap_leg_indexing_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_write>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_write>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_write
wire_traits<ores::trading::messaging::swap_leg_indexing_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_write>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_change>::write_json(
    const ores::trading::messaging::swap_leg_indexing_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_change
wire_traits<ores::trading::messaging::swap_leg_indexing_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_change>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_change>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_change
wire_traits<ores::trading::messaging::swap_leg_indexing_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_change>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_removal>::write_json(
    const ores::trading::messaging::swap_leg_indexing_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_removal
wire_traits<ores::trading::messaging::swap_leg_indexing_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_removal>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_removal>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_removal
wire_traits<ores::trading::messaging::swap_leg_indexing_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_removal>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_lookup>::write_json(
    const ores::trading::messaging::swap_leg_indexing_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_lookup
wire_traits<ores::trading::messaging::swap_leg_indexing_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_lookup>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_lookup
wire_traits<ores::trading::messaging::swap_leg_indexing_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexings_filter>::write_json(
    const ores::trading::messaging::swap_leg_indexings_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexings_filter
wire_traits<ores::trading::messaging::swap_leg_indexings_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexings_filter>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexings_filter>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexings_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexings_filter
wire_traits<ores::trading::messaging::swap_leg_indexings_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexings_filter>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_event>::write_json(
    const ores::trading::messaging::swap_leg_indexing_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_event
wire_traits<ores::trading::messaging::swap_leg_indexing_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_event>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_event>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_event
wire_traits<ores::trading::messaging::swap_leg_indexing_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_event>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_version_key>::write_json(
    const ores::trading::messaging::swap_leg_indexing_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_version_key
wire_traits<ores::trading::messaging::swap_leg_indexing_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_version_key>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_version_key
wire_traits<ores::trading::messaging::swap_leg_indexing_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_versions_filter>::write_json(
    const ores::trading::messaging::swap_leg_indexing_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::swap_leg_indexing_versions_filter
wire_traits<ores::trading::messaging::swap_leg_indexing_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::swap_leg_indexing_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::swap_leg_indexing_versions_filter>::write_msgpack(
    const ores::trading::messaging::swap_leg_indexing_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::swap_leg_indexing_versions_filter
wire_traits<ores::trading::messaging::swap_leg_indexing_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::swap_leg_indexing_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_swap_leg_indexings_request>::write_json(
    const ores::trading::messaging::list_swap_leg_indexings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_swap_leg_indexings_request
wire_traits<ores::trading::messaging::list_swap_leg_indexings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_swap_leg_indexings_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_swap_leg_indexings_request>::write_msgpack(
    const ores::trading::messaging::list_swap_leg_indexings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_swap_leg_indexings_request
wire_traits<ores::trading::messaging::list_swap_leg_indexings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_swap_leg_indexings_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_swap_leg_indexings_response>::write_json(
    const ores::trading::messaging::list_swap_leg_indexings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_swap_leg_indexings_response
wire_traits<ores::trading::messaging::list_swap_leg_indexings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_swap_leg_indexings_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_swap_leg_indexings_response>::write_msgpack(
    const ores::trading::messaging::list_swap_leg_indexings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_swap_leg_indexings_response
wire_traits<ores::trading::messaging::list_swap_leg_indexings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_swap_leg_indexings_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_swap_leg_indexing_request>::write_json(
    const ores::trading::messaging::get_swap_leg_indexing_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_swap_leg_indexing_request
wire_traits<ores::trading::messaging::get_swap_leg_indexing_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_swap_leg_indexing_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_swap_leg_indexing_request>::write_msgpack(
    const ores::trading::messaging::get_swap_leg_indexing_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_swap_leg_indexing_request
wire_traits<ores::trading::messaging::get_swap_leg_indexing_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_swap_leg_indexing_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_swap_leg_indexing_response>::write_json(
    const ores::trading::messaging::get_swap_leg_indexing_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_swap_leg_indexing_response
wire_traits<ores::trading::messaging::get_swap_leg_indexing_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_swap_leg_indexing_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_swap_leg_indexing_response>::write_msgpack(
    const ores::trading::messaging::get_swap_leg_indexing_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_swap_leg_indexing_response
wire_traits<ores::trading::messaging::get_swap_leg_indexing_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_swap_leg_indexing_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_request>::write_json(
    const ores::trading::messaging::get_many_swap_leg_indexings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_swap_leg_indexings_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_request>::write_msgpack(
    const ores::trading::messaging::get_many_swap_leg_indexings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_swap_leg_indexings_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_response>::write_json(
    const ores::trading::messaging::get_many_swap_leg_indexings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_swap_leg_indexings_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_response>::write_msgpack(
    const ores::trading::messaging::get_many_swap_leg_indexings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::get_many_swap_leg_indexings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_swap_leg_indexings_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::put_swap_leg_indexing_request>::write_json(
    const ores::trading::messaging::put_swap_leg_indexing_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_swap_leg_indexing_request
wire_traits<ores::trading::messaging::put_swap_leg_indexing_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_swap_leg_indexing_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_swap_leg_indexing_request>::write_msgpack(
    const ores::trading::messaging::put_swap_leg_indexing_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_swap_leg_indexing_request
wire_traits<ores::trading::messaging::put_swap_leg_indexing_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_swap_leg_indexing_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_swap_leg_indexing_response>::write_json(
    const ores::trading::messaging::put_swap_leg_indexing_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_swap_leg_indexing_response
wire_traits<ores::trading::messaging::put_swap_leg_indexing_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_swap_leg_indexing_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_swap_leg_indexing_response>::write_msgpack(
    const ores::trading::messaging::put_swap_leg_indexing_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_swap_leg_indexing_response
wire_traits<ores::trading::messaging::put_swap_leg_indexing_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_swap_leg_indexing_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_request>::write_json(
    const ores::trading::messaging::put_many_swap_leg_indexings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_swap_leg_indexings_request>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_request>::write_msgpack(
    const ores::trading::messaging::put_many_swap_leg_indexings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_swap_leg_indexings_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_response>::write_json(
    const ores::trading::messaging::put_many_swap_leg_indexings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_swap_leg_indexings_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_response>::write_msgpack(
    const ores::trading::messaging::put_many_swap_leg_indexings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::put_many_swap_leg_indexings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_swap_leg_indexings_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::delete_swap_leg_indexing_request>::write_json(
    const ores::trading::messaging::delete_swap_leg_indexing_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_swap_leg_indexing_request
wire_traits<ores::trading::messaging::delete_swap_leg_indexing_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_swap_leg_indexing_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_swap_leg_indexing_request>::write_msgpack(
    const ores::trading::messaging::delete_swap_leg_indexing_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_swap_leg_indexing_request
wire_traits<ores::trading::messaging::delete_swap_leg_indexing_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_swap_leg_indexing_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_swap_leg_indexing_response>::write_json(
    const ores::trading::messaging::delete_swap_leg_indexing_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_swap_leg_indexing_response
wire_traits<ores::trading::messaging::delete_swap_leg_indexing_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_swap_leg_indexing_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_swap_leg_indexing_response>::write_msgpack(
    const ores::trading::messaging::delete_swap_leg_indexing_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_swap_leg_indexing_response
wire_traits<ores::trading::messaging::delete_swap_leg_indexing_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_swap_leg_indexing_response>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_request>::write_json(
    const ores::trading::messaging::delete_many_swap_leg_indexings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_swap_leg_indexings_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_request>::write_msgpack(
    const ores::trading::messaging::delete_many_swap_leg_indexings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_swap_leg_indexings_request
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_swap_leg_indexings_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_response>::write_json(
    const ores::trading::messaging::delete_many_swap_leg_indexings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_swap_leg_indexings_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_response>::write_msgpack(
    const ores::trading::messaging::delete_many_swap_leg_indexings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_swap_leg_indexings_response
wire_traits<ores::trading::messaging::delete_many_swap_leg_indexings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_swap_leg_indexings_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>::write_json(
    const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_swap_leg_indexings_request>(data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>::write_json(
    const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>(text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response
wire_traits<ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_swap_leg_indexings_response>(data);
}

std::string
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_request>::write_json(
    const ores::trading::messaging::list_swap_leg_indexing_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_swap_leg_indexing_versions_request
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_swap_leg_indexing_versions_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_request>::write_msgpack(
    const ores::trading::messaging::list_swap_leg_indexing_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_swap_leg_indexing_versions_request
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_swap_leg_indexing_versions_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_response>::write_json(
    const ores::trading::messaging::list_swap_leg_indexing_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_swap_leg_indexing_versions_response
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_swap_leg_indexing_versions_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_response>::write_msgpack(
    const ores::trading::messaging::list_swap_leg_indexing_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_swap_leg_indexing_versions_response
wire_traits<ores::trading::messaging::list_swap_leg_indexing_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_swap_leg_indexing_versions_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_request>::write_json(
    const ores::trading::messaging::get_swap_leg_indexing_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_swap_leg_indexing_version_request
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_swap_leg_indexing_version_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_request>::write_msgpack(
    const ores::trading::messaging::get_swap_leg_indexing_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_swap_leg_indexing_version_request
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_swap_leg_indexing_version_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_response>::write_json(
    const ores::trading::messaging::get_swap_leg_indexing_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_swap_leg_indexing_version_response
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_swap_leg_indexing_version_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_response>::write_msgpack(
    const ores::trading::messaging::get_swap_leg_indexing_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_swap_leg_indexing_version_response
wire_traits<ores::trading::messaging::get_swap_leg_indexing_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_swap_leg_indexing_version_response>(
        data);
}

}
