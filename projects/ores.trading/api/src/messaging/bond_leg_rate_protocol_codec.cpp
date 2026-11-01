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
#include "ores.trading.api/domain/bond_leg_rate_codec.hpp"
#include "ores.trading.api/messaging/bond_leg_rate_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::bond_leg_rate_key>::write_json(
    const ores::trading::messaging::bond_leg_rate_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_key
wire_traits<ores::trading::messaging::bond_leg_rate_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_key>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_key>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_key
wire_traits<ores::trading::messaging::bond_leg_rate_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_key>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_write>::write_json(
    const ores::trading::messaging::bond_leg_rate_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_write
wire_traits<ores::trading::messaging::bond_leg_rate_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_write>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_write>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_write
wire_traits<ores::trading::messaging::bond_leg_rate_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_write>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_change>::write_json(
    const ores::trading::messaging::bond_leg_rate_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_change
wire_traits<ores::trading::messaging::bond_leg_rate_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_change>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_change>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_change
wire_traits<ores::trading::messaging::bond_leg_rate_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_change>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_removal>::write_json(
    const ores::trading::messaging::bond_leg_rate_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_removal
wire_traits<ores::trading::messaging::bond_leg_rate_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_removal>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_removal>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_removal
wire_traits<ores::trading::messaging::bond_leg_rate_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_removal>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_lookup>::write_json(
    const ores::trading::messaging::bond_leg_rate_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_lookup
wire_traits<ores::trading::messaging::bond_leg_rate_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_lookup>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_lookup
wire_traits<ores::trading::messaging::bond_leg_rate_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rates_filter>::write_json(
    const ores::trading::messaging::bond_leg_rates_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rates_filter
wire_traits<ores::trading::messaging::bond_leg_rates_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rates_filter>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rates_filter>::write_msgpack(
    const ores::trading::messaging::bond_leg_rates_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rates_filter
wire_traits<ores::trading::messaging::bond_leg_rates_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rates_filter>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_event>::write_json(
    const ores::trading::messaging::bond_leg_rate_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_event
wire_traits<ores::trading::messaging::bond_leg_rate_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_event>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_event>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_event
wire_traits<ores::trading::messaging::bond_leg_rate_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_event>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_version_key>::write_json(
    const ores::trading::messaging::bond_leg_rate_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_version_key
wire_traits<ores::trading::messaging::bond_leg_rate_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_version_key>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_version_key
wire_traits<ores::trading::messaging::bond_leg_rate_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_versions_filter>::write_json(
    const ores::trading::messaging::bond_leg_rate_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::bond_leg_rate_versions_filter
wire_traits<ores::trading::messaging::bond_leg_rate_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::bond_leg_rate_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::bond_leg_rate_versions_filter>::write_msgpack(
    const ores::trading::messaging::bond_leg_rate_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::bond_leg_rate_versions_filter
wire_traits<ores::trading::messaging::bond_leg_rate_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::bond_leg_rate_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rates_request>::write_json(
    const ores::trading::messaging::list_bond_leg_rates_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_leg_rates_request
wire_traits<ores::trading::messaging::list_bond_leg_rates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_leg_rates_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rates_request>::write_msgpack(
    const ores::trading::messaging::list_bond_leg_rates_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_leg_rates_request
wire_traits<ores::trading::messaging::list_bond_leg_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_leg_rates_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rates_response>::write_json(
    const ores::trading::messaging::list_bond_leg_rates_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_leg_rates_response
wire_traits<ores::trading::messaging::list_bond_leg_rates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_leg_rates_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rates_response>::write_msgpack(
    const ores::trading::messaging::list_bond_leg_rates_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_leg_rates_response
wire_traits<ores::trading::messaging::list_bond_leg_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_leg_rates_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_request>::write_json(
    const ores::trading::messaging::get_bond_leg_rate_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_leg_rate_request
wire_traits<ores::trading::messaging::get_bond_leg_rate_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_leg_rate_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_request>::write_msgpack(
    const ores::trading::messaging::get_bond_leg_rate_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_leg_rate_request
wire_traits<ores::trading::messaging::get_bond_leg_rate_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_leg_rate_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_response>::write_json(
    const ores::trading::messaging::get_bond_leg_rate_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_leg_rate_response
wire_traits<ores::trading::messaging::get_bond_leg_rate_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_leg_rate_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_response>::write_msgpack(
    const ores::trading::messaging::get_bond_leg_rate_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_leg_rate_response
wire_traits<ores::trading::messaging::get_bond_leg_rate_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_leg_rate_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_leg_rates_request>::write_json(
    const ores::trading::messaging::get_many_bond_leg_rates_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::get_many_bond_leg_rates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_bond_leg_rates_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_leg_rates_request>::write_msgpack(
    const ores::trading::messaging::get_many_bond_leg_rates_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::get_many_bond_leg_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_bond_leg_rates_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_leg_rates_response>::write_json(
    const ores::trading::messaging::get_many_bond_leg_rates_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::get_many_bond_leg_rates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_bond_leg_rates_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_bond_leg_rates_response>::write_msgpack(
    const ores::trading::messaging::get_many_bond_leg_rates_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::get_many_bond_leg_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_bond_leg_rates_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_leg_rate_request>::write_json(
    const ores::trading::messaging::put_bond_leg_rate_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_leg_rate_request
wire_traits<ores::trading::messaging::put_bond_leg_rate_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_leg_rate_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_leg_rate_request>::write_msgpack(
    const ores::trading::messaging::put_bond_leg_rate_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_leg_rate_request
wire_traits<ores::trading::messaging::put_bond_leg_rate_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_leg_rate_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_bond_leg_rate_response>::write_json(
    const ores::trading::messaging::put_bond_leg_rate_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_bond_leg_rate_response
wire_traits<ores::trading::messaging::put_bond_leg_rate_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_bond_leg_rate_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_bond_leg_rate_response>::write_msgpack(
    const ores::trading::messaging::put_bond_leg_rate_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_bond_leg_rate_response
wire_traits<ores::trading::messaging::put_bond_leg_rate_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_bond_leg_rate_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_leg_rates_request>::write_json(
    const ores::trading::messaging::put_many_bond_leg_rates_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::put_many_bond_leg_rates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_bond_leg_rates_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_leg_rates_request>::write_msgpack(
    const ores::trading::messaging::put_many_bond_leg_rates_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::put_many_bond_leg_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_bond_leg_rates_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_leg_rates_response>::write_json(
    const ores::trading::messaging::put_many_bond_leg_rates_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::put_many_bond_leg_rates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_bond_leg_rates_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_bond_leg_rates_response>::write_msgpack(
    const ores::trading::messaging::put_many_bond_leg_rates_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::put_many_bond_leg_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_bond_leg_rates_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_bond_leg_rate_request>::write_json(
    const ores::trading::messaging::delete_bond_leg_rate_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_bond_leg_rate_request
wire_traits<ores::trading::messaging::delete_bond_leg_rate_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_bond_leg_rate_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_bond_leg_rate_request>::write_msgpack(
    const ores::trading::messaging::delete_bond_leg_rate_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_bond_leg_rate_request
wire_traits<ores::trading::messaging::delete_bond_leg_rate_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_bond_leg_rate_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_bond_leg_rate_response>::write_json(
    const ores::trading::messaging::delete_bond_leg_rate_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_bond_leg_rate_response
wire_traits<ores::trading::messaging::delete_bond_leg_rate_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_bond_leg_rate_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_bond_leg_rate_response>::write_msgpack(
    const ores::trading::messaging::delete_bond_leg_rate_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_bond_leg_rate_response
wire_traits<ores::trading::messaging::delete_bond_leg_rate_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_bond_leg_rate_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_request>::write_json(
    const ores::trading::messaging::delete_many_bond_leg_rates_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_bond_leg_rates_request>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_request>::write_msgpack(
    const ores::trading::messaging::delete_many_bond_leg_rates_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_bond_leg_rates_request
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_bond_leg_rates_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_response>::write_json(
    const ores::trading::messaging::delete_many_bond_leg_rates_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_bond_leg_rates_response>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_response>::write_msgpack(
    const ores::trading::messaging::delete_many_bond_leg_rates_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_bond_leg_rates_response
wire_traits<ores::trading::messaging::delete_many_bond_leg_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_bond_leg_rates_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>::write_json(
    const ores::trading::messaging::list_by_trade_id_bond_leg_rates_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_bond_leg_rates_request
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_bond_leg_rates_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_bond_leg_rates_request
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_by_trade_id_bond_leg_rates_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>::write_json(
    const ores::trading::messaging::list_by_trade_id_bond_leg_rates_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_bond_leg_rates_response
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_bond_leg_rates_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_bond_leg_rates_response
wire_traits<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_by_trade_id_bond_leg_rates_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_request>::write_json(
    const ores::trading::messaging::list_bond_leg_rate_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_leg_rate_versions_request
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_leg_rate_versions_request>(text);
}

std::string
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_request>::write_msgpack(
    const ores::trading::messaging::list_bond_leg_rate_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_leg_rate_versions_request
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_leg_rate_versions_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_response>::write_json(
    const ores::trading::messaging::list_bond_leg_rate_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_bond_leg_rate_versions_response
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_bond_leg_rate_versions_response>(text);
}

std::string
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_response>::write_msgpack(
    const ores::trading::messaging::list_bond_leg_rate_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_bond_leg_rate_versions_response
wire_traits<ores::trading::messaging::list_bond_leg_rate_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_bond_leg_rate_versions_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_version_request>::write_json(
    const ores::trading::messaging::get_bond_leg_rate_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_leg_rate_version_request
wire_traits<ores::trading::messaging::get_bond_leg_rate_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_leg_rate_version_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_version_request>::write_msgpack(
    const ores::trading::messaging::get_bond_leg_rate_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_leg_rate_version_request
wire_traits<ores::trading::messaging::get_bond_leg_rate_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_leg_rate_version_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_bond_leg_rate_version_response>::write_json(
    const ores::trading::messaging::get_bond_leg_rate_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_bond_leg_rate_version_response
wire_traits<ores::trading::messaging::get_bond_leg_rate_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_bond_leg_rate_version_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_bond_leg_rate_version_response>::write_msgpack(
    const ores::trading::messaging::get_bond_leg_rate_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_bond_leg_rate_version_response
wire_traits<ores::trading::messaging::get_bond_leg_rate_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_bond_leg_rate_version_response>(data);
}

}
