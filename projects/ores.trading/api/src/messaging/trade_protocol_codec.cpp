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
#include "ores.trading.api/domain/booking_nature.hpp"
#include "ores.trading.api/domain/counterparty_scope.hpp"
#include "ores.trading.api/domain/entry_channel.hpp"
#include "ores.trading.api/domain/origin.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::trade_key>::write_json(
    const ores::trading::messaging::trade_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_key
wire_traits<ores::trading::messaging::trade_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_key>(text);
}

std::string wire_traits<ores::trading::messaging::trade_key>::write_msgpack(
    const ores::trading::messaging::trade_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_key
wire_traits<ores::trading::messaging::trade_key>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_key>(data);
}

std::string wire_traits<ores::trading::messaging::trade_write>::write_json(
    const ores::trading::messaging::trade_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_write
wire_traits<ores::trading::messaging::trade_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_write>(text);
}

std::string wire_traits<ores::trading::messaging::trade_write>::write_msgpack(
    const ores::trading::messaging::trade_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_write
wire_traits<ores::trading::messaging::trade_write>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_write>(data);
}

std::string wire_traits<ores::trading::messaging::trade_change>::write_json(
    const ores::trading::messaging::trade_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_change
wire_traits<ores::trading::messaging::trade_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_change>(text);
}

std::string wire_traits<ores::trading::messaging::trade_change>::write_msgpack(
    const ores::trading::messaging::trade_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_change
wire_traits<ores::trading::messaging::trade_change>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_change>(data);
}

std::string wire_traits<ores::trading::messaging::trade_removal>::write_json(
    const ores::trading::messaging::trade_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_removal
wire_traits<ores::trading::messaging::trade_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_removal>(text);
}

std::string wire_traits<ores::trading::messaging::trade_removal>::write_msgpack(
    const ores::trading::messaging::trade_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_removal
wire_traits<ores::trading::messaging::trade_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_removal>(data);
}

std::string wire_traits<ores::trading::messaging::trade_lookup>::write_json(
    const ores::trading::messaging::trade_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_lookup
wire_traits<ores::trading::messaging::trade_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::trade_lookup>::write_msgpack(
    const ores::trading::messaging::trade_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_lookup
wire_traits<ores::trading::messaging::trade_lookup>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::trades_filter>::write_json(
    const ores::trading::messaging::trades_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::trades_filter
wire_traits<ores::trading::messaging::trades_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trades_filter>(text);
}

std::string wire_traits<ores::trading::messaging::trades_filter>::write_msgpack(
    const ores::trading::messaging::trades_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trades_filter
wire_traits<ores::trading::messaging::trades_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trades_filter>(data);
}

std::string wire_traits<ores::trading::messaging::trade_event>::write_json(
    const ores::trading::messaging::trade_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_event
wire_traits<ores::trading::messaging::trade_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_event>(text);
}

std::string wire_traits<ores::trading::messaging::trade_event>::write_msgpack(
    const ores::trading::messaging::trade_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_event
wire_traits<ores::trading::messaging::trade_event>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_event>(data);
}

std::string wire_traits<ores::trading::messaging::trade_version_key>::write_json(
    const ores::trading::messaging::trade_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_version_key
wire_traits<ores::trading::messaging::trade_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::trade_version_key>::write_msgpack(
    const ores::trading::messaging::trade_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_version_key
wire_traits<ores::trading::messaging::trade_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::trade_versions_filter>::write_json(
    const ores::trading::messaging::trade_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_versions_filter
wire_traits<ores::trading::messaging::trade_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::trade_versions_filter>::write_msgpack(
    const ores::trading::messaging::trade_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_versions_filter
wire_traits<ores::trading::messaging::trade_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_trades_request>::write_json(
    const ores::trading::messaging::list_trades_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trades_request
wire_traits<ores::trading::messaging::list_trades_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trades_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_trades_request>::write_msgpack(
    const ores::trading::messaging::list_trades_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trades_request
wire_traits<ores::trading::messaging::list_trades_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trades_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_trades_response>::write_json(
    const ores::trading::messaging::list_trades_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trades_response
wire_traits<ores::trading::messaging::list_trades_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trades_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_trades_response>::write_msgpack(
    const ores::trading::messaging::list_trades_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trades_response
wire_traits<ores::trading::messaging::list_trades_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trades_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_request>::write_json(
    const ores::trading::messaging::get_trade_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_request
wire_traits<ores::trading::messaging::get_trade_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_request>::write_msgpack(
    const ores::trading::messaging::get_trade_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_request
wire_traits<ores::trading::messaging::get_trade_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_response>::write_json(
    const ores::trading::messaging::get_trade_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_response
wire_traits<ores::trading::messaging::get_trade_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_response>::write_msgpack(
    const ores::trading::messaging::get_trade_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_response
wire_traits<ores::trading::messaging::get_trade_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trades_request>::write_json(
    const ores::trading::messaging::get_many_trades_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trades_request
wire_traits<ores::trading::messaging::get_many_trades_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trades_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_trades_request>::write_msgpack(
    const ores::trading::messaging::get_many_trades_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trades_request
wire_traits<ores::trading::messaging::get_many_trades_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trades_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trades_response>::write_json(
    const ores::trading::messaging::get_many_trades_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trades_response
wire_traits<ores::trading::messaging::get_many_trades_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trades_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_trades_response>::write_msgpack(
    const ores::trading::messaging::get_many_trades_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trades_response
wire_traits<ores::trading::messaging::get_many_trades_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trades_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_trade_request>::write_json(
    const ores::trading::messaging::put_trade_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_trade_request
wire_traits<ores::trading::messaging::put_trade_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_trade_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_trade_request>::write_msgpack(
    const ores::trading::messaging::put_trade_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_trade_request
wire_traits<ores::trading::messaging::put_trade_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_trade_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_trade_response>::write_json(
    const ores::trading::messaging::put_trade_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_trade_response
wire_traits<ores::trading::messaging::put_trade_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_trade_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_trade_response>::write_msgpack(
    const ores::trading::messaging::put_trade_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_trade_response
wire_traits<ores::trading::messaging::put_trade_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_trade_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_trades_request>::write_json(
    const ores::trading::messaging::put_many_trades_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_trades_request
wire_traits<ores::trading::messaging::put_many_trades_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_trades_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_trades_request>::write_msgpack(
    const ores::trading::messaging::put_many_trades_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_trades_request
wire_traits<ores::trading::messaging::put_many_trades_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_trades_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_trades_response>::write_json(
    const ores::trading::messaging::put_many_trades_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_trades_response
wire_traits<ores::trading::messaging::put_many_trades_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_trades_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_trades_response>::write_msgpack(
    const ores::trading::messaging::put_many_trades_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_trades_response
wire_traits<ores::trading::messaging::put_many_trades_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_trades_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_trade_request>::write_json(
    const ores::trading::messaging::delete_trade_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_trade_request
wire_traits<ores::trading::messaging::delete_trade_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_trade_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_trade_request>::write_msgpack(
    const ores::trading::messaging::delete_trade_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_trade_request
wire_traits<ores::trading::messaging::delete_trade_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_trade_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_trade_response>::write_json(
    const ores::trading::messaging::delete_trade_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_trade_response
wire_traits<ores::trading::messaging::delete_trade_response>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_trade_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_trade_response>::write_msgpack(
    const ores::trading::messaging::delete_trade_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_trade_response
wire_traits<ores::trading::messaging::delete_trade_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_trade_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_trades_request>::write_json(
    const ores::trading::messaging::delete_many_trades_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_trades_request
wire_traits<ores::trading::messaging::delete_many_trades_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_trades_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_many_trades_request>::write_msgpack(
    const ores::trading::messaging::delete_many_trades_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_trades_request
wire_traits<ores::trading::messaging::delete_many_trades_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_trades_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_trades_response>::write_json(
    const ores::trading::messaging::delete_many_trades_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_trades_response
wire_traits<ores::trading::messaging::delete_many_trades_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_trades_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_many_trades_response>::write_msgpack(
    const ores::trading::messaging::delete_many_trades_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_trades_response
wire_traits<ores::trading::messaging::delete_many_trades_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_trades_response>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_versions_request>::write_json(
    const ores::trading::messaging::list_trade_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_versions_request
wire_traits<ores::trading::messaging::list_trade_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_versions_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_versions_request>::write_msgpack(
    const ores::trading::messaging::list_trade_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_versions_request
wire_traits<ores::trading::messaging::list_trade_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_versions_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_versions_response>::write_json(
    const ores::trading::messaging::list_trade_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_versions_response
wire_traits<ores::trading::messaging::list_trade_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_versions_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_versions_response>::write_msgpack(
    const ores::trading::messaging::list_trade_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_versions_response
wire_traits<ores::trading::messaging::list_trade_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_versions_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_version_request>::write_json(
    const ores::trading::messaging::get_trade_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_version_request
wire_traits<ores::trading::messaging::get_trade_version_request>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_version_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_version_request>::write_msgpack(
    const ores::trading::messaging::get_trade_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_version_request
wire_traits<ores::trading::messaging::get_trade_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_version_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_version_response>::write_json(
    const ores::trading::messaging::get_trade_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_version_response
wire_traits<ores::trading::messaging::get_trade_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_version_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_version_response>::write_msgpack(
    const ores::trading::messaging::get_trade_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_version_response
wire_traits<ores::trading::messaging::get_trade_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_version_response>(data);
}

}
