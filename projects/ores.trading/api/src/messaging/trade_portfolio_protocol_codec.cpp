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
#include "ores.trading.api/domain/trade_portfolio_codec.hpp"
#include "ores.trading.api/messaging/trade_portfolio_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::trade_portfolio_key>::write_json(
    const ores::trading::messaging::trade_portfolio_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_key
wire_traits<ores::trading::messaging::trade_portfolio_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_key>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_key>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_key
wire_traits<ores::trading::messaging::trade_portfolio_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_key>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_write>::write_json(
    const ores::trading::messaging::trade_portfolio_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_write
wire_traits<ores::trading::messaging::trade_portfolio_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_write>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_write>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_write
wire_traits<ores::trading::messaging::trade_portfolio_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_write>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_change>::write_json(
    const ores::trading::messaging::trade_portfolio_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_change
wire_traits<ores::trading::messaging::trade_portfolio_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_change>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_change>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_change
wire_traits<ores::trading::messaging::trade_portfolio_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_change>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_removal>::write_json(
    const ores::trading::messaging::trade_portfolio_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_removal
wire_traits<ores::trading::messaging::trade_portfolio_removal>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_removal>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_removal>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_removal
wire_traits<ores::trading::messaging::trade_portfolio_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_removal>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_lookup>::write_json(
    const ores::trading::messaging::trade_portfolio_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_lookup
wire_traits<ores::trading::messaging::trade_portfolio_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_lookup>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_lookup
wire_traits<ores::trading::messaging::trade_portfolio_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolios_filter>::write_json(
    const ores::trading::messaging::trade_portfolios_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolios_filter
wire_traits<ores::trading::messaging::trade_portfolios_filter>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolios_filter>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolios_filter>::write_msgpack(
    const ores::trading::messaging::trade_portfolios_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolios_filter
wire_traits<ores::trading::messaging::trade_portfolios_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolios_filter>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_event>::write_json(
    const ores::trading::messaging::trade_portfolio_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_event
wire_traits<ores::trading::messaging::trade_portfolio_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_event>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_event>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_event
wire_traits<ores::trading::messaging::trade_portfolio_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_event>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_version_key>::write_json(
    const ores::trading::messaging::trade_portfolio_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_version_key
wire_traits<ores::trading::messaging::trade_portfolio_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_version_key>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_version_key
wire_traits<ores::trading::messaging::trade_portfolio_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_versions_filter>::write_json(
    const ores::trading::messaging::trade_portfolio_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::trade_portfolio_versions_filter
wire_traits<ores::trading::messaging::trade_portfolio_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::trade_portfolio_versions_filter>(text);
}

std::string wire_traits<ores::trading::messaging::trade_portfolio_versions_filter>::write_msgpack(
    const ores::trading::messaging::trade_portfolio_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::trade_portfolio_versions_filter
wire_traits<ores::trading::messaging::trade_portfolio_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::trade_portfolio_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_portfolios_request>::write_json(
    const ores::trading::messaging::list_trade_portfolios_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_portfolios_request
wire_traits<ores::trading::messaging::list_trade_portfolios_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_portfolios_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_portfolios_request>::write_msgpack(
    const ores::trading::messaging::list_trade_portfolios_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_portfolios_request
wire_traits<ores::trading::messaging::list_trade_portfolios_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_portfolios_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_trade_portfolios_response>::write_json(
    const ores::trading::messaging::list_trade_portfolios_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_portfolios_response
wire_traits<ores::trading::messaging::list_trade_portfolios_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_portfolios_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_trade_portfolios_response>::write_msgpack(
    const ores::trading::messaging::list_trade_portfolios_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_portfolios_response
wire_traits<ores::trading::messaging::list_trade_portfolios_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_portfolios_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_request>::write_json(
    const ores::trading::messaging::get_trade_portfolio_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_portfolio_request
wire_traits<ores::trading::messaging::get_trade_portfolio_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_portfolio_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_request>::write_msgpack(
    const ores::trading::messaging::get_trade_portfolio_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_portfolio_request
wire_traits<ores::trading::messaging::get_trade_portfolio_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_portfolio_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_response>::write_json(
    const ores::trading::messaging::get_trade_portfolio_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_portfolio_response
wire_traits<ores::trading::messaging::get_trade_portfolio_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_portfolio_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_response>::write_msgpack(
    const ores::trading::messaging::get_trade_portfolio_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_portfolio_response
wire_traits<ores::trading::messaging::get_trade_portfolio_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_portfolio_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_portfolios_request>::write_json(
    const ores::trading::messaging::get_many_trade_portfolios_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trade_portfolios_request
wire_traits<ores::trading::messaging::get_many_trade_portfolios_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trade_portfolios_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_portfolios_request>::write_msgpack(
    const ores::trading::messaging::get_many_trade_portfolios_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trade_portfolios_request
wire_traits<ores::trading::messaging::get_many_trade_portfolios_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trade_portfolios_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_trade_portfolios_response>::write_json(
    const ores::trading::messaging::get_many_trade_portfolios_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_trade_portfolios_response
wire_traits<ores::trading::messaging::get_many_trade_portfolios_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_trade_portfolios_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_trade_portfolios_response>::write_msgpack(
    const ores::trading::messaging::get_many_trade_portfolios_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_trade_portfolios_response
wire_traits<ores::trading::messaging::get_many_trade_portfolios_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_trade_portfolios_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_trade_portfolio_request>::write_json(
    const ores::trading::messaging::put_trade_portfolio_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_trade_portfolio_request
wire_traits<ores::trading::messaging::put_trade_portfolio_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_trade_portfolio_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_trade_portfolio_request>::write_msgpack(
    const ores::trading::messaging::put_trade_portfolio_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_trade_portfolio_request
wire_traits<ores::trading::messaging::put_trade_portfolio_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_trade_portfolio_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_trade_portfolio_response>::write_json(
    const ores::trading::messaging::put_trade_portfolio_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_trade_portfolio_response
wire_traits<ores::trading::messaging::put_trade_portfolio_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_trade_portfolio_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_trade_portfolio_response>::write_msgpack(
    const ores::trading::messaging::put_trade_portfolio_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_trade_portfolio_response
wire_traits<ores::trading::messaging::put_trade_portfolio_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_trade_portfolio_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_trade_portfolios_request>::write_json(
    const ores::trading::messaging::put_many_trade_portfolios_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_trade_portfolios_request
wire_traits<ores::trading::messaging::put_many_trade_portfolios_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_trade_portfolios_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_many_trade_portfolios_request>::write_msgpack(
    const ores::trading::messaging::put_many_trade_portfolios_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_trade_portfolios_request
wire_traits<ores::trading::messaging::put_many_trade_portfolios_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_trade_portfolios_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_trade_portfolios_response>::write_json(
    const ores::trading::messaging::put_many_trade_portfolios_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_trade_portfolios_response
wire_traits<ores::trading::messaging::put_many_trade_portfolios_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_trade_portfolios_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_trade_portfolios_response>::write_msgpack(
    const ores::trading::messaging::put_many_trade_portfolios_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_trade_portfolios_response
wire_traits<ores::trading::messaging::put_many_trade_portfolios_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_trade_portfolios_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_trade_portfolio_request>::write_json(
    const ores::trading::messaging::delete_trade_portfolio_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_trade_portfolio_request
wire_traits<ores::trading::messaging::delete_trade_portfolio_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_trade_portfolio_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_trade_portfolio_request>::write_msgpack(
    const ores::trading::messaging::delete_trade_portfolio_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_trade_portfolio_request
wire_traits<ores::trading::messaging::delete_trade_portfolio_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_trade_portfolio_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_trade_portfolio_response>::write_json(
    const ores::trading::messaging::delete_trade_portfolio_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_trade_portfolio_response
wire_traits<ores::trading::messaging::delete_trade_portfolio_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_trade_portfolio_response>(text);
}

std::string wire_traits<ores::trading::messaging::delete_trade_portfolio_response>::write_msgpack(
    const ores::trading::messaging::delete_trade_portfolio_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_trade_portfolio_response
wire_traits<ores::trading::messaging::delete_trade_portfolio_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_trade_portfolio_response>(data);
}

std::string wire_traits<ores::trading::messaging::delete_many_trade_portfolios_request>::write_json(
    const ores::trading::messaging::delete_many_trade_portfolios_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_trade_portfolios_request
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_trade_portfolios_request>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_request>::write_msgpack(
    const ores::trading::messaging::delete_many_trade_portfolios_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_trade_portfolios_request
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_trade_portfolios_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_response>::write_json(
    const ores::trading::messaging::delete_many_trade_portfolios_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_trade_portfolios_response
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_trade_portfolios_response>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_response>::write_msgpack(
    const ores::trading::messaging::delete_many_trade_portfolios_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_trade_portfolios_response
wire_traits<ores::trading::messaging::delete_many_trade_portfolios_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_trade_portfolios_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_request>::write_json(
    const ores::trading::messaging::list_by_trade_id_trade_portfolios_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_trade_portfolios_request
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_by_trade_id_trade_portfolios_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_request>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_trade_portfolios_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_trade_portfolios_request
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_trade_portfolios_request>(data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_response>::write_json(
    const ores::trading::messaging::list_by_trade_id_trade_portfolios_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_trade_portfolios_response
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_by_trade_id_trade_portfolios_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_response>::write_msgpack(
    const ores::trading::messaging::list_by_trade_id_trade_portfolios_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_trade_portfolios_response
wire_traits<ores::trading::messaging::list_by_trade_id_trade_portfolios_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_trade_portfolios_response>(data);
}

std::string
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_request>::write_json(
    const ores::trading::messaging::list_trade_portfolio_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_portfolio_versions_request
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_portfolio_versions_request>(text);
}

std::string
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_request>::write_msgpack(
    const ores::trading::messaging::list_trade_portfolio_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_portfolio_versions_request
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_portfolio_versions_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_response>::write_json(
    const ores::trading::messaging::list_trade_portfolio_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_trade_portfolio_versions_response
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_trade_portfolio_versions_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_response>::write_msgpack(
    const ores::trading::messaging::list_trade_portfolio_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_trade_portfolio_versions_response
wire_traits<ores::trading::messaging::list_trade_portfolio_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_trade_portfolio_versions_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_version_request>::write_json(
    const ores::trading::messaging::get_trade_portfolio_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_portfolio_version_request
wire_traits<ores::trading::messaging::get_trade_portfolio_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_portfolio_version_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_trade_portfolio_version_request>::write_msgpack(
    const ores::trading::messaging::get_trade_portfolio_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_portfolio_version_request
wire_traits<ores::trading::messaging::get_trade_portfolio_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_portfolio_version_request>(
        data);
}

std::string wire_traits<ores::trading::messaging::get_trade_portfolio_version_response>::write_json(
    const ores::trading::messaging::get_trade_portfolio_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_trade_portfolio_version_response
wire_traits<ores::trading::messaging::get_trade_portfolio_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_trade_portfolio_version_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_trade_portfolio_version_response>::write_msgpack(
    const ores::trading::messaging::get_trade_portfolio_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_trade_portfolio_version_response
wire_traits<ores::trading::messaging::get_trade_portfolio_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_trade_portfolio_version_response>(
        data);
}

}
