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
#include "ores.trading.api/domain/instrument_option_exercise_price_codec.hpp"
#include "ores.trading.api/messaging/instrument_option_exercise_price_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::instrument_option_exercise_price_key>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_key
wire_traits<ores::trading::messaging::instrument_option_exercise_price_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_key>(text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_key>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_key
wire_traits<ores::trading::messaging::instrument_option_exercise_price_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_key>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_write>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_write
wire_traits<ores::trading::messaging::instrument_option_exercise_price_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_write>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_write>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_write
wire_traits<ores::trading::messaging::instrument_option_exercise_price_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_write>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_change>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_change
wire_traits<ores::trading::messaging::instrument_option_exercise_price_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_change>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_change>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_change
wire_traits<ores::trading::messaging::instrument_option_exercise_price_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_change>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_removal>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_removal
wire_traits<ores::trading::messaging::instrument_option_exercise_price_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_removal>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_removal>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_removal
wire_traits<ores::trading::messaging::instrument_option_exercise_price_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_removal>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_lookup>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_lookup
wire_traits<ores::trading::messaging::instrument_option_exercise_price_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_lookup>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_lookup>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_lookup
wire_traits<ores::trading::messaging::instrument_option_exercise_price_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_lookup>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_prices_filter>::write_json(
    const ores::trading::messaging::instrument_option_exercise_prices_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_prices_filter
wire_traits<ores::trading::messaging::instrument_option_exercise_prices_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_prices_filter>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_prices_filter>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_prices_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_prices_filter
wire_traits<ores::trading::messaging::instrument_option_exercise_prices_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_prices_filter>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_event>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_event
wire_traits<ores::trading::messaging::instrument_option_exercise_price_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::instrument_option_exercise_price_event>(
        text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_event>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_event
wire_traits<ores::trading::messaging::instrument_option_exercise_price_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::instrument_option_exercise_price_event>(
        data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_version_key>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_version_key
wire_traits<ores::trading::messaging::instrument_option_exercise_price_version_key>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::instrument_option_exercise_price_version_key>(text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_version_key>::write_msgpack(
    const ores::trading::messaging::instrument_option_exercise_price_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_version_key
wire_traits<ores::trading::messaging::instrument_option_exercise_price_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::instrument_option_exercise_price_version_key>(data);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_versions_filter>::write_json(
    const ores::trading::messaging::instrument_option_exercise_price_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::instrument_option_exercise_price_versions_filter
wire_traits<ores::trading::messaging::instrument_option_exercise_price_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::instrument_option_exercise_price_versions_filter>(text);
}

std::string
wire_traits<ores::trading::messaging::instrument_option_exercise_price_versions_filter>::
    write_msgpack(
        const ores::trading::messaging::instrument_option_exercise_price_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::instrument_option_exercise_price_versions_filter
wire_traits<ores::trading::messaging::instrument_option_exercise_price_versions_filter>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::instrument_option_exercise_price_versions_filter>(data);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_request>::write_json(
    const ores::trading::messaging::list_instrument_option_exercise_prices_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_instrument_option_exercise_prices_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_request>::
    write_msgpack(
        const ores::trading::messaging::list_instrument_option_exercise_prices_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_instrument_option_exercise_prices_request>(data);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_response>::write_json(
    const ores::trading::messaging::list_instrument_option_exercise_prices_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_instrument_option_exercise_prices_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_response>::
    write_msgpack(
        const ores::trading::messaging::list_instrument_option_exercise_prices_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::list_instrument_option_exercise_prices_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_instrument_option_exercise_prices_response>(data);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_request>::write_json(
    const ores::trading::messaging::get_instrument_option_exercise_price_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_instrument_option_exercise_price_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_request>::write_msgpack(
    const ores::trading::messaging::get_instrument_option_exercise_price_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_instrument_option_exercise_price_request>(data);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_response>::write_json(
    const ores::trading::messaging::get_instrument_option_exercise_price_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_instrument_option_exercise_price_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_response>::write_msgpack(
    const ores::trading::messaging::get_instrument_option_exercise_price_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_instrument_option_exercise_price_response>(data);
}

std::string
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_request>::
    write_json(
        const ores::trading::messaging::get_many_instrument_option_exercise_prices_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_many_instrument_option_exercise_prices_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_request>::
    write_msgpack(
        const ores::trading::messaging::get_many_instrument_option_exercise_prices_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_many_instrument_option_exercise_prices_request>(data);
}

std::string
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_response>::
    write_json(
        const ores::trading::messaging::get_many_instrument_option_exercise_prices_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_many_instrument_option_exercise_prices_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_response>::
    write_msgpack(
        const ores::trading::messaging::get_many_instrument_option_exercise_prices_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::get_many_instrument_option_exercise_prices_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_many_instrument_option_exercise_prices_response>(data);
}

std::string
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_request>::write_json(
    const ores::trading::messaging::put_instrument_option_exercise_price_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::put_instrument_option_exercise_price_request>(text);
}

std::string
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_request>::write_msgpack(
    const ores::trading::messaging::put_instrument_option_exercise_price_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::put_instrument_option_exercise_price_request>(data);
}

std::string
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_response>::write_json(
    const ores::trading::messaging::put_instrument_option_exercise_price_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::put_instrument_option_exercise_price_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_response>::write_msgpack(
    const ores::trading::messaging::put_instrument_option_exercise_price_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::put_instrument_option_exercise_price_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::put_instrument_option_exercise_price_response>(data);
}

std::string
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_request>::
    write_json(
        const ores::trading::messaging::put_many_instrument_option_exercise_prices_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::put_many_instrument_option_exercise_prices_request>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_request>::
    write_msgpack(
        const ores::trading::messaging::put_many_instrument_option_exercise_prices_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::put_many_instrument_option_exercise_prices_request>(data);
}

std::string
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_response>::
    write_json(
        const ores::trading::messaging::put_many_instrument_option_exercise_prices_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::put_many_instrument_option_exercise_prices_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_response>::
    write_msgpack(
        const ores::trading::messaging::put_many_instrument_option_exercise_prices_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::put_many_instrument_option_exercise_prices_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::put_many_instrument_option_exercise_prices_response>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_request>::write_json(
    const ores::trading::messaging::delete_instrument_option_exercise_price_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::delete_instrument_option_exercise_price_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_request>::
    write_msgpack(
        const ores::trading::messaging::delete_instrument_option_exercise_price_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_instrument_option_exercise_price_request
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::delete_instrument_option_exercise_price_request>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_response>::write_json(
    const ores::trading::messaging::delete_instrument_option_exercise_price_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::trading::messaging::delete_instrument_option_exercise_price_response>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_response>::
    write_msgpack(
        const ores::trading::messaging::delete_instrument_option_exercise_price_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_instrument_option_exercise_price_response
wire_traits<ores::trading::messaging::delete_instrument_option_exercise_price_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::delete_instrument_option_exercise_price_response>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>::
    write_json(
        const ores::trading::messaging::delete_many_instrument_option_exercise_prices_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>::
    write_msgpack(
        const ores::trading::messaging::delete_many_instrument_option_exercise_prices_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::delete_many_instrument_option_exercise_prices_request>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>::
    write_json(
        const ores::trading::messaging::delete_many_instrument_option_exercise_prices_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>::
    write_msgpack(
        const ores::trading::messaging::delete_many_instrument_option_exercise_prices_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::delete_many_instrument_option_exercise_prices_response>(data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>::
    write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request&
            v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>(text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>::
    write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request&
            v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_request>(data);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>::
    write_json(
        const ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response&
            v) {
    return object::write_json(v);
}

ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>::
    write_msgpack(
        const ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response&
            v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response
wire_traits<ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_by_trade_id_instrument_option_exercise_prices_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_request>::
    write_json(
        const ores::trading::messaging::list_instrument_option_exercise_price_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_instrument_option_exercise_price_versions_request
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_instrument_option_exercise_price_versions_request>(text);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_request>::
    write_msgpack(
        const ores::trading::messaging::list_instrument_option_exercise_price_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_instrument_option_exercise_price_versions_request
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_instrument_option_exercise_price_versions_request>(data);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_response>::
    write_json(
        const ores::trading::messaging::list_instrument_option_exercise_price_versions_response&
            v) {
    return object::write_json(v);
}

ores::trading::messaging::list_instrument_option_exercise_price_versions_response
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::list_instrument_option_exercise_price_versions_response>(text);
}

std::string
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_response>::
    write_msgpack(
        const ores::trading::messaging::list_instrument_option_exercise_price_versions_response&
            v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_instrument_option_exercise_price_versions_response
wire_traits<ores::trading::messaging::list_instrument_option_exercise_price_versions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_instrument_option_exercise_price_versions_response>(data);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_request>::
    write_json(
        const ores::trading::messaging::get_instrument_option_exercise_price_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_version_request
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_instrument_option_exercise_price_version_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_request>::
    write_msgpack(
        const ores::trading::messaging::get_instrument_option_exercise_price_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_version_request
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_instrument_option_exercise_price_version_request>(data);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_response>::
    write_json(
        const ores::trading::messaging::get_instrument_option_exercise_price_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_version_response
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::trading::messaging::get_instrument_option_exercise_price_version_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_response>::
    write_msgpack(
        const ores::trading::messaging::get_instrument_option_exercise_price_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_instrument_option_exercise_price_version_response
wire_traits<ores::trading::messaging::get_instrument_option_exercise_price_version_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::get_instrument_option_exercise_price_version_response>(data);
}

}
