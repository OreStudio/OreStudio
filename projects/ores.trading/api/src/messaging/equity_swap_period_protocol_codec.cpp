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
#include "ores.trading.api/domain/equity_swap_period_codec.hpp"
#include "ores.trading.api/messaging/equity_swap_period_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::trading::messaging::equity_swap_period_key>::write_json(
    const ores::trading::messaging::equity_swap_period_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_key
wire_traits<ores::trading::messaging::equity_swap_period_key>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_key>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_key>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_key
wire_traits<ores::trading::messaging::equity_swap_period_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_key>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_write>::write_json(
    const ores::trading::messaging::equity_swap_period_write& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_write
wire_traits<ores::trading::messaging::equity_swap_period_write>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_write>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_write>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_write& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_write
wire_traits<ores::trading::messaging::equity_swap_period_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_write>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_change>::write_json(
    const ores::trading::messaging::equity_swap_period_change& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_change
wire_traits<ores::trading::messaging::equity_swap_period_change>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_change>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_change>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_change& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_change
wire_traits<ores::trading::messaging::equity_swap_period_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_change>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_removal>::write_json(
    const ores::trading::messaging::equity_swap_period_removal& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_removal
wire_traits<ores::trading::messaging::equity_swap_period_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_removal>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_removal>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_removal& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_removal
wire_traits<ores::trading::messaging::equity_swap_period_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_removal>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_lookup>::write_json(
    const ores::trading::messaging::equity_swap_period_lookup& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_lookup
wire_traits<ores::trading::messaging::equity_swap_period_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_lookup>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_lookup>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_lookup& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_lookup
wire_traits<ores::trading::messaging::equity_swap_period_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_lookup>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_event>::write_json(
    const ores::trading::messaging::equity_swap_period_event& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_event
wire_traits<ores::trading::messaging::equity_swap_period_event>::read_json(std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_event>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_event>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_event& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_event
wire_traits<ores::trading::messaging::equity_swap_period_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_event>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_version_key>::write_json(
    const ores::trading::messaging::equity_swap_period_version_key& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_version_key
wire_traits<ores::trading::messaging::equity_swap_period_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_version_key>(text);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_version_key>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_version_key& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_version_key
wire_traits<ores::trading::messaging::equity_swap_period_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_version_key>(data);
}

std::string wire_traits<ores::trading::messaging::equity_swap_period_versions_filter>::write_json(
    const ores::trading::messaging::equity_swap_period_versions_filter& v) {
    return object::write_json(v);
}

ores::trading::messaging::equity_swap_period_versions_filter
wire_traits<ores::trading::messaging::equity_swap_period_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::equity_swap_period_versions_filter>(text);
}

std::string
wire_traits<ores::trading::messaging::equity_swap_period_versions_filter>::write_msgpack(
    const ores::trading::messaging::equity_swap_period_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::equity_swap_period_versions_filter
wire_traits<ores::trading::messaging::equity_swap_period_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::equity_swap_period_versions_filter>(data);
}

std::string wire_traits<ores::trading::messaging::list_equity_swap_periods_request>::write_json(
    const ores::trading::messaging::list_equity_swap_periods_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_equity_swap_periods_request
wire_traits<ores::trading::messaging::list_equity_swap_periods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_equity_swap_periods_request>(text);
}

std::string wire_traits<ores::trading::messaging::list_equity_swap_periods_request>::write_msgpack(
    const ores::trading::messaging::list_equity_swap_periods_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_equity_swap_periods_request
wire_traits<ores::trading::messaging::list_equity_swap_periods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_equity_swap_periods_request>(data);
}

std::string wire_traits<ores::trading::messaging::list_equity_swap_periods_response>::write_json(
    const ores::trading::messaging::list_equity_swap_periods_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_equity_swap_periods_response
wire_traits<ores::trading::messaging::list_equity_swap_periods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_equity_swap_periods_response>(text);
}

std::string wire_traits<ores::trading::messaging::list_equity_swap_periods_response>::write_msgpack(
    const ores::trading::messaging::list_equity_swap_periods_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_equity_swap_periods_response
wire_traits<ores::trading::messaging::list_equity_swap_periods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_equity_swap_periods_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_equity_swap_period_request>::write_json(
    const ores::trading::messaging::get_equity_swap_period_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_equity_swap_period_request
wire_traits<ores::trading::messaging::get_equity_swap_period_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_equity_swap_period_request>(text);
}

std::string wire_traits<ores::trading::messaging::get_equity_swap_period_request>::write_msgpack(
    const ores::trading::messaging::get_equity_swap_period_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_equity_swap_period_request
wire_traits<ores::trading::messaging::get_equity_swap_period_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_equity_swap_period_request>(data);
}

std::string wire_traits<ores::trading::messaging::get_equity_swap_period_response>::write_json(
    const ores::trading::messaging::get_equity_swap_period_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_equity_swap_period_response
wire_traits<ores::trading::messaging::get_equity_swap_period_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_equity_swap_period_response>(text);
}

std::string wire_traits<ores::trading::messaging::get_equity_swap_period_response>::write_msgpack(
    const ores::trading::messaging::get_equity_swap_period_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_equity_swap_period_response
wire_traits<ores::trading::messaging::get_equity_swap_period_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_equity_swap_period_response>(data);
}

std::string wire_traits<ores::trading::messaging::get_many_equity_swap_periods_request>::write_json(
    const ores::trading::messaging::get_many_equity_swap_periods_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_equity_swap_periods_request>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_request>::write_msgpack(
    const ores::trading::messaging::get_many_equity_swap_periods_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_equity_swap_periods_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_response>::write_json(
    const ores::trading::messaging::get_many_equity_swap_periods_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_many_equity_swap_periods_response>(text);
}

std::string
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_response>::write_msgpack(
    const ores::trading::messaging::get_many_equity_swap_periods_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::get_many_equity_swap_periods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_many_equity_swap_periods_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::put_equity_swap_period_request>::write_json(
    const ores::trading::messaging::put_equity_swap_period_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_equity_swap_period_request
wire_traits<ores::trading::messaging::put_equity_swap_period_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_equity_swap_period_request>(text);
}

std::string wire_traits<ores::trading::messaging::put_equity_swap_period_request>::write_msgpack(
    const ores::trading::messaging::put_equity_swap_period_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_equity_swap_period_request
wire_traits<ores::trading::messaging::put_equity_swap_period_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_equity_swap_period_request>(data);
}

std::string wire_traits<ores::trading::messaging::put_equity_swap_period_response>::write_json(
    const ores::trading::messaging::put_equity_swap_period_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_equity_swap_period_response
wire_traits<ores::trading::messaging::put_equity_swap_period_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_equity_swap_period_response>(text);
}

std::string wire_traits<ores::trading::messaging::put_equity_swap_period_response>::write_msgpack(
    const ores::trading::messaging::put_equity_swap_period_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_equity_swap_period_response
wire_traits<ores::trading::messaging::put_equity_swap_period_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_equity_swap_period_response>(data);
}

std::string wire_traits<ores::trading::messaging::put_many_equity_swap_periods_request>::write_json(
    const ores::trading::messaging::put_many_equity_swap_periods_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_equity_swap_periods_request>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_request>::write_msgpack(
    const ores::trading::messaging::put_many_equity_swap_periods_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_equity_swap_periods_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_response>::write_json(
    const ores::trading::messaging::put_many_equity_swap_periods_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::put_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::put_many_equity_swap_periods_response>(text);
}

std::string
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_response>::write_msgpack(
    const ores::trading::messaging::put_many_equity_swap_periods_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::put_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::put_many_equity_swap_periods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::put_many_equity_swap_periods_response>(
        data);
}

std::string wire_traits<ores::trading::messaging::delete_equity_swap_period_request>::write_json(
    const ores::trading::messaging::delete_equity_swap_period_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_equity_swap_period_request
wire_traits<ores::trading::messaging::delete_equity_swap_period_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_equity_swap_period_request>(text);
}

std::string wire_traits<ores::trading::messaging::delete_equity_swap_period_request>::write_msgpack(
    const ores::trading::messaging::delete_equity_swap_period_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_equity_swap_period_request
wire_traits<ores::trading::messaging::delete_equity_swap_period_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_equity_swap_period_request>(data);
}

std::string wire_traits<ores::trading::messaging::delete_equity_swap_period_response>::write_json(
    const ores::trading::messaging::delete_equity_swap_period_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_equity_swap_period_response
wire_traits<ores::trading::messaging::delete_equity_swap_period_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_equity_swap_period_response>(text);
}

std::string
wire_traits<ores::trading::messaging::delete_equity_swap_period_response>::write_msgpack(
    const ores::trading::messaging::delete_equity_swap_period_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_equity_swap_period_response
wire_traits<ores::trading::messaging::delete_equity_swap_period_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_equity_swap_period_response>(data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_request>::write_json(
    const ores::trading::messaging::delete_many_equity_swap_periods_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_equity_swap_periods_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_request>::write_msgpack(
    const ores::trading::messaging::delete_many_equity_swap_periods_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_equity_swap_periods_request
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_equity_swap_periods_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_response>::write_json(
    const ores::trading::messaging::delete_many_equity_swap_periods_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::delete_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::delete_many_equity_swap_periods_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_response>::write_msgpack(
    const ores::trading::messaging::delete_many_equity_swap_periods_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::delete_many_equity_swap_periods_response
wire_traits<ores::trading::messaging::delete_many_equity_swap_periods_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::delete_many_equity_swap_periods_response>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_request>::write_json(
    const ores::trading::messaging::list_equity_swap_period_versions_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_equity_swap_period_versions_request
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_equity_swap_period_versions_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_request>::write_msgpack(
    const ores::trading::messaging::list_equity_swap_period_versions_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_equity_swap_period_versions_request
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::list_equity_swap_period_versions_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_response>::write_json(
    const ores::trading::messaging::list_equity_swap_period_versions_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::list_equity_swap_period_versions_response
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::list_equity_swap_period_versions_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_response>::write_msgpack(
    const ores::trading::messaging::list_equity_swap_period_versions_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::list_equity_swap_period_versions_response
wire_traits<ores::trading::messaging::list_equity_swap_period_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::trading::messaging::list_equity_swap_period_versions_response>(data);
}

std::string
wire_traits<ores::trading::messaging::get_equity_swap_period_version_request>::write_json(
    const ores::trading::messaging::get_equity_swap_period_version_request& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_equity_swap_period_version_request
wire_traits<ores::trading::messaging::get_equity_swap_period_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_equity_swap_period_version_request>(
        text);
}

std::string
wire_traits<ores::trading::messaging::get_equity_swap_period_version_request>::write_msgpack(
    const ores::trading::messaging::get_equity_swap_period_version_request& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_equity_swap_period_version_request
wire_traits<ores::trading::messaging::get_equity_swap_period_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_equity_swap_period_version_request>(
        data);
}

std::string
wire_traits<ores::trading::messaging::get_equity_swap_period_version_response>::write_json(
    const ores::trading::messaging::get_equity_swap_period_version_response& v) {
    return object::write_json(v);
}

ores::trading::messaging::get_equity_swap_period_version_response
wire_traits<ores::trading::messaging::get_equity_swap_period_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::trading::messaging::get_equity_swap_period_version_response>(
        text);
}

std::string
wire_traits<ores::trading::messaging::get_equity_swap_period_version_response>::write_msgpack(
    const ores::trading::messaging::get_equity_swap_period_version_response& v) {
    return object::write_msgpack(v);
}

ores::trading::messaging::get_equity_swap_period_version_response
wire_traits<ores::trading::messaging::get_equity_swap_period_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::trading::messaging::get_equity_swap_period_version_response>(
        data);
}

}
