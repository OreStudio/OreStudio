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
#include "ores.marketdata.api/domain/market_series_codec.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::marketdata::messaging::market_series_key>::write_json(
    const ores::marketdata::messaging::market_series_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_key
wire_traits<ores::marketdata::messaging::market_series_key>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_key>::write_msgpack(
    const ores::marketdata::messaging::market_series_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_key
wire_traits<ores::marketdata::messaging::market_series_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_write>::write_json(
    const ores::marketdata::messaging::market_series_write& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_write
wire_traits<ores::marketdata::messaging::market_series_write>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_write>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_write>::write_msgpack(
    const ores::marketdata::messaging::market_series_write& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_write
wire_traits<ores::marketdata::messaging::market_series_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_write>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_change>::write_json(
    const ores::marketdata::messaging::market_series_change& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_change
wire_traits<ores::marketdata::messaging::market_series_change>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_change>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_change>::write_msgpack(
    const ores::marketdata::messaging::market_series_change& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_change
wire_traits<ores::marketdata::messaging::market_series_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_change>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_removal>::write_json(
    const ores::marketdata::messaging::market_series_removal& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_removal
wire_traits<ores::marketdata::messaging::market_series_removal>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_removal>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_removal>::write_msgpack(
    const ores::marketdata::messaging::market_series_removal& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_removal
wire_traits<ores::marketdata::messaging::market_series_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_removal>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_lookup>::write_json(
    const ores::marketdata::messaging::market_series_lookup& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_lookup
wire_traits<ores::marketdata::messaging::market_series_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_lookup>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_lookup>::write_msgpack(
    const ores::marketdata::messaging::market_series_lookup& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_lookup
wire_traits<ores::marketdata::messaging::market_series_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_lookup>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_filter>::write_json(
    const ores::marketdata::messaging::market_series_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_filter
wire_traits<ores::marketdata::messaging::market_series_filter>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_filter>::write_msgpack(
    const ores::marketdata::messaging::market_series_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_filter
wire_traits<ores::marketdata::messaging::market_series_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_event>::write_json(
    const ores::marketdata::messaging::market_series_event& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_event
wire_traits<ores::marketdata::messaging::market_series_event>::read_json(std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_event>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_event>::write_msgpack(
    const ores::marketdata::messaging::market_series_event& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_event
wire_traits<ores::marketdata::messaging::market_series_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_event>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_version_key>::write_json(
    const ores::marketdata::messaging::market_series_version_key& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_version_key
wire_traits<ores::marketdata::messaging::market_series_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_version_key>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_version_key>::write_msgpack(
    const ores::marketdata::messaging::market_series_version_key& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_version_key
wire_traits<ores::marketdata::messaging::market_series_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_version_key>(data);
}

std::string wire_traits<ores::marketdata::messaging::market_series_versions_filter>::write_json(
    const ores::marketdata::messaging::market_series_versions_filter& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::market_series_versions_filter
wire_traits<ores::marketdata::messaging::market_series_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::market_series_versions_filter>(text);
}

std::string wire_traits<ores::marketdata::messaging::market_series_versions_filter>::write_msgpack(
    const ores::marketdata::messaging::market_series_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::market_series_versions_filter
wire_traits<ores::marketdata::messaging::market_series_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::market_series_versions_filter>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_market_series_request>::write_json(
    const ores::marketdata::messaging::list_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_series_request
wire_traits<ores::marketdata::messaging::list_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::list_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_series_request
wire_traits<ores::marketdata::messaging::list_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::list_market_series_response>::write_json(
    const ores::marketdata::messaging::list_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_series_response
wire_traits<ores::marketdata::messaging::list_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_series_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::list_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::list_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_series_response
wire_traits<ores::marketdata::messaging::list_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_market_series_request>::write_json(
    const ores::marketdata::messaging::get_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_series_request
wire_traits<ores::marketdata::messaging::get_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::get_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_series_request
wire_traits<ores::marketdata::messaging::get_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_market_series_response>::write_json(
    const ores::marketdata::messaging::get_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_series_response
wire_traits<ores::marketdata::messaging::get_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_series_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::get_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_series_response
wire_traits<ores::marketdata::messaging::get_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_market_series_request>::write_json(
    const ores::marketdata::messaging::get_many_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_market_series_request
wire_traits<ores::marketdata::messaging::get_many_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::get_many_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::get_many_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_market_series_request
wire_traits<ores::marketdata::messaging::get_many_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::get_many_market_series_response>::write_json(
    const ores::marketdata::messaging::get_many_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_many_market_series_response
wire_traits<ores::marketdata::messaging::get_many_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_many_market_series_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_many_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::get_many_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_many_market_series_response
wire_traits<ores::marketdata::messaging::get_many_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_many_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_market_series_request>::write_json(
    const ores::marketdata::messaging::put_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_market_series_request
wire_traits<ores::marketdata::messaging::put_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::put_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_market_series_request
wire_traits<ores::marketdata::messaging::put_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_market_series_response>::write_json(
    const ores::marketdata::messaging::put_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_market_series_response
wire_traits<ores::marketdata::messaging::put_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_market_series_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::put_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_market_series_response
wire_traits<ores::marketdata::messaging::put_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_many_market_series_request>::write_json(
    const ores::marketdata::messaging::put_many_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_market_series_request
wire_traits<ores::marketdata::messaging::put_many_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::put_many_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::put_many_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_market_series_request
wire_traits<ores::marketdata::messaging::put_many_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::put_many_market_series_response>::write_json(
    const ores::marketdata::messaging::put_many_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::put_many_market_series_response
wire_traits<ores::marketdata::messaging::put_many_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::put_many_market_series_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::put_many_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::put_many_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::put_many_market_series_response
wire_traits<ores::marketdata::messaging::put_many_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::put_many_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_market_series_request>::write_json(
    const ores::marketdata::messaging::delete_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_market_series_request
wire_traits<ores::marketdata::messaging::delete_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_market_series_request>(text);
}

std::string wire_traits<ores::marketdata::messaging::delete_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::delete_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_market_series_request
wire_traits<ores::marketdata::messaging::delete_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_market_series_request>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_market_series_response>::write_json(
    const ores::marketdata::messaging::delete_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_market_series_response
wire_traits<ores::marketdata::messaging::delete_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_market_series_response>(text);
}

std::string wire_traits<ores::marketdata::messaging::delete_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::delete_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_market_series_response
wire_traits<ores::marketdata::messaging::delete_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_market_series_response>(data);
}

std::string wire_traits<ores::marketdata::messaging::delete_many_market_series_request>::write_json(
    const ores::marketdata::messaging::delete_many_market_series_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_market_series_request
wire_traits<ores::marketdata::messaging::delete_many_market_series_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_market_series_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_series_request>::write_msgpack(
    const ores::marketdata::messaging::delete_many_market_series_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_market_series_request
wire_traits<ores::marketdata::messaging::delete_many_market_series_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_many_market_series_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_series_response>::write_json(
    const ores::marketdata::messaging::delete_many_market_series_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::delete_many_market_series_response
wire_traits<ores::marketdata::messaging::delete_many_market_series_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::delete_many_market_series_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::delete_many_market_series_response>::write_msgpack(
    const ores::marketdata::messaging::delete_many_market_series_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::delete_many_market_series_response
wire_traits<ores::marketdata::messaging::delete_many_market_series_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::delete_many_market_series_response>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_series_versions_request>::write_json(
    const ores::marketdata::messaging::list_market_series_versions_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_series_versions_request
wire_traits<ores::marketdata::messaging::list_market_series_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_series_versions_request>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_series_versions_request>::write_msgpack(
    const ores::marketdata::messaging::list_market_series_versions_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_series_versions_request
wire_traits<ores::marketdata::messaging::list_market_series_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_series_versions_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_series_versions_response>::write_json(
    const ores::marketdata::messaging::list_market_series_versions_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::list_market_series_versions_response
wire_traits<ores::marketdata::messaging::list_market_series_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::list_market_series_versions_response>(
        text);
}

std::string
wire_traits<ores::marketdata::messaging::list_market_series_versions_response>::write_msgpack(
    const ores::marketdata::messaging::list_market_series_versions_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::list_market_series_versions_response
wire_traits<ores::marketdata::messaging::list_market_series_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::list_market_series_versions_response>(
        data);
}

std::string wire_traits<ores::marketdata::messaging::get_market_series_version_request>::write_json(
    const ores::marketdata::messaging::get_market_series_version_request& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_series_version_request
wire_traits<ores::marketdata::messaging::get_market_series_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_series_version_request>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_market_series_version_request>::write_msgpack(
    const ores::marketdata::messaging::get_market_series_version_request& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_series_version_request
wire_traits<ores::marketdata::messaging::get_market_series_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_series_version_request>(
        data);
}

std::string
wire_traits<ores::marketdata::messaging::get_market_series_version_response>::write_json(
    const ores::marketdata::messaging::get_market_series_version_response& v) {
    return object::write_json(v);
}

ores::marketdata::messaging::get_market_series_version_response
wire_traits<ores::marketdata::messaging::get_market_series_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::marketdata::messaging::get_market_series_version_response>(text);
}

std::string
wire_traits<ores::marketdata::messaging::get_market_series_version_response>::write_msgpack(
    const ores::marketdata::messaging::get_market_series_version_response& v) {
    return object::write_msgpack(v);
}

ores::marketdata::messaging::get_market_series_version_response
wire_traits<ores::marketdata::messaging::get_market_series_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::marketdata::messaging::get_market_series_version_response>(
        data);
}

}
