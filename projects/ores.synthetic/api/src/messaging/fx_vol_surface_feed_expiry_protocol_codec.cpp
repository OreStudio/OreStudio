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
#include "ores.synthetic.api/domain/fx_vol_surface_feed_expiry_codec.hpp"
#include "ores.synthetic.api/messaging/fx_vol_surface_feed_expiry_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_key
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>(text);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_key
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_key>(data);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_write& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_write
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_write& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_write
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_write>(data);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_change& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_change
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_change& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_change
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_change>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_removal>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_lookup>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiries_filter>(
        data);
}

std::string wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_event& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_event
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_event& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_event
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_event>(data);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::fx_vol_surface_feed_expiry_version_key>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>::write_json(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>(text);
}

std::string
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>::write_msgpack(
    const ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter
wire_traits<ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::fx_vol_surface_feed_expiry_versions_filter>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>::write_json(
    const ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>::write_msgpack(
    const ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>::write_json(
    const ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>::write_msgpack(
    const ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiries_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>::write_json(
    const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>::write_msgpack(
    const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>::write_json(
    const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>::write_msgpack(
    const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>::write_json(
    const ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>::
    write_msgpack(
        const ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>::write_json(
    const ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>::
    write_msgpack(
        const ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_many_fx_vol_surface_feed_expiries_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>::write_json(
    const ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>::write_msgpack(
    const ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_request>(
        data);
}

std::string
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>::write_json(
    const ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>::write_msgpack(
    const ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::put_fx_vol_surface_feed_expiry_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>::write_json(
    const ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>(text);
}

std::string wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>::
    write_msgpack(
        const ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>::write_json(
    const ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>::
    write_msgpack(
        const ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::put_many_fx_vol_surface_feed_expiries_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>::write_json(
    const ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>(
        text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>::write_msgpack(
    const ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>::write_json(
    const ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>::write_msgpack(
    const ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response
wire_traits<ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_fx_vol_surface_feed_expiry_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>::
    write_json(
        const ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>::
    write_msgpack(
        const ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>::
    write_json(
        const ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>::
    write_msgpack(
        const ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response
wire_traits<ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::delete_many_fx_vol_surface_feed_expiries_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>::
    write_json(
        const ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>::
    write_msgpack(
        const ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>::
    write_json(
        const ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>::
    write_msgpack(
        const ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response
wire_traits<ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::list_fx_vol_surface_feed_expiry_versions_response>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>::write_json(
    const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>::
    write_msgpack(
        const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_request>(data);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>::
    write_json(
        const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response& v) {
    return object::write_json(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>(text);
}

std::string
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>::
    write_msgpack(
        const ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response& v) {
    return object::write_msgpack(v);
}

ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response
wire_traits<ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::synthetic::messaging::get_fx_vol_surface_feed_expiry_version_response>(data);
}

}
