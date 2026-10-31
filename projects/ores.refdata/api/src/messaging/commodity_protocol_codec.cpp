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
#include "ores.refdata.api/domain/commodity_codec.hpp"
#include "ores.refdata.api/messaging/commodity_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::commodity_key>::write_json(
    const ores::refdata::messaging::commodity_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_key
wire_traits<ores::refdata::messaging::commodity_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_key>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_key>::write_msgpack(
    const ores::refdata::messaging::commodity_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_key
wire_traits<ores::refdata::messaging::commodity_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_key>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_write>::write_json(
    const ores::refdata::messaging::commodity_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_write
wire_traits<ores::refdata::messaging::commodity_write>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_write>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_write>::write_msgpack(
    const ores::refdata::messaging::commodity_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_write
wire_traits<ores::refdata::messaging::commodity_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_write>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_change>::write_json(
    const ores::refdata::messaging::commodity_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_change
wire_traits<ores::refdata::messaging::commodity_change>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_change>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_change>::write_msgpack(
    const ores::refdata::messaging::commodity_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_change
wire_traits<ores::refdata::messaging::commodity_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_change>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_removal>::write_json(
    const ores::refdata::messaging::commodity_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_removal
wire_traits<ores::refdata::messaging::commodity_removal>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_removal>::write_msgpack(
    const ores::refdata::messaging::commodity_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_removal
wire_traits<ores::refdata::messaging::commodity_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_lookup>::write_json(
    const ores::refdata::messaging::commodity_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_lookup
wire_traits<ores::refdata::messaging::commodity_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_lookup>::write_msgpack(
    const ores::refdata::messaging::commodity_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_lookup
wire_traits<ores::refdata::messaging::commodity_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::commodities_filter>::write_json(
    const ores::refdata::messaging::commodities_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodities_filter
wire_traits<ores::refdata::messaging::commodities_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodities_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::commodities_filter>::write_msgpack(
    const ores::refdata::messaging::commodities_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodities_filter
wire_traits<ores::refdata::messaging::commodities_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodities_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_event>::write_json(
    const ores::refdata::messaging::commodity_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_event
wire_traits<ores::refdata::messaging::commodity_event>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_event>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_event>::write_msgpack(
    const ores::refdata::messaging::commodity_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_event
wire_traits<ores::refdata::messaging::commodity_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_event>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_version_key>::write_json(
    const ores::refdata::messaging::commodity_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_version_key
wire_traits<ores::refdata::messaging::commodity_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_version_key>::write_msgpack(
    const ores::refdata::messaging::commodity_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_version_key
wire_traits<ores::refdata::messaging::commodity_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_version_key>(data);
}

std::string wire_traits<ores::refdata::messaging::commodity_versions_filter>::write_json(
    const ores::refdata::messaging::commodity_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::commodity_versions_filter
wire_traits<ores::refdata::messaging::commodity_versions_filter>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::commodity_versions_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::commodity_versions_filter>::write_msgpack(
    const ores::refdata::messaging::commodity_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::commodity_versions_filter
wire_traits<ores::refdata::messaging::commodity_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::commodity_versions_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_commodities_request>::write_json(
    const ores::refdata::messaging::list_commodities_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_commodities_request
wire_traits<ores::refdata::messaging::list_commodities_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_commodities_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_commodities_request>::write_msgpack(
    const ores::refdata::messaging::list_commodities_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_commodities_request
wire_traits<ores::refdata::messaging::list_commodities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_commodities_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_commodities_response>::write_json(
    const ores::refdata::messaging::list_commodities_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_commodities_response
wire_traits<ores::refdata::messaging::list_commodities_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_commodities_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_commodities_response>::write_msgpack(
    const ores::refdata::messaging::list_commodities_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_commodities_response
wire_traits<ores::refdata::messaging::list_commodities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_commodities_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_request>::write_json(
    const ores::refdata::messaging::get_commodity_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_commodity_request
wire_traits<ores::refdata::messaging::get_commodity_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_commodity_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_request>::write_msgpack(
    const ores::refdata::messaging::get_commodity_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_commodity_request
wire_traits<ores::refdata::messaging::get_commodity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_commodity_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_response>::write_json(
    const ores::refdata::messaging::get_commodity_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_commodity_response
wire_traits<ores::refdata::messaging::get_commodity_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_commodity_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_response>::write_msgpack(
    const ores::refdata::messaging::get_commodity_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_commodity_response
wire_traits<ores::refdata::messaging::get_commodity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_commodity_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_commodities_request>::write_json(
    const ores::refdata::messaging::get_many_commodities_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_commodities_request
wire_traits<ores::refdata::messaging::get_many_commodities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_commodities_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_commodities_request>::write_msgpack(
    const ores::refdata::messaging::get_many_commodities_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_commodities_request
wire_traits<ores::refdata::messaging::get_many_commodities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_commodities_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_commodities_response>::write_json(
    const ores::refdata::messaging::get_many_commodities_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_commodities_response
wire_traits<ores::refdata::messaging::get_many_commodities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_commodities_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_many_commodities_response>::write_msgpack(
    const ores::refdata::messaging::get_many_commodities_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_commodities_response
wire_traits<ores::refdata::messaging::get_many_commodities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_commodities_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_commodity_request>::write_json(
    const ores::refdata::messaging::put_commodity_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_commodity_request
wire_traits<ores::refdata::messaging::put_commodity_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_commodity_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_commodity_request>::write_msgpack(
    const ores::refdata::messaging::put_commodity_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_commodity_request
wire_traits<ores::refdata::messaging::put_commodity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_commodity_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_commodity_response>::write_json(
    const ores::refdata::messaging::put_commodity_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_commodity_response
wire_traits<ores::refdata::messaging::put_commodity_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_commodity_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_commodity_response>::write_msgpack(
    const ores::refdata::messaging::put_commodity_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_commodity_response
wire_traits<ores::refdata::messaging::put_commodity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_commodity_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_commodities_request>::write_json(
    const ores::refdata::messaging::put_many_commodities_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_commodities_request
wire_traits<ores::refdata::messaging::put_many_commodities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_commodities_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_commodities_request>::write_msgpack(
    const ores::refdata::messaging::put_many_commodities_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_commodities_request
wire_traits<ores::refdata::messaging::put_many_commodities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_commodities_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_commodities_response>::write_json(
    const ores::refdata::messaging::put_many_commodities_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_commodities_response
wire_traits<ores::refdata::messaging::put_many_commodities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_commodities_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_many_commodities_response>::write_msgpack(
    const ores::refdata::messaging::put_many_commodities_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_commodities_response
wire_traits<ores::refdata::messaging::put_many_commodities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_commodities_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_commodity_request>::write_json(
    const ores::refdata::messaging::delete_commodity_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_commodity_request
wire_traits<ores::refdata::messaging::delete_commodity_request>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_commodity_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_commodity_request>::write_msgpack(
    const ores::refdata::messaging::delete_commodity_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_commodity_request
wire_traits<ores::refdata::messaging::delete_commodity_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_commodity_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_commodity_response>::write_json(
    const ores::refdata::messaging::delete_commodity_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_commodity_response
wire_traits<ores::refdata::messaging::delete_commodity_response>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_commodity_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_commodity_response>::write_msgpack(
    const ores::refdata::messaging::delete_commodity_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_commodity_response
wire_traits<ores::refdata::messaging::delete_commodity_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_commodity_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_commodities_request>::write_json(
    const ores::refdata::messaging::delete_many_commodities_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_commodities_request
wire_traits<ores::refdata::messaging::delete_many_commodities_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_commodities_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_many_commodities_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_commodities_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_commodities_request
wire_traits<ores::refdata::messaging::delete_many_commodities_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_commodities_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_many_commodities_response>::write_json(
    const ores::refdata::messaging::delete_many_commodities_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_commodities_response
wire_traits<ores::refdata::messaging::delete_many_commodities_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_commodities_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_many_commodities_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_commodities_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_commodities_response
wire_traits<ores::refdata::messaging::delete_many_commodities_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_commodities_response>(data);
}

std::string wire_traits<ores::refdata::messaging::list_commodity_versions_request>::write_json(
    const ores::refdata::messaging::list_commodity_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_commodity_versions_request
wire_traits<ores::refdata::messaging::list_commodity_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_commodity_versions_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_commodity_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_commodity_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_commodity_versions_request
wire_traits<ores::refdata::messaging::list_commodity_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_commodity_versions_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_commodity_versions_response>::write_json(
    const ores::refdata::messaging::list_commodity_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_commodity_versions_response
wire_traits<ores::refdata::messaging::list_commodity_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_commodity_versions_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_commodity_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_commodity_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_commodity_versions_response
wire_traits<ores::refdata::messaging::list_commodity_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_commodity_versions_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_version_request>::write_json(
    const ores::refdata::messaging::get_commodity_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_commodity_version_request
wire_traits<ores::refdata::messaging::get_commodity_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_commodity_version_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_version_request>::write_msgpack(
    const ores::refdata::messaging::get_commodity_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_commodity_version_request
wire_traits<ores::refdata::messaging::get_commodity_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_commodity_version_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_version_response>::write_json(
    const ores::refdata::messaging::get_commodity_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_commodity_version_response
wire_traits<ores::refdata::messaging::get_commodity_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_commodity_version_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_commodity_version_response>::write_msgpack(
    const ores::refdata::messaging::get_commodity_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_commodity_version_response
wire_traits<ores::refdata::messaging::get_commodity_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_commodity_version_response>(data);
}

}
