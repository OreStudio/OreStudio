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
#include "ores.refdata.api/domain/csa_eligible_currency_codec.hpp"
#include "ores.refdata.api/messaging/csa_eligible_currency_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_key>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_key
wire_traits<ores::refdata::messaging::csa_eligible_currency_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_key>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_key>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_key
wire_traits<ores::refdata::messaging::csa_eligible_currency_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_key>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_write>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_write
wire_traits<ores::refdata::messaging::csa_eligible_currency_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_write>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_write>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_write
wire_traits<ores::refdata::messaging::csa_eligible_currency_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_write>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_change>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_change
wire_traits<ores::refdata::messaging::csa_eligible_currency_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_change>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_change>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_change
wire_traits<ores::refdata::messaging::csa_eligible_currency_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_change>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_removal>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_removal
wire_traits<ores::refdata::messaging::csa_eligible_currency_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_removal>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_removal
wire_traits<ores::refdata::messaging::csa_eligible_currency_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_lookup>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_lookup
wire_traits<ores::refdata::messaging::csa_eligible_currency_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_lookup>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_lookup
wire_traits<ores::refdata::messaging::csa_eligible_currency_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currencies_filter>::write_json(
    const ores::refdata::messaging::csa_eligible_currencies_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currencies_filter
wire_traits<ores::refdata::messaging::csa_eligible_currencies_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currencies_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currencies_filter>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currencies_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currencies_filter
wire_traits<ores::refdata::messaging::csa_eligible_currencies_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currencies_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_event>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_event
wire_traits<ores::refdata::messaging::csa_eligible_currency_event>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_event>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_event>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_event
wire_traits<ores::refdata::messaging::csa_eligible_currency_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_event>(data);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_version_key>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_version_key
wire_traits<ores::refdata::messaging::csa_eligible_currency_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::csa_eligible_currency_version_key>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_version_key
wire_traits<ores::refdata::messaging::csa_eligible_currency_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_version_key>(data);
}

std::string
wire_traits<ores::refdata::messaging::csa_eligible_currency_versions_filter>::write_json(
    const ores::refdata::messaging::csa_eligible_currency_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::csa_eligible_currency_versions_filter
wire_traits<ores::refdata::messaging::csa_eligible_currency_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::csa_eligible_currency_versions_filter>(text);
}

std::string
wire_traits<ores::refdata::messaging::csa_eligible_currency_versions_filter>::write_msgpack(
    const ores::refdata::messaging::csa_eligible_currency_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::csa_eligible_currency_versions_filter
wire_traits<ores::refdata::messaging::csa_eligible_currency_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::csa_eligible_currency_versions_filter>(
        data);
}

std::string wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_request>::write_json(
    const ores::refdata::messaging::list_csa_eligible_currencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_csa_eligible_currencies_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_request>::write_msgpack(
    const ores::refdata::messaging::list_csa_eligible_currencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_csa_eligible_currencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_response>::write_json(
    const ores::refdata::messaging::list_csa_eligible_currencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_csa_eligible_currencies_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_response>::write_msgpack(
    const ores::refdata::messaging::list_csa_eligible_currencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::list_csa_eligible_currencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_csa_eligible_currencies_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::get_csa_eligible_currency_request>::write_json(
    const ores::refdata::messaging::get_csa_eligible_currency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_csa_eligible_currency_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_csa_eligible_currency_request>::write_msgpack(
    const ores::refdata::messaging::get_csa_eligible_currency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_csa_eligible_currency_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_csa_eligible_currency_response>::write_json(
    const ores::refdata::messaging::get_csa_eligible_currency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_csa_eligible_currency_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_response>::write_msgpack(
    const ores::refdata::messaging::get_csa_eligible_currency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_csa_eligible_currency_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_request>::write_json(
    const ores::refdata::messaging::get_many_csa_eligible_currencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_csa_eligible_currencies_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_request>::write_msgpack(
    const ores::refdata::messaging::get_many_csa_eligible_currencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_csa_eligible_currencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_response>::write_json(
    const ores::refdata::messaging::get_many_csa_eligible_currencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_csa_eligible_currencies_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_response>::write_msgpack(
    const ores::refdata::messaging::get_many_csa_eligible_currencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::get_many_csa_eligible_currencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_many_csa_eligible_currencies_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_csa_eligible_currency_request>::write_json(
    const ores::refdata::messaging::put_csa_eligible_currency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::put_csa_eligible_currency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_csa_eligible_currency_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_csa_eligible_currency_request>::write_msgpack(
    const ores::refdata::messaging::put_csa_eligible_currency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::put_csa_eligible_currency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_csa_eligible_currency_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_csa_eligible_currency_response>::write_json(
    const ores::refdata::messaging::put_csa_eligible_currency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::put_csa_eligible_currency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_csa_eligible_currency_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_csa_eligible_currency_response>::write_msgpack(
    const ores::refdata::messaging::put_csa_eligible_currency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::put_csa_eligible_currency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_csa_eligible_currency_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_request>::write_json(
    const ores::refdata::messaging::put_many_csa_eligible_currencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_csa_eligible_currencies_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_request>::write_msgpack(
    const ores::refdata::messaging::put_many_csa_eligible_currencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_csa_eligible_currencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_response>::write_json(
    const ores::refdata::messaging::put_many_csa_eligible_currencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_csa_eligible_currencies_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_response>::write_msgpack(
    const ores::refdata::messaging::put_many_csa_eligible_currencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::put_many_csa_eligible_currencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::put_many_csa_eligible_currencies_response>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_request>::write_json(
    const ores::refdata::messaging::delete_csa_eligible_currency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_csa_eligible_currency_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_request>::write_msgpack(
    const ores::refdata::messaging::delete_csa_eligible_currency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_csa_eligible_currency_request
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_csa_eligible_currency_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_response>::write_json(
    const ores::refdata::messaging::delete_csa_eligible_currency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_csa_eligible_currency_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_response>::write_msgpack(
    const ores::refdata::messaging::delete_csa_eligible_currency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_csa_eligible_currency_response
wire_traits<ores::refdata::messaging::delete_csa_eligible_currency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_csa_eligible_currency_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_request>::write_json(
    const ores::refdata::messaging::delete_many_csa_eligible_currencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_csa_eligible_currencies_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_csa_eligible_currencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_many_csa_eligible_currencies_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_response>::write_json(
    const ores::refdata::messaging::delete_many_csa_eligible_currencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::delete_many_csa_eligible_currencies_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_csa_eligible_currencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::delete_many_csa_eligible_currencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::delete_many_csa_eligible_currencies_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>::write_json(
    const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>::
    write_msgpack(
        const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>::write_json(
    const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>::
    write_msgpack(
        const ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response
wire_traits<ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_by_csa_id_csa_eligible_currencies_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_request>::write_json(
    const ores::refdata::messaging::list_csa_eligible_currency_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_csa_eligible_currency_versions_request
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_csa_eligible_currency_versions_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_csa_eligible_currency_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_csa_eligible_currency_versions_request
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_csa_eligible_currency_versions_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_response>::write_json(
    const ores::refdata::messaging::list_csa_eligible_currency_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_csa_eligible_currency_versions_response
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::refdata::messaging::list_csa_eligible_currency_versions_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_csa_eligible_currency_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_csa_eligible_currency_versions_response
wire_traits<ores::refdata::messaging::list_csa_eligible_currency_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::list_csa_eligible_currency_versions_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_request>::write_json(
    const ores::refdata::messaging::get_csa_eligible_currency_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_csa_eligible_currency_version_request
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_csa_eligible_currency_version_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_request>::write_msgpack(
    const ores::refdata::messaging::get_csa_eligible_currency_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_csa_eligible_currency_version_request
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_csa_eligible_currency_version_request>(data);
}

std::string
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_response>::write_json(
    const ores::refdata::messaging::get_csa_eligible_currency_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_csa_eligible_currency_version_response
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_csa_eligible_currency_version_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_response>::write_msgpack(
    const ores::refdata::messaging::get_csa_eligible_currency_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_csa_eligible_currency_version_response
wire_traits<ores::refdata::messaging::get_csa_eligible_currency_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::refdata::messaging::get_csa_eligible_currency_version_response>(data);
}

}
