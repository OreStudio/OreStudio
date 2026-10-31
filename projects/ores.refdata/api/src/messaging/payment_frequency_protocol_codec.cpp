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
#include "ores.refdata.api/domain/payment_frequency_codec.hpp"
#include "ores.refdata.api/messaging/payment_frequency_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::refdata::messaging::payment_frequency_key>::write_json(
    const ores::refdata::messaging::payment_frequency_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_key
wire_traits<ores::refdata::messaging::payment_frequency_key>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_key>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_key>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_key
wire_traits<ores::refdata::messaging::payment_frequency_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_key>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_write>::write_json(
    const ores::refdata::messaging::payment_frequency_write& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_write
wire_traits<ores::refdata::messaging::payment_frequency_write>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_write>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_write>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_write& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_write
wire_traits<ores::refdata::messaging::payment_frequency_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_write>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_change>::write_json(
    const ores::refdata::messaging::payment_frequency_change& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_change
wire_traits<ores::refdata::messaging::payment_frequency_change>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_change>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_change>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_change& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_change
wire_traits<ores::refdata::messaging::payment_frequency_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_change>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_removal>::write_json(
    const ores::refdata::messaging::payment_frequency_removal& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_removal
wire_traits<ores::refdata::messaging::payment_frequency_removal>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_removal>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_removal>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_removal& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_removal
wire_traits<ores::refdata::messaging::payment_frequency_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_removal>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_lookup>::write_json(
    const ores::refdata::messaging::payment_frequency_lookup& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_lookup
wire_traits<ores::refdata::messaging::payment_frequency_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_lookup>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_lookup>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_lookup& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_lookup
wire_traits<ores::refdata::messaging::payment_frequency_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_lookup>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequencies_filter>::write_json(
    const ores::refdata::messaging::payment_frequencies_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequencies_filter
wire_traits<ores::refdata::messaging::payment_frequencies_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequencies_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequencies_filter>::write_msgpack(
    const ores::refdata::messaging::payment_frequencies_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequencies_filter
wire_traits<ores::refdata::messaging::payment_frequencies_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequencies_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_event>::write_json(
    const ores::refdata::messaging::payment_frequency_event& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_event
wire_traits<ores::refdata::messaging::payment_frequency_event>::read_json(std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_event>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_event>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_event& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_event
wire_traits<ores::refdata::messaging::payment_frequency_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_event>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_version_key>::write_json(
    const ores::refdata::messaging::payment_frequency_version_key& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_version_key
wire_traits<ores::refdata::messaging::payment_frequency_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_version_key>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_version_key>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_version_key& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_version_key
wire_traits<ores::refdata::messaging::payment_frequency_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_version_key>(data);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_versions_filter>::write_json(
    const ores::refdata::messaging::payment_frequency_versions_filter& v) {
    return object::write_json(v);
}

ores::refdata::messaging::payment_frequency_versions_filter
wire_traits<ores::refdata::messaging::payment_frequency_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::payment_frequency_versions_filter>(text);
}

std::string wire_traits<ores::refdata::messaging::payment_frequency_versions_filter>::write_msgpack(
    const ores::refdata::messaging::payment_frequency_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::payment_frequency_versions_filter
wire_traits<ores::refdata::messaging::payment_frequency_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::payment_frequency_versions_filter>(data);
}

std::string wire_traits<ores::refdata::messaging::list_payment_frequencies_request>::write_json(
    const ores::refdata::messaging::list_payment_frequencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_payment_frequencies_request
wire_traits<ores::refdata::messaging::list_payment_frequencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_payment_frequencies_request>(text);
}

std::string wire_traits<ores::refdata::messaging::list_payment_frequencies_request>::write_msgpack(
    const ores::refdata::messaging::list_payment_frequencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_payment_frequencies_request
wire_traits<ores::refdata::messaging::list_payment_frequencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_payment_frequencies_request>(data);
}

std::string wire_traits<ores::refdata::messaging::list_payment_frequencies_response>::write_json(
    const ores::refdata::messaging::list_payment_frequencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_payment_frequencies_response
wire_traits<ores::refdata::messaging::list_payment_frequencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_payment_frequencies_response>(text);
}

std::string wire_traits<ores::refdata::messaging::list_payment_frequencies_response>::write_msgpack(
    const ores::refdata::messaging::list_payment_frequencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_payment_frequencies_response
wire_traits<ores::refdata::messaging::list_payment_frequencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_payment_frequencies_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_payment_frequency_request>::write_json(
    const ores::refdata::messaging::get_payment_frequency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_payment_frequency_request
wire_traits<ores::refdata::messaging::get_payment_frequency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_payment_frequency_request>(text);
}

std::string wire_traits<ores::refdata::messaging::get_payment_frequency_request>::write_msgpack(
    const ores::refdata::messaging::get_payment_frequency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_payment_frequency_request
wire_traits<ores::refdata::messaging::get_payment_frequency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_payment_frequency_request>(data);
}

std::string wire_traits<ores::refdata::messaging::get_payment_frequency_response>::write_json(
    const ores::refdata::messaging::get_payment_frequency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_payment_frequency_response
wire_traits<ores::refdata::messaging::get_payment_frequency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_payment_frequency_response>(text);
}

std::string wire_traits<ores::refdata::messaging::get_payment_frequency_response>::write_msgpack(
    const ores::refdata::messaging::get_payment_frequency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_payment_frequency_response
wire_traits<ores::refdata::messaging::get_payment_frequency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_payment_frequency_response>(data);
}

std::string wire_traits<ores::refdata::messaging::get_many_payment_frequencies_request>::write_json(
    const ores::refdata::messaging::get_many_payment_frequencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_payment_frequencies_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_request>::write_msgpack(
    const ores::refdata::messaging::get_many_payment_frequencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_payment_frequencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_response>::write_json(
    const ores::refdata::messaging::get_many_payment_frequencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_many_payment_frequencies_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_response>::write_msgpack(
    const ores::refdata::messaging::get_many_payment_frequencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::get_many_payment_frequencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_many_payment_frequencies_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::put_payment_frequency_request>::write_json(
    const ores::refdata::messaging::put_payment_frequency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_payment_frequency_request
wire_traits<ores::refdata::messaging::put_payment_frequency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_payment_frequency_request>(text);
}

std::string wire_traits<ores::refdata::messaging::put_payment_frequency_request>::write_msgpack(
    const ores::refdata::messaging::put_payment_frequency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_payment_frequency_request
wire_traits<ores::refdata::messaging::put_payment_frequency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_payment_frequency_request>(data);
}

std::string wire_traits<ores::refdata::messaging::put_payment_frequency_response>::write_json(
    const ores::refdata::messaging::put_payment_frequency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_payment_frequency_response
wire_traits<ores::refdata::messaging::put_payment_frequency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_payment_frequency_response>(text);
}

std::string wire_traits<ores::refdata::messaging::put_payment_frequency_response>::write_msgpack(
    const ores::refdata::messaging::put_payment_frequency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_payment_frequency_response
wire_traits<ores::refdata::messaging::put_payment_frequency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_payment_frequency_response>(data);
}

std::string wire_traits<ores::refdata::messaging::put_many_payment_frequencies_request>::write_json(
    const ores::refdata::messaging::put_many_payment_frequencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_payment_frequencies_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_request>::write_msgpack(
    const ores::refdata::messaging::put_many_payment_frequencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_payment_frequencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_response>::write_json(
    const ores::refdata::messaging::put_many_payment_frequencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::put_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::put_many_payment_frequencies_response>(text);
}

std::string
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_response>::write_msgpack(
    const ores::refdata::messaging::put_many_payment_frequencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::put_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::put_many_payment_frequencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::put_many_payment_frequencies_response>(
        data);
}

std::string wire_traits<ores::refdata::messaging::delete_payment_frequency_request>::write_json(
    const ores::refdata::messaging::delete_payment_frequency_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_payment_frequency_request
wire_traits<ores::refdata::messaging::delete_payment_frequency_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_payment_frequency_request>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_payment_frequency_request>::write_msgpack(
    const ores::refdata::messaging::delete_payment_frequency_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_payment_frequency_request
wire_traits<ores::refdata::messaging::delete_payment_frequency_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_payment_frequency_request>(data);
}

std::string wire_traits<ores::refdata::messaging::delete_payment_frequency_response>::write_json(
    const ores::refdata::messaging::delete_payment_frequency_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_payment_frequency_response
wire_traits<ores::refdata::messaging::delete_payment_frequency_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_payment_frequency_response>(text);
}

std::string wire_traits<ores::refdata::messaging::delete_payment_frequency_response>::write_msgpack(
    const ores::refdata::messaging::delete_payment_frequency_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_payment_frequency_response
wire_traits<ores::refdata::messaging::delete_payment_frequency_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_payment_frequency_response>(data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_request>::write_json(
    const ores::refdata::messaging::delete_many_payment_frequencies_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_payment_frequencies_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_request>::write_msgpack(
    const ores::refdata::messaging::delete_many_payment_frequencies_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_payment_frequencies_request
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_payment_frequencies_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_response>::write_json(
    const ores::refdata::messaging::delete_many_payment_frequencies_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::delete_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::delete_many_payment_frequencies_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_response>::write_msgpack(
    const ores::refdata::messaging::delete_many_payment_frequencies_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::delete_many_payment_frequencies_response
wire_traits<ores::refdata::messaging::delete_many_payment_frequencies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::delete_many_payment_frequencies_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_request>::write_json(
    const ores::refdata::messaging::list_payment_frequency_versions_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_payment_frequency_versions_request
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_payment_frequency_versions_request>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_request>::write_msgpack(
    const ores::refdata::messaging::list_payment_frequency_versions_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_payment_frequency_versions_request
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_payment_frequency_versions_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_response>::write_json(
    const ores::refdata::messaging::list_payment_frequency_versions_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::list_payment_frequency_versions_response
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::list_payment_frequency_versions_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_response>::write_msgpack(
    const ores::refdata::messaging::list_payment_frequency_versions_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::list_payment_frequency_versions_response
wire_traits<ores::refdata::messaging::list_payment_frequency_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::list_payment_frequency_versions_response>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_payment_frequency_version_request>::write_json(
    const ores::refdata::messaging::get_payment_frequency_version_request& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_payment_frequency_version_request
wire_traits<ores::refdata::messaging::get_payment_frequency_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_payment_frequency_version_request>(text);
}

std::string
wire_traits<ores::refdata::messaging::get_payment_frequency_version_request>::write_msgpack(
    const ores::refdata::messaging::get_payment_frequency_version_request& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_payment_frequency_version_request
wire_traits<ores::refdata::messaging::get_payment_frequency_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_payment_frequency_version_request>(
        data);
}

std::string
wire_traits<ores::refdata::messaging::get_payment_frequency_version_response>::write_json(
    const ores::refdata::messaging::get_payment_frequency_version_response& v) {
    return object::write_json(v);
}

ores::refdata::messaging::get_payment_frequency_version_response
wire_traits<ores::refdata::messaging::get_payment_frequency_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::refdata::messaging::get_payment_frequency_version_response>(
        text);
}

std::string
wire_traits<ores::refdata::messaging::get_payment_frequency_version_response>::write_msgpack(
    const ores::refdata::messaging::get_payment_frequency_version_response& v) {
    return object::write_msgpack(v);
}

ores::refdata::messaging::get_payment_frequency_version_response
wire_traits<ores::refdata::messaging::get_payment_frequency_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::refdata::messaging::get_payment_frequency_version_response>(
        data);
}

}
