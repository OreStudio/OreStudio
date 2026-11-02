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
#include "ores.inbox.api/domain/delivery_outcome.hpp"
#include "ores.inbox.api/domain/notification_delivery_codec.hpp"
#include "ores.inbox.api/messaging/notification_delivery_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::notification_delivery_key>::write_json(
    const ores::inbox::messaging::notification_delivery_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_key
wire_traits<ores::inbox::messaging::notification_delivery_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_key>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_key>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_key
wire_traits<ores::inbox::messaging::notification_delivery_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_key>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_write>::write_json(
    const ores::inbox::messaging::notification_delivery_write& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_write
wire_traits<ores::inbox::messaging::notification_delivery_write>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_write>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_write>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_write& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_write
wire_traits<ores::inbox::messaging::notification_delivery_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_write>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_change>::write_json(
    const ores::inbox::messaging::notification_delivery_change& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_change
wire_traits<ores::inbox::messaging::notification_delivery_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_change>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_change>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_change& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_change
wire_traits<ores::inbox::messaging::notification_delivery_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_change>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_removal>::write_json(
    const ores::inbox::messaging::notification_delivery_removal& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_removal
wire_traits<ores::inbox::messaging::notification_delivery_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_removal>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_removal>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_removal& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_removal
wire_traits<ores::inbox::messaging::notification_delivery_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_removal>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_lookup>::write_json(
    const ores::inbox::messaging::notification_delivery_lookup& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_lookup
wire_traits<ores::inbox::messaging::notification_delivery_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_lookup>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_lookup>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_lookup& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_lookup
wire_traits<ores::inbox::messaging::notification_delivery_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_lookup>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_deliveries_filter>::write_json(
    const ores::inbox::messaging::notification_deliveries_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_deliveries_filter
wire_traits<ores::inbox::messaging::notification_deliveries_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_deliveries_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_deliveries_filter>::write_msgpack(
    const ores::inbox::messaging::notification_deliveries_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_deliveries_filter
wire_traits<ores::inbox::messaging::notification_deliveries_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_deliveries_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_event>::write_json(
    const ores::inbox::messaging::notification_delivery_event& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_event
wire_traits<ores::inbox::messaging::notification_delivery_event>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_event>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_event>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_event& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_event
wire_traits<ores::inbox::messaging::notification_delivery_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_event>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_version_key>::write_json(
    const ores::inbox::messaging::notification_delivery_version_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_version_key
wire_traits<ores::inbox::messaging::notification_delivery_version_key>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_version_key>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_version_key>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_version_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_version_key
wire_traits<ores::inbox::messaging::notification_delivery_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_version_key>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_delivery_versions_filter>::write_json(
    const ores::inbox::messaging::notification_delivery_versions_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_delivery_versions_filter
wire_traits<ores::inbox::messaging::notification_delivery_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_delivery_versions_filter>(text);
}

std::string
wire_traits<ores::inbox::messaging::notification_delivery_versions_filter>::write_msgpack(
    const ores::inbox::messaging::notification_delivery_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_delivery_versions_filter
wire_traits<ores::inbox::messaging::notification_delivery_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_delivery_versions_filter>(
        data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_deliveries_request>::write_json(
    const ores::inbox::messaging::list_notification_deliveries_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_deliveries_request
wire_traits<ores::inbox::messaging::list_notification_deliveries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_deliveries_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_deliveries_request>::write_msgpack(
    const ores::inbox::messaging::list_notification_deliveries_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_deliveries_request
wire_traits<ores::inbox::messaging::list_notification_deliveries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_deliveries_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_deliveries_response>::write_json(
    const ores::inbox::messaging::list_notification_deliveries_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_deliveries_response
wire_traits<ores::inbox::messaging::list_notification_deliveries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_deliveries_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_deliveries_response>::write_msgpack(
    const ores::inbox::messaging::list_notification_deliveries_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_deliveries_response
wire_traits<ores::inbox::messaging::list_notification_deliveries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_deliveries_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_delivery_request>::write_json(
    const ores::inbox::messaging::get_notification_delivery_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_delivery_request
wire_traits<ores::inbox::messaging::get_notification_delivery_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_delivery_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_delivery_request>::write_msgpack(
    const ores::inbox::messaging::get_notification_delivery_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_delivery_request
wire_traits<ores::inbox::messaging::get_notification_delivery_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_delivery_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_delivery_response>::write_json(
    const ores::inbox::messaging::get_notification_delivery_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_delivery_response
wire_traits<ores::inbox::messaging::get_notification_delivery_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_delivery_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_delivery_response>::write_msgpack(
    const ores::inbox::messaging::get_notification_delivery_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_delivery_response
wire_traits<ores::inbox::messaging::get_notification_delivery_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_delivery_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_request>::write_json(
    const ores::inbox::messaging::get_many_notification_deliveries_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notification_deliveries_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_request>::write_msgpack(
    const ores::inbox::messaging::get_many_notification_deliveries_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notification_deliveries_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_response>::write_json(
    const ores::inbox::messaging::get_many_notification_deliveries_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notification_deliveries_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_response>::write_msgpack(
    const ores::inbox::messaging::get_many_notification_deliveries_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::get_many_notification_deliveries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notification_deliveries_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_delivery_request>::write_json(
    const ores::inbox::messaging::put_notification_delivery_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_delivery_request
wire_traits<ores::inbox::messaging::put_notification_delivery_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_delivery_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_delivery_request>::write_msgpack(
    const ores::inbox::messaging::put_notification_delivery_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_delivery_request
wire_traits<ores::inbox::messaging::put_notification_delivery_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_delivery_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_delivery_response>::write_json(
    const ores::inbox::messaging::put_notification_delivery_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_delivery_response
wire_traits<ores::inbox::messaging::put_notification_delivery_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_delivery_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_delivery_response>::write_msgpack(
    const ores::inbox::messaging::put_notification_delivery_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_delivery_response
wire_traits<ores::inbox::messaging::put_notification_delivery_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_delivery_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_request>::write_json(
    const ores::inbox::messaging::put_many_notification_deliveries_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notification_deliveries_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_request>::write_msgpack(
    const ores::inbox::messaging::put_many_notification_deliveries_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notification_deliveries_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_response>::write_json(
    const ores::inbox::messaging::put_many_notification_deliveries_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notification_deliveries_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_response>::write_msgpack(
    const ores::inbox::messaging::put_many_notification_deliveries_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::put_many_notification_deliveries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notification_deliveries_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_delivery_request>::write_json(
    const ores::inbox::messaging::delete_notification_delivery_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_delivery_request
wire_traits<ores::inbox::messaging::delete_notification_delivery_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_delivery_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::delete_notification_delivery_request>::write_msgpack(
    const ores::inbox::messaging::delete_notification_delivery_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_delivery_request
wire_traits<ores::inbox::messaging::delete_notification_delivery_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_delivery_request>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_delivery_response>::write_json(
    const ores::inbox::messaging::delete_notification_delivery_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_delivery_response
wire_traits<ores::inbox::messaging::delete_notification_delivery_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_delivery_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::delete_notification_delivery_response>::write_msgpack(
    const ores::inbox::messaging::delete_notification_delivery_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_delivery_response
wire_traits<ores::inbox::messaging::delete_notification_delivery_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_delivery_response>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_request>::write_json(
    const ores::inbox::messaging::delete_many_notification_deliveries_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notification_deliveries_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_request>::write_msgpack(
    const ores::inbox::messaging::delete_many_notification_deliveries_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notification_deliveries_request
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::delete_many_notification_deliveries_request>(data);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_response>::write_json(
    const ores::inbox::messaging::delete_many_notification_deliveries_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notification_deliveries_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_response>::write_msgpack(
    const ores::inbox::messaging::delete_many_notification_deliveries_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notification_deliveries_response
wire_traits<ores::inbox::messaging::delete_many_notification_deliveries_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::delete_many_notification_deliveries_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>::
    write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_notification_id_notification_deliveries_request
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>::
    write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_notification_id_notification_deliveries_request
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_by_notification_id_notification_deliveries_request>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>::
    write_json(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_notification_id_notification_deliveries_response
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>::
    read_json(std::string_view text) {
    return object::read_json<
        ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>::
    write_msgpack(
        const ores::inbox::messaging::list_by_notification_id_notification_deliveries_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_notification_id_notification_deliveries_response
wire_traits<ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_by_notification_id_notification_deliveries_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_request>::write_json(
    const ores::inbox::messaging::list_notification_delivery_versions_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_delivery_versions_request
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_delivery_versions_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_request>::write_msgpack(
    const ores::inbox::messaging::list_notification_delivery_versions_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_delivery_versions_request
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_notification_delivery_versions_request>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_response>::write_json(
    const ores::inbox::messaging::list_notification_delivery_versions_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_delivery_versions_response
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_delivery_versions_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_response>::write_msgpack(
    const ores::inbox::messaging::list_notification_delivery_versions_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_delivery_versions_response
wire_traits<ores::inbox::messaging::list_notification_delivery_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_notification_delivery_versions_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::get_notification_delivery_version_request>::write_json(
    const ores::inbox::messaging::get_notification_delivery_version_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_delivery_version_request
wire_traits<ores::inbox::messaging::get_notification_delivery_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_delivery_version_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_notification_delivery_version_request>::write_msgpack(
    const ores::inbox::messaging::get_notification_delivery_version_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_delivery_version_request
wire_traits<ores::inbox::messaging::get_notification_delivery_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_delivery_version_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::get_notification_delivery_version_response>::write_json(
    const ores::inbox::messaging::get_notification_delivery_version_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_delivery_version_response
wire_traits<ores::inbox::messaging::get_notification_delivery_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_delivery_version_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_notification_delivery_version_response>::write_msgpack(
    const ores::inbox::messaging::get_notification_delivery_version_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_delivery_version_response
wire_traits<ores::inbox::messaging::get_notification_delivery_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_delivery_version_response>(
        data);
}

}
