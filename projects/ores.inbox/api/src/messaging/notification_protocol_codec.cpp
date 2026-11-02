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
#include "ores.inbox.api/domain/notification_codec.hpp"
#include "ores.inbox.api/messaging/notification_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::notification_key>::write_json(
    const ores::inbox::messaging::notification_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_key
wire_traits<ores::inbox::messaging::notification_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_key>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_key>::write_msgpack(
    const ores::inbox::messaging::notification_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_key
wire_traits<ores::inbox::messaging::notification_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_key>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_write>::write_json(
    const ores::inbox::messaging::notification_write& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_write
wire_traits<ores::inbox::messaging::notification_write>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_write>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_write>::write_msgpack(
    const ores::inbox::messaging::notification_write& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_write
wire_traits<ores::inbox::messaging::notification_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_write>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_change>::write_json(
    const ores::inbox::messaging::notification_change& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_change
wire_traits<ores::inbox::messaging::notification_change>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_change>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_change>::write_msgpack(
    const ores::inbox::messaging::notification_change& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_change
wire_traits<ores::inbox::messaging::notification_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_change>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_removal>::write_json(
    const ores::inbox::messaging::notification_removal& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_removal
wire_traits<ores::inbox::messaging::notification_removal>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_removal>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_removal>::write_msgpack(
    const ores::inbox::messaging::notification_removal& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_removal
wire_traits<ores::inbox::messaging::notification_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_removal>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_lookup>::write_json(
    const ores::inbox::messaging::notification_lookup& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_lookup
wire_traits<ores::inbox::messaging::notification_lookup>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_lookup>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_lookup>::write_msgpack(
    const ores::inbox::messaging::notification_lookup& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_lookup
wire_traits<ores::inbox::messaging::notification_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_lookup>(data);
}

std::string wire_traits<ores::inbox::messaging::notifications_filter>::write_json(
    const ores::inbox::messaging::notifications_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notifications_filter
wire_traits<ores::inbox::messaging::notifications_filter>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notifications_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::notifications_filter>::write_msgpack(
    const ores::inbox::messaging::notifications_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notifications_filter
wire_traits<ores::inbox::messaging::notifications_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notifications_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_event>::write_json(
    const ores::inbox::messaging::notification_event& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_event
wire_traits<ores::inbox::messaging::notification_event>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_event>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_event>::write_msgpack(
    const ores::inbox::messaging::notification_event& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_event
wire_traits<ores::inbox::messaging::notification_event>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_event>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_version_key>::write_json(
    const ores::inbox::messaging::notification_version_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_version_key
wire_traits<ores::inbox::messaging::notification_version_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_version_key>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_version_key>::write_msgpack(
    const ores::inbox::messaging::notification_version_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_version_key
wire_traits<ores::inbox::messaging::notification_version_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_version_key>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_versions_filter>::write_json(
    const ores::inbox::messaging::notification_versions_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_versions_filter
wire_traits<ores::inbox::messaging::notification_versions_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_versions_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_versions_filter>::write_msgpack(
    const ores::inbox::messaging::notification_versions_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_versions_filter
wire_traits<ores::inbox::messaging::notification_versions_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_versions_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notifications_request>::write_json(
    const ores::inbox::messaging::list_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notifications_request
wire_traits<ores::inbox::messaging::list_notifications_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_notifications_request>::write_msgpack(
    const ores::inbox::messaging::list_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notifications_request
wire_traits<ores::inbox::messaging::list_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notifications_response>::write_json(
    const ores::inbox::messaging::list_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notifications_response
wire_traits<ores::inbox::messaging::list_notifications_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_notifications_response>::write_msgpack(
    const ores::inbox::messaging::list_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notifications_response
wire_traits<ores::inbox::messaging::list_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notifications_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_request>::write_json(
    const ores::inbox::messaging::get_notification_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_request
wire_traits<ores::inbox::messaging::get_notification_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_request>::write_msgpack(
    const ores::inbox::messaging::get_notification_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_request
wire_traits<ores::inbox::messaging::get_notification_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_response>::write_json(
    const ores::inbox::messaging::get_notification_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_response
wire_traits<ores::inbox::messaging::get_notification_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_response>::write_msgpack(
    const ores::inbox::messaging::get_notification_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_response
wire_traits<ores::inbox::messaging::get_notification_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_many_notifications_request>::write_json(
    const ores::inbox::messaging::get_many_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notifications_request
wire_traits<ores::inbox::messaging::get_many_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_many_notifications_request>::write_msgpack(
    const ores::inbox::messaging::get_many_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notifications_request
wire_traits<ores::inbox::messaging::get_many_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_many_notifications_response>::write_json(
    const ores::inbox::messaging::get_many_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notifications_response
wire_traits<ores::inbox::messaging::get_many_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_many_notifications_response>::write_msgpack(
    const ores::inbox::messaging::get_many_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notifications_response
wire_traits<ores::inbox::messaging::get_many_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notifications_response>(data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_request>::write_json(
    const ores::inbox::messaging::put_notification_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_request
wire_traits<ores::inbox::messaging::put_notification_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_request>::write_msgpack(
    const ores::inbox::messaging::put_notification_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_request
wire_traits<ores::inbox::messaging::put_notification_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_response>::write_json(
    const ores::inbox::messaging::put_notification_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_response
wire_traits<ores::inbox::messaging::put_notification_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_response>::write_msgpack(
    const ores::inbox::messaging::put_notification_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_response
wire_traits<ores::inbox::messaging::put_notification_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_response>(data);
}

std::string wire_traits<ores::inbox::messaging::put_many_notifications_request>::write_json(
    const ores::inbox::messaging::put_many_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notifications_request
wire_traits<ores::inbox::messaging::put_many_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_many_notifications_request>::write_msgpack(
    const ores::inbox::messaging::put_many_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notifications_request
wire_traits<ores::inbox::messaging::put_many_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_many_notifications_response>::write_json(
    const ores::inbox::messaging::put_many_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notifications_response
wire_traits<ores::inbox::messaging::put_many_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_many_notifications_response>::write_msgpack(
    const ores::inbox::messaging::put_many_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notifications_response
wire_traits<ores::inbox::messaging::put_many_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notifications_response>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_request>::write_json(
    const ores::inbox::messaging::delete_notification_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_request
wire_traits<ores::inbox::messaging::delete_notification_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_request>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_request>::write_msgpack(
    const ores::inbox::messaging::delete_notification_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_request
wire_traits<ores::inbox::messaging::delete_notification_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_request>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_response>::write_json(
    const ores::inbox::messaging::delete_notification_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_response
wire_traits<ores::inbox::messaging::delete_notification_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_response>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_response>::write_msgpack(
    const ores::inbox::messaging::delete_notification_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_response
wire_traits<ores::inbox::messaging::delete_notification_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_response>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_many_notifications_request>::write_json(
    const ores::inbox::messaging::delete_many_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notifications_request
wire_traits<ores::inbox::messaging::delete_many_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_many_notifications_request>::write_msgpack(
    const ores::inbox::messaging::delete_many_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notifications_request
wire_traits<ores::inbox::messaging::delete_many_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_many_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::delete_many_notifications_response>::write_json(
    const ores::inbox::messaging::delete_many_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notifications_response
wire_traits<ores::inbox::messaging::delete_many_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::delete_many_notifications_response>::write_msgpack(
    const ores::inbox::messaging::delete_many_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notifications_response
wire_traits<ores::inbox::messaging::delete_many_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_many_notifications_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_request>::write_json(
    const ores::inbox::messaging::list_by_kind_code_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_kind_code_notifications_request
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_by_kind_code_notifications_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_request>::write_msgpack(
    const ores::inbox::messaging::list_by_kind_code_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_kind_code_notifications_request
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_by_kind_code_notifications_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_response>::write_json(
    const ores::inbox::messaging::list_by_kind_code_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_kind_code_notifications_response
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_by_kind_code_notifications_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_response>::write_msgpack(
    const ores::inbox::messaging::list_by_kind_code_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_kind_code_notifications_response
wire_traits<ores::inbox::messaging::list_by_kind_code_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_by_kind_code_notifications_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_versions_request>::write_json(
    const ores::inbox::messaging::list_notification_versions_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_versions_request
wire_traits<ores::inbox::messaging::list_notification_versions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_versions_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_notification_versions_request>::write_msgpack(
    const ores::inbox::messaging::list_notification_versions_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_versions_request
wire_traits<ores::inbox::messaging::list_notification_versions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_versions_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_versions_response>::write_json(
    const ores::inbox::messaging::list_notification_versions_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_versions_response
wire_traits<ores::inbox::messaging::list_notification_versions_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_versions_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_notification_versions_response>::write_msgpack(
    const ores::inbox::messaging::list_notification_versions_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_versions_response
wire_traits<ores::inbox::messaging::list_notification_versions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_versions_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_version_request>::write_json(
    const ores::inbox::messaging::get_notification_version_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_version_request
wire_traits<ores::inbox::messaging::get_notification_version_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_version_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_version_request>::write_msgpack(
    const ores::inbox::messaging::get_notification_version_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_version_request
wire_traits<ores::inbox::messaging::get_notification_version_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_version_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_version_response>::write_json(
    const ores::inbox::messaging::get_notification_version_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_version_response
wire_traits<ores::inbox::messaging::get_notification_version_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_version_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_version_response>::write_msgpack(
    const ores::inbox::messaging::get_notification_version_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_version_response
wire_traits<ores::inbox::messaging::get_notification_version_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_version_response>(data);
}

}
