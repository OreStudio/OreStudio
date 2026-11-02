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
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::notification_argument_value>::write_json(
    const ores::inbox::messaging::notification_argument_value& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_argument_value
wire_traits<ores::inbox::messaging::notification_argument_value>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_argument_value>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_argument_value>::write_msgpack(
    const ores::inbox::messaging::notification_argument_value& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_argument_value
wire_traits<ores::inbox::messaging::notification_argument_value>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_argument_value>(data);
}

std::string wire_traits<ores::inbox::messaging::inbox_notification>::write_json(
    const ores::inbox::messaging::inbox_notification& v) {
    return object::write_json(v);
}

ores::inbox::messaging::inbox_notification
wire_traits<ores::inbox::messaging::inbox_notification>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::inbox_notification>(text);
}

std::string wire_traits<ores::inbox::messaging::inbox_notification>::write_msgpack(
    const ores::inbox::messaging::inbox_notification& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::inbox_notification
wire_traits<ores::inbox::messaging::inbox_notification>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::inbox_notification>(data);
}

std::string wire_traits<ores::inbox::messaging::raise_notification_request>::write_json(
    const ores::inbox::messaging::raise_notification_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::raise_notification_request
wire_traits<ores::inbox::messaging::raise_notification_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::raise_notification_request>(text);
}

std::string wire_traits<ores::inbox::messaging::raise_notification_request>::write_msgpack(
    const ores::inbox::messaging::raise_notification_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::raise_notification_request
wire_traits<ores::inbox::messaging::raise_notification_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::raise_notification_request>(data);
}

std::string wire_traits<ores::inbox::messaging::raise_notification_response>::write_json(
    const ores::inbox::messaging::raise_notification_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::raise_notification_response
wire_traits<ores::inbox::messaging::raise_notification_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::raise_notification_response>(text);
}

std::string wire_traits<ores::inbox::messaging::raise_notification_response>::write_msgpack(
    const ores::inbox::messaging::raise_notification_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::raise_notification_response
wire_traits<ores::inbox::messaging::raise_notification_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::raise_notification_response>(data);
}

std::string wire_traits<ores::inbox::messaging::list_my_notifications_request>::write_json(
    const ores::inbox::messaging::list_my_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_my_notifications_request
wire_traits<ores::inbox::messaging::list_my_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_my_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_my_notifications_request>::write_msgpack(
    const ores::inbox::messaging::list_my_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_my_notifications_request
wire_traits<ores::inbox::messaging::list_my_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_my_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_my_notifications_response>::write_json(
    const ores::inbox::messaging::list_my_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_my_notifications_response
wire_traits<ores::inbox::messaging::list_my_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_my_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_my_notifications_response>::write_msgpack(
    const ores::inbox::messaging::list_my_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_my_notifications_response
wire_traits<ores::inbox::messaging::list_my_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_my_notifications_response>(data);
}

std::string wire_traits<ores::inbox::messaging::count_unread_notifications_request>::write_json(
    const ores::inbox::messaging::count_unread_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::count_unread_notifications_request
wire_traits<ores::inbox::messaging::count_unread_notifications_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::count_unread_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::count_unread_notifications_request>::write_msgpack(
    const ores::inbox::messaging::count_unread_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::count_unread_notifications_request
wire_traits<ores::inbox::messaging::count_unread_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::count_unread_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::count_unread_notifications_response>::write_json(
    const ores::inbox::messaging::count_unread_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::count_unread_notifications_response
wire_traits<ores::inbox::messaging::count_unread_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::count_unread_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::count_unread_notifications_response>::write_msgpack(
    const ores::inbox::messaging::count_unread_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::count_unread_notifications_response
wire_traits<ores::inbox::messaging::count_unread_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::count_unread_notifications_response>(data);
}

std::string wire_traits<ores::inbox::messaging::mark_notifications_read_request>::write_json(
    const ores::inbox::messaging::mark_notifications_read_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::mark_notifications_read_request
wire_traits<ores::inbox::messaging::mark_notifications_read_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::mark_notifications_read_request>(text);
}

std::string wire_traits<ores::inbox::messaging::mark_notifications_read_request>::write_msgpack(
    const ores::inbox::messaging::mark_notifications_read_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::mark_notifications_read_request
wire_traits<ores::inbox::messaging::mark_notifications_read_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::mark_notifications_read_request>(data);
}

std::string wire_traits<ores::inbox::messaging::mark_notifications_read_response>::write_json(
    const ores::inbox::messaging::mark_notifications_read_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::mark_notifications_read_response
wire_traits<ores::inbox::messaging::mark_notifications_read_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::mark_notifications_read_response>(text);
}

std::string wire_traits<ores::inbox::messaging::mark_notifications_read_response>::write_msgpack(
    const ores::inbox::messaging::mark_notifications_read_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::mark_notifications_read_response
wire_traits<ores::inbox::messaging::mark_notifications_read_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::mark_notifications_read_response>(data);
}

std::string wire_traits<ores::inbox::messaging::clear_notifications_request>::write_json(
    const ores::inbox::messaging::clear_notifications_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::clear_notifications_request
wire_traits<ores::inbox::messaging::clear_notifications_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::clear_notifications_request>(text);
}

std::string wire_traits<ores::inbox::messaging::clear_notifications_request>::write_msgpack(
    const ores::inbox::messaging::clear_notifications_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::clear_notifications_request
wire_traits<ores::inbox::messaging::clear_notifications_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::clear_notifications_request>(data);
}

std::string wire_traits<ores::inbox::messaging::clear_notifications_response>::write_json(
    const ores::inbox::messaging::clear_notifications_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::clear_notifications_response
wire_traits<ores::inbox::messaging::clear_notifications_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::clear_notifications_response>(text);
}

std::string wire_traits<ores::inbox::messaging::clear_notifications_response>::write_msgpack(
    const ores::inbox::messaging::clear_notifications_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::clear_notifications_response
wire_traits<ores::inbox::messaging::clear_notifications_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::clear_notifications_response>(data);
}

}
