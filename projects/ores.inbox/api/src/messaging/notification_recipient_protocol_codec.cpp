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
#include "ores.inbox.api/domain/notification_recipient_codec.hpp"
#include "ores.inbox.api/messaging/notification_recipient_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::notification_recipient_key>::write_json(
    const ores::inbox::messaging::notification_recipient_key& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipient_key
wire_traits<ores::inbox::messaging::notification_recipient_key>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipient_key>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_key>::write_msgpack(
    const ores::inbox::messaging::notification_recipient_key& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipient_key
wire_traits<ores::inbox::messaging::notification_recipient_key>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipient_key>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_write>::write_json(
    const ores::inbox::messaging::notification_recipient_write& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipient_write
wire_traits<ores::inbox::messaging::notification_recipient_write>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipient_write>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_write>::write_msgpack(
    const ores::inbox::messaging::notification_recipient_write& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipient_write
wire_traits<ores::inbox::messaging::notification_recipient_write>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipient_write>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_change>::write_json(
    const ores::inbox::messaging::notification_recipient_change& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipient_change
wire_traits<ores::inbox::messaging::notification_recipient_change>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipient_change>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_change>::write_msgpack(
    const ores::inbox::messaging::notification_recipient_change& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipient_change
wire_traits<ores::inbox::messaging::notification_recipient_change>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipient_change>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_removal>::write_json(
    const ores::inbox::messaging::notification_recipient_removal& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipient_removal
wire_traits<ores::inbox::messaging::notification_recipient_removal>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipient_removal>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_removal>::write_msgpack(
    const ores::inbox::messaging::notification_recipient_removal& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipient_removal
wire_traits<ores::inbox::messaging::notification_recipient_removal>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipient_removal>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_lookup>::write_json(
    const ores::inbox::messaging::notification_recipient_lookup& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipient_lookup
wire_traits<ores::inbox::messaging::notification_recipient_lookup>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipient_lookup>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipient_lookup>::write_msgpack(
    const ores::inbox::messaging::notification_recipient_lookup& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipient_lookup
wire_traits<ores::inbox::messaging::notification_recipient_lookup>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipient_lookup>(data);
}

std::string wire_traits<ores::inbox::messaging::notification_recipients_filter>::write_json(
    const ores::inbox::messaging::notification_recipients_filter& v) {
    return object::write_json(v);
}

ores::inbox::messaging::notification_recipients_filter
wire_traits<ores::inbox::messaging::notification_recipients_filter>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::notification_recipients_filter>(text);
}

std::string wire_traits<ores::inbox::messaging::notification_recipients_filter>::write_msgpack(
    const ores::inbox::messaging::notification_recipients_filter& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::notification_recipients_filter
wire_traits<ores::inbox::messaging::notification_recipients_filter>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::notification_recipients_filter>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_recipients_request>::write_json(
    const ores::inbox::messaging::list_notification_recipients_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_recipients_request
wire_traits<ores::inbox::messaging::list_notification_recipients_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_recipients_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_recipients_request>::write_msgpack(
    const ores::inbox::messaging::list_notification_recipients_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_recipients_request
wire_traits<ores::inbox::messaging::list_notification_recipients_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_recipients_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_notification_recipients_response>::write_json(
    const ores::inbox::messaging::list_notification_recipients_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_notification_recipients_response
wire_traits<ores::inbox::messaging::list_notification_recipients_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_notification_recipients_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_notification_recipients_response>::write_msgpack(
    const ores::inbox::messaging::list_notification_recipients_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_notification_recipients_response
wire_traits<ores::inbox::messaging::list_notification_recipients_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_notification_recipients_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_recipient_request>::write_json(
    const ores::inbox::messaging::get_notification_recipient_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_recipient_request
wire_traits<ores::inbox::messaging::get_notification_recipient_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_recipient_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_recipient_request>::write_msgpack(
    const ores::inbox::messaging::get_notification_recipient_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_recipient_request
wire_traits<ores::inbox::messaging::get_notification_recipient_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_recipient_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_notification_recipient_response>::write_json(
    const ores::inbox::messaging::get_notification_recipient_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_notification_recipient_response
wire_traits<ores::inbox::messaging::get_notification_recipient_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_notification_recipient_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_notification_recipient_response>::write_msgpack(
    const ores::inbox::messaging::get_notification_recipient_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_notification_recipient_response
wire_traits<ores::inbox::messaging::get_notification_recipient_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_notification_recipient_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_recipients_request>::write_json(
    const ores::inbox::messaging::get_many_notification_recipients_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notification_recipients_request
wire_traits<ores::inbox::messaging::get_many_notification_recipients_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notification_recipients_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_recipients_request>::write_msgpack(
    const ores::inbox::messaging::get_many_notification_recipients_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notification_recipients_request
wire_traits<ores::inbox::messaging::get_many_notification_recipients_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notification_recipients_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_recipients_response>::write_json(
    const ores::inbox::messaging::get_many_notification_recipients_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_many_notification_recipients_response
wire_traits<ores::inbox::messaging::get_many_notification_recipients_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_many_notification_recipients_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::get_many_notification_recipients_response>::write_msgpack(
    const ores::inbox::messaging::get_many_notification_recipients_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_many_notification_recipients_response
wire_traits<ores::inbox::messaging::get_many_notification_recipients_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_many_notification_recipients_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_recipient_request>::write_json(
    const ores::inbox::messaging::put_notification_recipient_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_recipient_request
wire_traits<ores::inbox::messaging::put_notification_recipient_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_recipient_request>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_recipient_request>::write_msgpack(
    const ores::inbox::messaging::put_notification_recipient_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_recipient_request
wire_traits<ores::inbox::messaging::put_notification_recipient_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_recipient_request>(data);
}

std::string wire_traits<ores::inbox::messaging::put_notification_recipient_response>::write_json(
    const ores::inbox::messaging::put_notification_recipient_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_notification_recipient_response
wire_traits<ores::inbox::messaging::put_notification_recipient_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_notification_recipient_response>(text);
}

std::string wire_traits<ores::inbox::messaging::put_notification_recipient_response>::write_msgpack(
    const ores::inbox::messaging::put_notification_recipient_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_notification_recipient_response
wire_traits<ores::inbox::messaging::put_notification_recipient_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_notification_recipient_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_recipients_request>::write_json(
    const ores::inbox::messaging::put_many_notification_recipients_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notification_recipients_request
wire_traits<ores::inbox::messaging::put_many_notification_recipients_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notification_recipients_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_recipients_request>::write_msgpack(
    const ores::inbox::messaging::put_many_notification_recipients_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notification_recipients_request
wire_traits<ores::inbox::messaging::put_many_notification_recipients_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notification_recipients_request>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_recipients_response>::write_json(
    const ores::inbox::messaging::put_many_notification_recipients_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::put_many_notification_recipients_response
wire_traits<ores::inbox::messaging::put_many_notification_recipients_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::put_many_notification_recipients_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::put_many_notification_recipients_response>::write_msgpack(
    const ores::inbox::messaging::put_many_notification_recipients_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::put_many_notification_recipients_response
wire_traits<ores::inbox::messaging::put_many_notification_recipients_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::put_many_notification_recipients_response>(
        data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_recipient_request>::write_json(
    const ores::inbox::messaging::delete_notification_recipient_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_recipient_request
wire_traits<ores::inbox::messaging::delete_notification_recipient_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_recipient_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::delete_notification_recipient_request>::write_msgpack(
    const ores::inbox::messaging::delete_notification_recipient_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_recipient_request
wire_traits<ores::inbox::messaging::delete_notification_recipient_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_recipient_request>(
        data);
}

std::string wire_traits<ores::inbox::messaging::delete_notification_recipient_response>::write_json(
    const ores::inbox::messaging::delete_notification_recipient_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_notification_recipient_response
wire_traits<ores::inbox::messaging::delete_notification_recipient_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_notification_recipient_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::delete_notification_recipient_response>::write_msgpack(
    const ores::inbox::messaging::delete_notification_recipient_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_notification_recipient_response
wire_traits<ores::inbox::messaging::delete_notification_recipient_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::delete_notification_recipient_response>(
        data);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_request>::write_json(
    const ores::inbox::messaging::delete_many_notification_recipients_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notification_recipients_request
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notification_recipients_request>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_request>::write_msgpack(
    const ores::inbox::messaging::delete_many_notification_recipients_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notification_recipients_request
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::delete_many_notification_recipients_request>(data);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_response>::write_json(
    const ores::inbox::messaging::delete_many_notification_recipients_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::delete_many_notification_recipients_response
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::delete_many_notification_recipients_response>(
        text);
}

std::string
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_response>::write_msgpack(
    const ores::inbox::messaging::delete_many_notification_recipients_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::delete_many_notification_recipients_response
wire_traits<ores::inbox::messaging::delete_many_notification_recipients_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::delete_many_notification_recipients_response>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_request>::write_json(
    const ores::inbox::messaging::list_by_account_id_notification_recipients_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_account_id_notification_recipients_request
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_request>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::inbox::messaging::list_by_account_id_notification_recipients_request>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_request>::
    write_msgpack(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_account_id_notification_recipients_request
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_request>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_by_account_id_notification_recipients_request>(data);
}

std::string
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_response>::
    write_json(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_by_account_id_notification_recipients_response
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::inbox::messaging::list_by_account_id_notification_recipients_response>(text);
}

std::string
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_response>::
    write_msgpack(
        const ores::inbox::messaging::list_by_account_id_notification_recipients_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_by_account_id_notification_recipients_response
wire_traits<ores::inbox::messaging::list_by_account_id_notification_recipients_response>::
    read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::inbox::messaging::list_by_account_id_notification_recipients_response>(data);
}

}
