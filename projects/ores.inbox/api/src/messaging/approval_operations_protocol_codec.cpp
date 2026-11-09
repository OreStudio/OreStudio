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
#include "ores.inbox.api/domain/approval_request_codec.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::inbox::messaging::raise_approval_request_request>::write_json(
    const ores::inbox::messaging::raise_approval_request_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::raise_approval_request_request
wire_traits<ores::inbox::messaging::raise_approval_request_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::raise_approval_request_request>(text);
}

std::string wire_traits<ores::inbox::messaging::raise_approval_request_request>::write_msgpack(
    const ores::inbox::messaging::raise_approval_request_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::raise_approval_request_request
wire_traits<ores::inbox::messaging::raise_approval_request_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::raise_approval_request_request>(data);
}

std::string wire_traits<ores::inbox::messaging::raise_approval_request_response>::write_json(
    const ores::inbox::messaging::raise_approval_request_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::raise_approval_request_response
wire_traits<ores::inbox::messaging::raise_approval_request_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::raise_approval_request_response>(text);
}

std::string wire_traits<ores::inbox::messaging::raise_approval_request_response>::write_msgpack(
    const ores::inbox::messaging::raise_approval_request_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::raise_approval_request_response
wire_traits<ores::inbox::messaging::raise_approval_request_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::raise_approval_request_response>(data);
}

std::string wire_traits<ores::inbox::messaging::withdraw_approval_request_request>::write_json(
    const ores::inbox::messaging::withdraw_approval_request_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::withdraw_approval_request_request
wire_traits<ores::inbox::messaging::withdraw_approval_request_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::withdraw_approval_request_request>(text);
}

std::string wire_traits<ores::inbox::messaging::withdraw_approval_request_request>::write_msgpack(
    const ores::inbox::messaging::withdraw_approval_request_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::withdraw_approval_request_request
wire_traits<ores::inbox::messaging::withdraw_approval_request_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::withdraw_approval_request_request>(data);
}

std::string wire_traits<ores::inbox::messaging::withdraw_approval_request_response>::write_json(
    const ores::inbox::messaging::withdraw_approval_request_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::withdraw_approval_request_response
wire_traits<ores::inbox::messaging::withdraw_approval_request_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::withdraw_approval_request_response>(text);
}

std::string wire_traits<ores::inbox::messaging::withdraw_approval_request_response>::write_msgpack(
    const ores::inbox::messaging::withdraw_approval_request_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::withdraw_approval_request_response
wire_traits<ores::inbox::messaging::withdraw_approval_request_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::withdraw_approval_request_response>(data);
}

std::string wire_traits<ores::inbox::messaging::decide_approval_request_request>::write_json(
    const ores::inbox::messaging::decide_approval_request_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::decide_approval_request_request
wire_traits<ores::inbox::messaging::decide_approval_request_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::decide_approval_request_request>(text);
}

std::string wire_traits<ores::inbox::messaging::decide_approval_request_request>::write_msgpack(
    const ores::inbox::messaging::decide_approval_request_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::decide_approval_request_request
wire_traits<ores::inbox::messaging::decide_approval_request_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::decide_approval_request_request>(data);
}

std::string wire_traits<ores::inbox::messaging::decide_approval_request_response>::write_json(
    const ores::inbox::messaging::decide_approval_request_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::decide_approval_request_response
wire_traits<ores::inbox::messaging::decide_approval_request_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::decide_approval_request_response>(text);
}

std::string wire_traits<ores::inbox::messaging::decide_approval_request_response>::write_msgpack(
    const ores::inbox::messaging::decide_approval_request_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::decide_approval_request_response
wire_traits<ores::inbox::messaging::decide_approval_request_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::decide_approval_request_response>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_queue_request>::write_json(
    const ores::inbox::messaging::list_approval_queue_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_queue_request
wire_traits<ores::inbox::messaging::list_approval_queue_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_queue_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_approval_queue_request>::write_msgpack(
    const ores::inbox::messaging::list_approval_queue_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_queue_request
wire_traits<ores::inbox::messaging::list_approval_queue_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_queue_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_approval_queue_response>::write_json(
    const ores::inbox::messaging::list_approval_queue_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_approval_queue_response
wire_traits<ores::inbox::messaging::list_approval_queue_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_approval_queue_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_approval_queue_response>::write_msgpack(
    const ores::inbox::messaging::list_approval_queue_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_approval_queue_response
wire_traits<ores::inbox::messaging::list_approval_queue_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_approval_queue_response>(data);
}

std::string wire_traits<ores::inbox::messaging::list_my_approval_requests_request>::write_json(
    const ores::inbox::messaging::list_my_approval_requests_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_my_approval_requests_request
wire_traits<ores::inbox::messaging::list_my_approval_requests_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_my_approval_requests_request>(text);
}

std::string wire_traits<ores::inbox::messaging::list_my_approval_requests_request>::write_msgpack(
    const ores::inbox::messaging::list_my_approval_requests_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_my_approval_requests_request
wire_traits<ores::inbox::messaging::list_my_approval_requests_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_my_approval_requests_request>(data);
}

std::string wire_traits<ores::inbox::messaging::list_my_approval_requests_response>::write_json(
    const ores::inbox::messaging::list_my_approval_requests_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::list_my_approval_requests_response
wire_traits<ores::inbox::messaging::list_my_approval_requests_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::list_my_approval_requests_response>(text);
}

std::string wire_traits<ores::inbox::messaging::list_my_approval_requests_response>::write_msgpack(
    const ores::inbox::messaging::list_my_approval_requests_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::list_my_approval_requests_response
wire_traits<ores::inbox::messaging::list_my_approval_requests_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::list_my_approval_requests_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approvals_mode_request>::write_json(
    const ores::inbox::messaging::get_approvals_mode_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approvals_mode_request
wire_traits<ores::inbox::messaging::get_approvals_mode_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approvals_mode_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approvals_mode_request>::write_msgpack(
    const ores::inbox::messaging::get_approvals_mode_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approvals_mode_request
wire_traits<ores::inbox::messaging::get_approvals_mode_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approvals_mode_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approvals_mode_response>::write_json(
    const ores::inbox::messaging::get_approvals_mode_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approvals_mode_response
wire_traits<ores::inbox::messaging::get_approvals_mode_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approvals_mode_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approvals_mode_response>::write_msgpack(
    const ores::inbox::messaging::get_approvals_mode_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approvals_mode_response
wire_traits<ores::inbox::messaging::get_approvals_mode_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approvals_mode_response>(data);
}

std::string wire_traits<ores::inbox::messaging::set_approvals_mode_request>::write_json(
    const ores::inbox::messaging::set_approvals_mode_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::set_approvals_mode_request
wire_traits<ores::inbox::messaging::set_approvals_mode_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::set_approvals_mode_request>(text);
}

std::string wire_traits<ores::inbox::messaging::set_approvals_mode_request>::write_msgpack(
    const ores::inbox::messaging::set_approvals_mode_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::set_approvals_mode_request
wire_traits<ores::inbox::messaging::set_approvals_mode_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::set_approvals_mode_request>(data);
}

std::string wire_traits<ores::inbox::messaging::set_approvals_mode_response>::write_json(
    const ores::inbox::messaging::set_approvals_mode_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::set_approvals_mode_response
wire_traits<ores::inbox::messaging::set_approvals_mode_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::set_approvals_mode_response>(text);
}

std::string wire_traits<ores::inbox::messaging::set_approvals_mode_response>::write_msgpack(
    const ores::inbox::messaging::set_approvals_mode_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::set_approvals_mode_response
wire_traits<ores::inbox::messaging::set_approvals_mode_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::set_approvals_mode_response>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_request>::write_json(
    const ores::inbox::messaging::get_approval_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_request
wire_traits<ores::inbox::messaging::get_approval_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_request>::write_msgpack(
    const ores::inbox::messaging::get_approval_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_request
wire_traits<ores::inbox::messaging::get_approval_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_response>::write_json(
    const ores::inbox::messaging::get_approval_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_response
wire_traits<ores::inbox::messaging::get_approval_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_response>::write_msgpack(
    const ores::inbox::messaging::get_approval_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_response
wire_traits<ores::inbox::messaging::get_approval_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_response>(data);
}

std::string wire_traits<ores::inbox::messaging::expire_overdue_approvals_request>::write_json(
    const ores::inbox::messaging::expire_overdue_approvals_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::expire_overdue_approvals_request
wire_traits<ores::inbox::messaging::expire_overdue_approvals_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::expire_overdue_approvals_request>(text);
}

std::string wire_traits<ores::inbox::messaging::expire_overdue_approvals_request>::write_msgpack(
    const ores::inbox::messaging::expire_overdue_approvals_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::expire_overdue_approvals_request
wire_traits<ores::inbox::messaging::expire_overdue_approvals_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::expire_overdue_approvals_request>(data);
}

std::string wire_traits<ores::inbox::messaging::expire_overdue_approvals_response>::write_json(
    const ores::inbox::messaging::expire_overdue_approvals_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::expire_overdue_approvals_response
wire_traits<ores::inbox::messaging::expire_overdue_approvals_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::expire_overdue_approvals_response>(text);
}

std::string wire_traits<ores::inbox::messaging::expire_overdue_approvals_response>::write_msgpack(
    const ores::inbox::messaging::expire_overdue_approvals_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::expire_overdue_approvals_response
wire_traits<ores::inbox::messaging::expire_overdue_approvals_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::expire_overdue_approvals_response>(data);
}

std::string wire_traits<ores::inbox::messaging::remind_expiring_approvals_request>::write_json(
    const ores::inbox::messaging::remind_expiring_approvals_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::remind_expiring_approvals_request
wire_traits<ores::inbox::messaging::remind_expiring_approvals_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::remind_expiring_approvals_request>(text);
}

std::string wire_traits<ores::inbox::messaging::remind_expiring_approvals_request>::write_msgpack(
    const ores::inbox::messaging::remind_expiring_approvals_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::remind_expiring_approvals_request
wire_traits<ores::inbox::messaging::remind_expiring_approvals_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::remind_expiring_approvals_request>(data);
}

std::string wire_traits<ores::inbox::messaging::remind_expiring_approvals_response>::write_json(
    const ores::inbox::messaging::remind_expiring_approvals_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::remind_expiring_approvals_response
wire_traits<ores::inbox::messaging::remind_expiring_approvals_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::remind_expiring_approvals_response>(text);
}

std::string wire_traits<ores::inbox::messaging::remind_expiring_approvals_response>::write_msgpack(
    const ores::inbox::messaging::remind_expiring_approvals_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::remind_expiring_approvals_response
wire_traits<ores::inbox::messaging::remind_expiring_approvals_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::remind_expiring_approvals_response>(data);
}

std::string wire_traits<ores::inbox::messaging::retry_stalled_applies_request>::write_json(
    const ores::inbox::messaging::retry_stalled_applies_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::retry_stalled_applies_request
wire_traits<ores::inbox::messaging::retry_stalled_applies_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::retry_stalled_applies_request>(text);
}

std::string wire_traits<ores::inbox::messaging::retry_stalled_applies_request>::write_msgpack(
    const ores::inbox::messaging::retry_stalled_applies_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::retry_stalled_applies_request
wire_traits<ores::inbox::messaging::retry_stalled_applies_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::retry_stalled_applies_request>(data);
}

std::string wire_traits<ores::inbox::messaging::retry_stalled_applies_response>::write_json(
    const ores::inbox::messaging::retry_stalled_applies_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::retry_stalled_applies_response
wire_traits<ores::inbox::messaging::retry_stalled_applies_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::retry_stalled_applies_response>(text);
}

std::string wire_traits<ores::inbox::messaging::retry_stalled_applies_response>::write_msgpack(
    const ores::inbox::messaging::retry_stalled_applies_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::retry_stalled_applies_response
wire_traits<ores::inbox::messaging::retry_stalled_applies_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::retry_stalled_applies_response>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_request_field>::write_json(
    const ores::inbox::messaging::approval_request_field& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_request_field
wire_traits<ores::inbox::messaging::approval_request_field>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_request_field>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_request_field>::write_msgpack(
    const ores::inbox::messaging::approval_request_field& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_request_field
wire_traits<ores::inbox::messaging::approval_request_field>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_request_field>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_request_version>::write_json(
    const ores::inbox::messaging::approval_request_version& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_request_version
wire_traits<ores::inbox::messaging::approval_request_version>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_request_version>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_request_version>::write_msgpack(
    const ores::inbox::messaging::approval_request_version& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_request_version
wire_traits<ores::inbox::messaging::approval_request_version>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_request_version>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_history_request>::write_json(
    const ores::inbox::messaging::get_approval_history_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_history_request
wire_traits<ores::inbox::messaging::get_approval_history_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_history_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_history_request>::write_msgpack(
    const ores::inbox::messaging::get_approval_history_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_history_request
wire_traits<ores::inbox::messaging::get_approval_history_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_history_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_history_response>::write_json(
    const ores::inbox::messaging::get_approval_history_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_history_response
wire_traits<ores::inbox::messaging::get_approval_history_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_history_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_history_response>::write_msgpack(
    const ores::inbox::messaging::get_approval_history_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_history_response
wire_traits<ores::inbox::messaging::get_approval_history_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_history_response>(data);
}

std::string wire_traits<ores::inbox::messaging::approval_part_progress>::write_json(
    const ores::inbox::messaging::approval_part_progress& v) {
    return object::write_json(v);
}

ores::inbox::messaging::approval_part_progress
wire_traits<ores::inbox::messaging::approval_part_progress>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::approval_part_progress>(text);
}

std::string wire_traits<ores::inbox::messaging::approval_part_progress>::write_msgpack(
    const ores::inbox::messaging::approval_part_progress& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::approval_part_progress
wire_traits<ores::inbox::messaging::approval_part_progress>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::approval_part_progress>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_parts_request>::write_json(
    const ores::inbox::messaging::get_approval_parts_request& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_parts_request
wire_traits<ores::inbox::messaging::get_approval_parts_request>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_parts_request>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_parts_request>::write_msgpack(
    const ores::inbox::messaging::get_approval_parts_request& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_parts_request
wire_traits<ores::inbox::messaging::get_approval_parts_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_parts_request>(data);
}

std::string wire_traits<ores::inbox::messaging::get_approval_parts_response>::write_json(
    const ores::inbox::messaging::get_approval_parts_response& v) {
    return object::write_json(v);
}

ores::inbox::messaging::get_approval_parts_response
wire_traits<ores::inbox::messaging::get_approval_parts_response>::read_json(std::string_view text) {
    return object::read_json<ores::inbox::messaging::get_approval_parts_response>(text);
}

std::string wire_traits<ores::inbox::messaging::get_approval_parts_response>::write_msgpack(
    const ores::inbox::messaging::get_approval_parts_response& v) {
    return object::write_msgpack(v);
}

ores::inbox::messaging::get_approval_parts_response
wire_traits<ores::inbox::messaging::get_approval_parts_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::inbox::messaging::get_approval_parts_response>(data);
}

}
