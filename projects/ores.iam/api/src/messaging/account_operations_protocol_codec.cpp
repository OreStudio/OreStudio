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
#include "ores.iam.api/domain/account_codec.hpp"
#include "ores.iam.api/domain/account_contact_information_codec.hpp"
#include "ores.iam.api/domain/login_info_codec.hpp"
#include "ores.iam.api/domain/session_codec.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::publish_accounts_from_dq_request>::write_json(
    const ores::iam::messaging::publish_accounts_from_dq_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::publish_accounts_from_dq_request
wire_traits<ores::iam::messaging::publish_accounts_from_dq_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::publish_accounts_from_dq_request>(text);
}

std::string wire_traits<ores::iam::messaging::publish_accounts_from_dq_request>::write_msgpack(
    const ores::iam::messaging::publish_accounts_from_dq_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::publish_accounts_from_dq_request
wire_traits<ores::iam::messaging::publish_accounts_from_dq_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::publish_accounts_from_dq_request>(data);
}

std::string wire_traits<ores::iam::messaging::save_account_request>::write_json(
    const ores::iam::messaging::save_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::save_account_request
wire_traits<ores::iam::messaging::save_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::save_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::save_account_request>::write_msgpack(
    const ores::iam::messaging::save_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::save_account_request
wire_traits<ores::iam::messaging::save_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::save_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::update_account_request>::write_json(
    const ores::iam::messaging::update_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_account_request
wire_traits<ores::iam::messaging::update_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::update_account_request>::write_msgpack(
    const ores::iam::messaging::update_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_account_request
wire_traits<ores::iam::messaging::update_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::update_account_response>::write_json(
    const ores::iam::messaging::update_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_account_response
wire_traits<ores::iam::messaging::update_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::update_account_response>::write_msgpack(
    const ores::iam::messaging::update_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_account_response
wire_traits<ores::iam::messaging::update_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::save_account_response>::write_json(
    const ores::iam::messaging::save_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::save_account_response
wire_traits<ores::iam::messaging::save_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::save_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::save_account_response>::write_msgpack(
    const ores::iam::messaging::save_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::save_account_response
wire_traits<ores::iam::messaging::save_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::save_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::delete_account_request>::write_json(
    const ores::iam::messaging::delete_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_account_request
wire_traits<ores::iam::messaging::delete_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::delete_account_request>::write_msgpack(
    const ores::iam::messaging::delete_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_account_request
wire_traits<ores::iam::messaging::delete_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::delete_account_response>::write_json(
    const ores::iam::messaging::delete_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::delete_account_response
wire_traits<ores::iam::messaging::delete_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::delete_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::delete_account_response>::write_msgpack(
    const ores::iam::messaging::delete_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::delete_account_response
wire_traits<ores::iam::messaging::delete_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::delete_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::account_operation_result>::write_json(
    const ores::iam::messaging::account_operation_result& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_operation_result
wire_traits<ores::iam::messaging::account_operation_result>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_operation_result>(text);
}

std::string wire_traits<ores::iam::messaging::account_operation_result>::write_msgpack(
    const ores::iam::messaging::account_operation_result& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_operation_result
wire_traits<ores::iam::messaging::account_operation_result>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_operation_result>(data);
}

std::string wire_traits<ores::iam::messaging::lock_account_request>::write_json(
    const ores::iam::messaging::lock_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::lock_account_request
wire_traits<ores::iam::messaging::lock_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::lock_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::lock_account_request>::write_msgpack(
    const ores::iam::messaging::lock_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::lock_account_request
wire_traits<ores::iam::messaging::lock_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::lock_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::lock_account_response>::write_json(
    const ores::iam::messaging::lock_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::lock_account_response
wire_traits<ores::iam::messaging::lock_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::lock_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::lock_account_response>::write_msgpack(
    const ores::iam::messaging::lock_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::lock_account_response
wire_traits<ores::iam::messaging::lock_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::lock_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::unlock_account_request>::write_json(
    const ores::iam::messaging::unlock_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::unlock_account_request
wire_traits<ores::iam::messaging::unlock_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::unlock_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::unlock_account_request>::write_msgpack(
    const ores::iam::messaging::unlock_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::unlock_account_request
wire_traits<ores::iam::messaging::unlock_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::unlock_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::unlock_account_response>::write_json(
    const ores::iam::messaging::unlock_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::unlock_account_response
wire_traits<ores::iam::messaging::unlock_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::unlock_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::unlock_account_response>::write_msgpack(
    const ores::iam::messaging::unlock_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::unlock_account_response
wire_traits<ores::iam::messaging::unlock_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::unlock_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::reset_password_request>::write_json(
    const ores::iam::messaging::reset_password_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_password_request
wire_traits<ores::iam::messaging::reset_password_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_password_request>(text);
}

std::string wire_traits<ores::iam::messaging::reset_password_request>::write_msgpack(
    const ores::iam::messaging::reset_password_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_password_request
wire_traits<ores::iam::messaging::reset_password_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_password_request>(data);
}

std::string wire_traits<ores::iam::messaging::reset_password_response>::write_json(
    const ores::iam::messaging::reset_password_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::reset_password_response
wire_traits<ores::iam::messaging::reset_password_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reset_password_response>(text);
}

std::string wire_traits<ores::iam::messaging::reset_password_response>::write_msgpack(
    const ores::iam::messaging::reset_password_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reset_password_response
wire_traits<ores::iam::messaging::reset_password_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reset_password_response>(data);
}

std::string wire_traits<ores::iam::messaging::change_password_request>::write_json(
    const ores::iam::messaging::change_password_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::change_password_request
wire_traits<ores::iam::messaging::change_password_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::change_password_request>(text);
}

std::string wire_traits<ores::iam::messaging::change_password_request>::write_msgpack(
    const ores::iam::messaging::change_password_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::change_password_request
wire_traits<ores::iam::messaging::change_password_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::change_password_request>(data);
}

std::string wire_traits<ores::iam::messaging::change_password_response>::write_json(
    const ores::iam::messaging::change_password_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::change_password_response
wire_traits<ores::iam::messaging::change_password_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::change_password_response>(text);
}

std::string wire_traits<ores::iam::messaging::change_password_response>::write_msgpack(
    const ores::iam::messaging::change_password_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::change_password_response
wire_traits<ores::iam::messaging::change_password_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::change_password_response>(data);
}

std::string wire_traits<ores::iam::messaging::update_my_email_request>::write_json(
    const ores::iam::messaging::update_my_email_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_my_email_request
wire_traits<ores::iam::messaging::update_my_email_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_my_email_request>(text);
}

std::string wire_traits<ores::iam::messaging::update_my_email_request>::write_msgpack(
    const ores::iam::messaging::update_my_email_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_my_email_request
wire_traits<ores::iam::messaging::update_my_email_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_my_email_request>(data);
}

std::string wire_traits<ores::iam::messaging::update_my_email_response>::write_json(
    const ores::iam::messaging::update_my_email_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_my_email_response
wire_traits<ores::iam::messaging::update_my_email_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_my_email_response>(text);
}

std::string wire_traits<ores::iam::messaging::update_my_email_response>::write_msgpack(
    const ores::iam::messaging::update_my_email_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_my_email_response
wire_traits<ores::iam::messaging::update_my_email_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_my_email_response>(data);
}

std::string wire_traits<ores::iam::messaging::set_my_default_party_request>::write_json(
    const ores::iam::messaging::set_my_default_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::set_my_default_party_request
wire_traits<ores::iam::messaging::set_my_default_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::set_my_default_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::set_my_default_party_request>::write_msgpack(
    const ores::iam::messaging::set_my_default_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::set_my_default_party_request
wire_traits<ores::iam::messaging::set_my_default_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::set_my_default_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::set_my_default_party_response>::write_json(
    const ores::iam::messaging::set_my_default_party_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::set_my_default_party_response
wire_traits<ores::iam::messaging::set_my_default_party_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::set_my_default_party_response>(text);
}

std::string wire_traits<ores::iam::messaging::set_my_default_party_response>::write_msgpack(
    const ores::iam::messaging::set_my_default_party_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::set_my_default_party_response
wire_traits<ores::iam::messaging::set_my_default_party_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::set_my_default_party_response>(data);
}

std::string wire_traits<ores::iam::messaging::select_party_request>::write_json(
    const ores::iam::messaging::select_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::select_party_request
wire_traits<ores::iam::messaging::select_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::select_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::select_party_request>::write_msgpack(
    const ores::iam::messaging::select_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::select_party_request
wire_traits<ores::iam::messaging::select_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::select_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::switch_party_request>::write_json(
    const ores::iam::messaging::switch_party_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::switch_party_request
wire_traits<ores::iam::messaging::switch_party_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::switch_party_request>(text);
}

std::string wire_traits<ores::iam::messaging::switch_party_request>::write_msgpack(
    const ores::iam::messaging::switch_party_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::switch_party_request
wire_traits<ores::iam::messaging::switch_party_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::switch_party_request>(data);
}

std::string wire_traits<ores::iam::messaging::select_party_response>::write_json(
    const ores::iam::messaging::select_party_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::select_party_response
wire_traits<ores::iam::messaging::select_party_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::select_party_response>(text);
}

std::string wire_traits<ores::iam::messaging::select_party_response>::write_msgpack(
    const ores::iam::messaging::select_party_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::select_party_response
wire_traits<ores::iam::messaging::select_party_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::select_party_response>(data);
}

std::string wire_traits<ores::iam::messaging::change_password_request_typed>::write_json(
    const ores::iam::messaging::change_password_request_typed& v) {
    return object::write_json(v);
}

ores::iam::messaging::change_password_request_typed
wire_traits<ores::iam::messaging::change_password_request_typed>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::change_password_request_typed>(text);
}

std::string wire_traits<ores::iam::messaging::change_password_request_typed>::write_msgpack(
    const ores::iam::messaging::change_password_request_typed& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::change_password_request_typed
wire_traits<ores::iam::messaging::change_password_request_typed>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::change_password_request_typed>(data);
}

std::string wire_traits<ores::iam::messaging::update_self_account_request>::write_json(
    const ores::iam::messaging::update_self_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_self_account_request
wire_traits<ores::iam::messaging::update_self_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_self_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::update_self_account_request>::write_msgpack(
    const ores::iam::messaging::update_self_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_self_account_request
wire_traits<ores::iam::messaging::update_self_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_self_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::update_self_account_response>::write_json(
    const ores::iam::messaging::update_self_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_self_account_response
wire_traits<ores::iam::messaging::update_self_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::update_self_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::update_self_account_response>::write_msgpack(
    const ores::iam::messaging::update_self_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_self_account_response
wire_traits<ores::iam::messaging::update_self_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::update_self_account_response>(data);
}

std::string
wire_traits<ores::iam::messaging::update_self_account_contact_information_request>::write_json(
    const ores::iam::messaging::update_self_account_contact_information_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_self_account_contact_information_request
wire_traits<ores::iam::messaging::update_self_account_contact_information_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::update_self_account_contact_information_request>(
        text);
}

std::string
wire_traits<ores::iam::messaging::update_self_account_contact_information_request>::write_msgpack(
    const ores::iam::messaging::update_self_account_contact_information_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_self_account_contact_information_request
wire_traits<ores::iam::messaging::update_self_account_contact_information_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::update_self_account_contact_information_request>(data);
}

std::string
wire_traits<ores::iam::messaging::update_self_account_contact_information_response>::write_json(
    const ores::iam::messaging::update_self_account_contact_information_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::update_self_account_contact_information_response
wire_traits<ores::iam::messaging::update_self_account_contact_information_response>::read_json(
    std::string_view text) {
    return object::read_json<
        ores::iam::messaging::update_self_account_contact_information_response>(text);
}

std::string
wire_traits<ores::iam::messaging::update_self_account_contact_information_response>::write_msgpack(
    const ores::iam::messaging::update_self_account_contact_information_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::update_self_account_contact_information_response
wire_traits<ores::iam::messaging::update_self_account_contact_information_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<
        ores::iam::messaging::update_self_account_contact_information_response>(data);
}

std::string
wire_traits<ores::iam::messaging::get_my_account_contact_information_request>::write_json(
    const ores::iam::messaging::get_my_account_contact_information_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_account_contact_information_request
wire_traits<ores::iam::messaging::get_my_account_contact_information_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_account_contact_information_request>(
        text);
}

std::string
wire_traits<ores::iam::messaging::get_my_account_contact_information_request>::write_msgpack(
    const ores::iam::messaging::get_my_account_contact_information_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_account_contact_information_request
wire_traits<ores::iam::messaging::get_my_account_contact_information_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_account_contact_information_request>(
        data);
}

std::string
wire_traits<ores::iam::messaging::get_my_account_contact_information_response>::write_json(
    const ores::iam::messaging::get_my_account_contact_information_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_account_contact_information_response
wire_traits<ores::iam::messaging::get_my_account_contact_information_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_account_contact_information_response>(
        text);
}

std::string
wire_traits<ores::iam::messaging::get_my_account_contact_information_response>::write_msgpack(
    const ores::iam::messaging::get_my_account_contact_information_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_account_contact_information_response
wire_traits<ores::iam::messaging::get_my_account_contact_information_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_account_contact_information_response>(
        data);
}

std::string wire_traits<ores::iam::messaging::get_my_account_request>::write_json(
    const ores::iam::messaging::get_my_account_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_account_request
wire_traits<ores::iam::messaging::get_my_account_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_account_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_account_request>::write_msgpack(
    const ores::iam::messaging::get_my_account_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_account_request
wire_traits<ores::iam::messaging::get_my_account_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_account_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_account_response>::write_json(
    const ores::iam::messaging::get_my_account_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_account_response
wire_traits<ores::iam::messaging::get_my_account_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_account_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_account_response>::write_msgpack(
    const ores::iam::messaging::get_my_account_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_account_response
wire_traits<ores::iam::messaging::get_my_account_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_account_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_login_info_request>::write_json(
    const ores::iam::messaging::get_my_login_info_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_login_info_request
wire_traits<ores::iam::messaging::get_my_login_info_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_login_info_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_login_info_request>::write_msgpack(
    const ores::iam::messaging::get_my_login_info_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_login_info_request
wire_traits<ores::iam::messaging::get_my_login_info_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_login_info_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_login_info_response>::write_json(
    const ores::iam::messaging::get_my_login_info_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_login_info_response
wire_traits<ores::iam::messaging::get_my_login_info_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_login_info_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_login_info_response>::write_msgpack(
    const ores::iam::messaging::get_my_login_info_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_login_info_response
wire_traits<ores::iam::messaging::get_my_login_info_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_login_info_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_sessions_request>::write_json(
    const ores::iam::messaging::get_my_sessions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_sessions_request
wire_traits<ores::iam::messaging::get_my_sessions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_sessions_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_sessions_request>::write_msgpack(
    const ores::iam::messaging::get_my_sessions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_sessions_request
wire_traits<ores::iam::messaging::get_my_sessions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_sessions_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_sessions_response>::write_json(
    const ores::iam::messaging::get_my_sessions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_sessions_response
wire_traits<ores::iam::messaging::get_my_sessions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_sessions_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_sessions_response>::write_msgpack(
    const ores::iam::messaging::get_my_sessions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_sessions_response
wire_traits<ores::iam::messaging::get_my_sessions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_sessions_response>(data);
}

std::string
wire_traits<ores::iam::messaging::my_party>::write_json(const ores::iam::messaging::my_party& v) {
    return object::write_json(v);
}

ores::iam::messaging::my_party
wire_traits<ores::iam::messaging::my_party>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::my_party>(text);
}

std::string wire_traits<ores::iam::messaging::my_party>::write_msgpack(
    const ores::iam::messaging::my_party& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::my_party
wire_traits<ores::iam::messaging::my_party>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::my_party>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_parties_request>::write_json(
    const ores::iam::messaging::get_my_parties_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_parties_request
wire_traits<ores::iam::messaging::get_my_parties_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_parties_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_parties_request>::write_msgpack(
    const ores::iam::messaging::get_my_parties_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_parties_request
wire_traits<ores::iam::messaging::get_my_parties_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_parties_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_parties_response>::write_json(
    const ores::iam::messaging::get_my_parties_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_parties_response
wire_traits<ores::iam::messaging::get_my_parties_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_parties_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_parties_response>::write_msgpack(
    const ores::iam::messaging::get_my_parties_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_parties_response
wire_traits<ores::iam::messaging::get_my_parties_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_parties_response>(data);
}

std::string wire_traits<ores::iam::messaging::set_reporting_line_request>::write_json(
    const ores::iam::messaging::set_reporting_line_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::set_reporting_line_request
wire_traits<ores::iam::messaging::set_reporting_line_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::set_reporting_line_request>(text);
}

std::string wire_traits<ores::iam::messaging::set_reporting_line_request>::write_msgpack(
    const ores::iam::messaging::set_reporting_line_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::set_reporting_line_request
wire_traits<ores::iam::messaging::set_reporting_line_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::set_reporting_line_request>(data);
}

std::string wire_traits<ores::iam::messaging::set_reporting_line_response>::write_json(
    const ores::iam::messaging::set_reporting_line_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::set_reporting_line_response
wire_traits<ores::iam::messaging::set_reporting_line_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::set_reporting_line_response>(text);
}

std::string wire_traits<ores::iam::messaging::set_reporting_line_response>::write_msgpack(
    const ores::iam::messaging::set_reporting_line_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::set_reporting_line_response
wire_traits<ores::iam::messaging::set_reporting_line_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::set_reporting_line_response>(data);
}

std::string wire_traits<ores::iam::messaging::reporting_tree_party>::write_json(
    const ores::iam::messaging::reporting_tree_party& v) {
    return object::write_json(v);
}

ores::iam::messaging::reporting_tree_party
wire_traits<ores::iam::messaging::reporting_tree_party>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reporting_tree_party>(text);
}

std::string wire_traits<ores::iam::messaging::reporting_tree_party>::write_msgpack(
    const ores::iam::messaging::reporting_tree_party& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reporting_tree_party
wire_traits<ores::iam::messaging::reporting_tree_party>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reporting_tree_party>(data);
}

std::string wire_traits<ores::iam::messaging::reporting_tree_node>::write_json(
    const ores::iam::messaging::reporting_tree_node& v) {
    return object::write_json(v);
}

ores::iam::messaging::reporting_tree_node
wire_traits<ores::iam::messaging::reporting_tree_node>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::reporting_tree_node>(text);
}

std::string wire_traits<ores::iam::messaging::reporting_tree_node>::write_msgpack(
    const ores::iam::messaging::reporting_tree_node& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::reporting_tree_node
wire_traits<ores::iam::messaging::reporting_tree_node>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::reporting_tree_node>(data);
}

std::string wire_traits<ores::iam::messaging::get_reporting_tree_request>::write_json(
    const ores::iam::messaging::get_reporting_tree_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_reporting_tree_request
wire_traits<ores::iam::messaging::get_reporting_tree_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_reporting_tree_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_reporting_tree_request>::write_msgpack(
    const ores::iam::messaging::get_reporting_tree_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_reporting_tree_request
wire_traits<ores::iam::messaging::get_reporting_tree_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_reporting_tree_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_reporting_tree_response>::write_json(
    const ores::iam::messaging::get_reporting_tree_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_reporting_tree_response
wire_traits<ores::iam::messaging::get_reporting_tree_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_reporting_tree_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_reporting_tree_response>::write_msgpack(
    const ores::iam::messaging::get_reporting_tree_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_reporting_tree_response
wire_traits<ores::iam::messaging::get_reporting_tree_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_reporting_tree_response>(data);
}

std::string wire_traits<ores::iam::messaging::attach_account_pictures_request>::write_json(
    const ores::iam::messaging::attach_account_pictures_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::attach_account_pictures_request
wire_traits<ores::iam::messaging::attach_account_pictures_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::attach_account_pictures_request>(text);
}

std::string wire_traits<ores::iam::messaging::attach_account_pictures_request>::write_msgpack(
    const ores::iam::messaging::attach_account_pictures_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::attach_account_pictures_request
wire_traits<ores::iam::messaging::attach_account_pictures_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::attach_account_pictures_request>(data);
}

std::string wire_traits<ores::iam::messaging::attach_account_pictures_response>::write_json(
    const ores::iam::messaging::attach_account_pictures_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::attach_account_pictures_response
wire_traits<ores::iam::messaging::attach_account_pictures_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::attach_account_pictures_response>(text);
}

std::string wire_traits<ores::iam::messaging::attach_account_pictures_response>::write_msgpack(
    const ores::iam::messaging::attach_account_pictures_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::attach_account_pictures_response
wire_traits<ores::iam::messaging::attach_account_pictures_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::attach_account_pictures_response>(data);
}

}
