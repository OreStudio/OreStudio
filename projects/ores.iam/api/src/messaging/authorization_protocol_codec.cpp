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
#include "ores.iam.api/domain/role_codec.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"

namespace ores::utility::codec {

std::string wire_traits<ores::iam::messaging::assign_role_request>::write_json(
    const ores::iam::messaging::assign_role_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::assign_role_request
wire_traits<ores::iam::messaging::assign_role_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::assign_role_request>(text);
}

std::string wire_traits<ores::iam::messaging::assign_role_request>::write_msgpack(
    const ores::iam::messaging::assign_role_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::assign_role_request
wire_traits<ores::iam::messaging::assign_role_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::assign_role_request>(data);
}

std::string wire_traits<ores::iam::messaging::assign_role_response>::write_json(
    const ores::iam::messaging::assign_role_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::assign_role_response
wire_traits<ores::iam::messaging::assign_role_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::assign_role_response>(text);
}

std::string wire_traits<ores::iam::messaging::assign_role_response>::write_msgpack(
    const ores::iam::messaging::assign_role_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::assign_role_response
wire_traits<ores::iam::messaging::assign_role_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::assign_role_response>(data);
}

std::string wire_traits<ores::iam::messaging::assign_role_by_name_response>::write_json(
    const ores::iam::messaging::assign_role_by_name_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::assign_role_by_name_response
wire_traits<ores::iam::messaging::assign_role_by_name_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::assign_role_by_name_response>(text);
}

std::string wire_traits<ores::iam::messaging::assign_role_by_name_response>::write_msgpack(
    const ores::iam::messaging::assign_role_by_name_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::assign_role_by_name_response
wire_traits<ores::iam::messaging::assign_role_by_name_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::assign_role_by_name_response>(data);
}

std::string wire_traits<ores::iam::messaging::assign_role_by_name_request>::write_json(
    const ores::iam::messaging::assign_role_by_name_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::assign_role_by_name_request
wire_traits<ores::iam::messaging::assign_role_by_name_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::assign_role_by_name_request>(text);
}

std::string wire_traits<ores::iam::messaging::assign_role_by_name_request>::write_msgpack(
    const ores::iam::messaging::assign_role_by_name_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::assign_role_by_name_request
wire_traits<ores::iam::messaging::assign_role_by_name_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::assign_role_by_name_request>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_role_request>::write_json(
    const ores::iam::messaging::revoke_role_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_role_request
wire_traits<ores::iam::messaging::revoke_role_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_role_request>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_role_request>::write_msgpack(
    const ores::iam::messaging::revoke_role_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_role_request
wire_traits<ores::iam::messaging::revoke_role_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_role_request>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_role_response>::write_json(
    const ores::iam::messaging::revoke_role_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_role_response
wire_traits<ores::iam::messaging::revoke_role_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_role_response>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_role_response>::write_msgpack(
    const ores::iam::messaging::revoke_role_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_role_response
wire_traits<ores::iam::messaging::revoke_role_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_role_response>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_role_by_name_response>::write_json(
    const ores::iam::messaging::revoke_role_by_name_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_role_by_name_response
wire_traits<ores::iam::messaging::revoke_role_by_name_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_role_by_name_response>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_role_by_name_response>::write_msgpack(
    const ores::iam::messaging::revoke_role_by_name_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_role_by_name_response
wire_traits<ores::iam::messaging::revoke_role_by_name_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_role_by_name_response>(data);
}

std::string wire_traits<ores::iam::messaging::revoke_role_by_name_request>::write_json(
    const ores::iam::messaging::revoke_role_by_name_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::revoke_role_by_name_request
wire_traits<ores::iam::messaging::revoke_role_by_name_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::revoke_role_by_name_request>(text);
}

std::string wire_traits<ores::iam::messaging::revoke_role_by_name_request>::write_msgpack(
    const ores::iam::messaging::revoke_role_by_name_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::revoke_role_by_name_request
wire_traits<ores::iam::messaging::revoke_role_by_name_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::revoke_role_by_name_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_account_roles_request>::write_json(
    const ores::iam::messaging::get_account_roles_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_account_roles_request
wire_traits<ores::iam::messaging::get_account_roles_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_account_roles_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_account_roles_request>::write_msgpack(
    const ores::iam::messaging::get_account_roles_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_account_roles_request
wire_traits<ores::iam::messaging::get_account_roles_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_account_roles_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_my_roles_request>::write_json(
    const ores::iam::messaging::get_my_roles_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_my_roles_request
wire_traits<ores::iam::messaging::get_my_roles_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_my_roles_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_my_roles_request>::write_msgpack(
    const ores::iam::messaging::get_my_roles_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_my_roles_request
wire_traits<ores::iam::messaging::get_my_roles_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_my_roles_request>(data);
}

std::string wire_traits<ores::iam::messaging::permission_resource_row>::write_json(
    const ores::iam::messaging::permission_resource_row& v) {
    return object::write_json(v);
}

ores::iam::messaging::permission_resource_row
wire_traits<ores::iam::messaging::permission_resource_row>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::permission_resource_row>(text);
}

std::string wire_traits<ores::iam::messaging::permission_resource_row>::write_msgpack(
    const ores::iam::messaging::permission_resource_row& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::permission_resource_row
wire_traits<ores::iam::messaging::permission_resource_row>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::permission_resource_row>(data);
}

std::string wire_traits<ores::iam::messaging::permission_area_count>::write_json(
    const ores::iam::messaging::permission_area_count& v) {
    return object::write_json(v);
}

ores::iam::messaging::permission_area_count
wire_traits<ores::iam::messaging::permission_area_count>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::permission_area_count>(text);
}

std::string wire_traits<ores::iam::messaging::permission_area_count>::write_msgpack(
    const ores::iam::messaging::permission_area_count& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::permission_area_count
wire_traits<ores::iam::messaging::permission_area_count>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::permission_area_count>(data);
}

std::string wire_traits<ores::iam::messaging::list_account_permissions_request>::write_json(
    const ores::iam::messaging::list_account_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_account_permissions_request
wire_traits<ores::iam::messaging::list_account_permissions_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_account_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_account_permissions_request>::write_msgpack(
    const ores::iam::messaging::list_account_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_account_permissions_request
wire_traits<ores::iam::messaging::list_account_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_account_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::list_my_permissions_request>::write_json(
    const ores::iam::messaging::list_my_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_my_permissions_request
wire_traits<ores::iam::messaging::list_my_permissions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_my_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_my_permissions_request>::write_msgpack(
    const ores::iam::messaging::list_my_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_my_permissions_request
wire_traits<ores::iam::messaging::list_my_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_my_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::permission_page_response>::write_json(
    const ores::iam::messaging::permission_page_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::permission_page_response
wire_traits<ores::iam::messaging::permission_page_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::permission_page_response>(text);
}

std::string wire_traits<ores::iam::messaging::permission_page_response>::write_msgpack(
    const ores::iam::messaging::permission_page_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::permission_page_response
wire_traits<ores::iam::messaging::permission_page_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::permission_page_response>(data);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_page_request>::write_json(
    const ores::iam::messaging::list_role_permissions_page_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_role_permissions_page_request
wire_traits<ores::iam::messaging::list_role_permissions_page_request>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::list_role_permissions_page_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_role_permissions_page_request>::write_msgpack(
    const ores::iam::messaging::list_role_permissions_page_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_role_permissions_page_request
wire_traits<ores::iam::messaging::list_role_permissions_page_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_role_permissions_page_request>(data);
}

std::string wire_traits<ores::iam::messaging::role_page_row>::write_json(
    const ores::iam::messaging::role_page_row& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_page_row
wire_traits<ores::iam::messaging::role_page_row>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_page_row>(text);
}

std::string wire_traits<ores::iam::messaging::role_page_row>::write_msgpack(
    const ores::iam::messaging::role_page_row& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_page_row
wire_traits<ores::iam::messaging::role_page_row>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_page_row>(data);
}

std::string wire_traits<ores::iam::messaging::list_roles_page_request>::write_json(
    const ores::iam::messaging::list_roles_page_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_roles_page_request
wire_traits<ores::iam::messaging::list_roles_page_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_roles_page_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_roles_page_request>::write_msgpack(
    const ores::iam::messaging::list_roles_page_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_roles_page_request
wire_traits<ores::iam::messaging::list_roles_page_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_roles_page_request>(data);
}

std::string wire_traits<ores::iam::messaging::role_page_response>::write_json(
    const ores::iam::messaging::role_page_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_page_response
wire_traits<ores::iam::messaging::role_page_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_page_response>(text);
}

std::string wire_traits<ores::iam::messaging::role_page_response>::write_msgpack(
    const ores::iam::messaging::role_page_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_page_response
wire_traits<ores::iam::messaging::role_page_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_page_response>(data);
}

std::string wire_traits<ores::iam::messaging::role_holder>::write_json(
    const ores::iam::messaging::role_holder& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_holder
wire_traits<ores::iam::messaging::role_holder>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_holder>(text);
}

std::string wire_traits<ores::iam::messaging::role_holder>::write_msgpack(
    const ores::iam::messaging::role_holder& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_holder
wire_traits<ores::iam::messaging::role_holder>::read_msgpack(std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_holder>(data);
}

std::string wire_traits<ores::iam::messaging::list_role_holders_request>::write_json(
    const ores::iam::messaging::list_role_holders_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::list_role_holders_request
wire_traits<ores::iam::messaging::list_role_holders_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::list_role_holders_request>(text);
}

std::string wire_traits<ores::iam::messaging::list_role_holders_request>::write_msgpack(
    const ores::iam::messaging::list_role_holders_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::list_role_holders_request
wire_traits<ores::iam::messaging::list_role_holders_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::list_role_holders_request>(data);
}

std::string wire_traits<ores::iam::messaging::role_holders_response>::write_json(
    const ores::iam::messaging::role_holders_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::role_holders_response
wire_traits<ores::iam::messaging::role_holders_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::role_holders_response>(text);
}

std::string wire_traits<ores::iam::messaging::role_holders_response>::write_msgpack(
    const ores::iam::messaging::role_holders_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::role_holders_response
wire_traits<ores::iam::messaging::role_holders_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::role_holders_response>(data);
}

std::string wire_traits<ores::iam::messaging::account_role_access>::write_json(
    const ores::iam::messaging::account_role_access& v) {
    return object::write_json(v);
}

ores::iam::messaging::account_role_access
wire_traits<ores::iam::messaging::account_role_access>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::account_role_access>(text);
}

std::string wire_traits<ores::iam::messaging::account_role_access>::write_msgpack(
    const ores::iam::messaging::account_role_access& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::account_role_access
wire_traits<ores::iam::messaging::account_role_access>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::account_role_access>(data);
}

std::string wire_traits<ores::iam::messaging::get_account_roles_response>::write_json(
    const ores::iam::messaging::get_account_roles_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_account_roles_response
wire_traits<ores::iam::messaging::get_account_roles_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_account_roles_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_account_roles_response>::write_msgpack(
    const ores::iam::messaging::get_account_roles_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_account_roles_response
wire_traits<ores::iam::messaging::get_account_roles_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_account_roles_response>(data);
}

std::string wire_traits<ores::iam::messaging::get_role_permissions_request>::write_json(
    const ores::iam::messaging::get_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_role_permissions_request
wire_traits<ores::iam::messaging::get_role_permissions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::get_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::get_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_role_permissions_request
wire_traits<ores::iam::messaging::get_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::get_role_permissions_response>::write_json(
    const ores::iam::messaging::get_role_permissions_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::get_role_permissions_response
wire_traits<ores::iam::messaging::get_role_permissions_response>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::get_role_permissions_response>(text);
}

std::string wire_traits<ores::iam::messaging::get_role_permissions_response>::write_msgpack(
    const ores::iam::messaging::get_role_permissions_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::get_role_permissions_response
wire_traits<ores::iam::messaging::get_role_permissions_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::get_role_permissions_response>(data);
}

std::string wire_traits<ores::iam::messaging::put_role_permissions_request>::write_json(
    const ores::iam::messaging::put_role_permissions_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::put_role_permissions_request
wire_traits<ores::iam::messaging::put_role_permissions_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::put_role_permissions_request>(text);
}

std::string wire_traits<ores::iam::messaging::put_role_permissions_request>::write_msgpack(
    const ores::iam::messaging::put_role_permissions_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::put_role_permissions_request
wire_traits<ores::iam::messaging::put_role_permissions_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::put_role_permissions_request>(data);
}

std::string wire_traits<ores::iam::messaging::suggest_role_commands_request>::write_json(
    const ores::iam::messaging::suggest_role_commands_request& v) {
    return object::write_json(v);
}

ores::iam::messaging::suggest_role_commands_request
wire_traits<ores::iam::messaging::suggest_role_commands_request>::read_json(std::string_view text) {
    return object::read_json<ores::iam::messaging::suggest_role_commands_request>(text);
}

std::string wire_traits<ores::iam::messaging::suggest_role_commands_request>::write_msgpack(
    const ores::iam::messaging::suggest_role_commands_request& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::suggest_role_commands_request
wire_traits<ores::iam::messaging::suggest_role_commands_request>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::suggest_role_commands_request>(data);
}

std::string wire_traits<ores::iam::messaging::suggest_role_commands_response>::write_json(
    const ores::iam::messaging::suggest_role_commands_response& v) {
    return object::write_json(v);
}

ores::iam::messaging::suggest_role_commands_response
wire_traits<ores::iam::messaging::suggest_role_commands_response>::read_json(
    std::string_view text) {
    return object::read_json<ores::iam::messaging::suggest_role_commands_response>(text);
}

std::string wire_traits<ores::iam::messaging::suggest_role_commands_response>::write_msgpack(
    const ores::iam::messaging::suggest_role_commands_response& v) {
    return object::write_msgpack(v);
}

ores::iam::messaging::suggest_role_commands_response
wire_traits<ores::iam::messaging::suggest_role_commands_response>::read_msgpack(
    std::span<const std::byte> data) {
    return object::read_msgpack<ores::iam::messaging::suggest_role_commands_response>(data);
}

}
