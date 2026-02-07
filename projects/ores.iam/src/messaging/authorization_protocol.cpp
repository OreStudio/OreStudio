/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/messaging/authorization_protocol.hpp"

#include <chrono>
#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::iam::messaging {

using ores::utility::serialization::reader;
using ores::utility::serialization::writer;
using ores::utility::serialization::error_code;

// ============================================================================
// Helper functions for serializing roles
// ============================================================================

namespace {

void write_timepoint(std::vector<std::byte>& buffer,
                     std::chrono::system_clock::time_point tp) {
    auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(
        tp.time_since_epoch()).count();
    writer::write_int64(buffer, nanos);
}

std::expected<std::chrono::system_clock::time_point, ores::utility::serialization::error_code>
read_timepoint(std::span<const std::byte>& data) {
    auto nanos = reader::read_int64(data);
    if (!nanos) return std::unexpected(nanos.error());
    return std::chrono::system_clock::time_point{
        std::chrono::duration_cast<std::chrono::system_clock::duration>(
            std::chrono::nanoseconds{*nanos})};
}

void serialize_role(std::vector<std::byte>& buffer, const domain::role& role) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(role.version));
    writer::write_uuid(buffer, role.id);
    writer::write_string(buffer, role.name);
    writer::write_string(buffer, role.description);
    writer::write_string(buffer, role.recorded_by);
    writer::write_string(buffer, role.change_reason_code);
    writer::write_string(buffer, role.change_commentary);
    write_timepoint(buffer, role.recorded_at);
    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(role.permission_codes.size()));
    for (const auto& code : role.permission_codes) {
        writer::write_string(buffer, code);
    }
}

std::expected<domain::role, ores::utility::serialization::error_code>
deserialize_role(std::span<const std::byte>& data) {
    domain::role role;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    role.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    role.id = *id_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    role.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    role.description = *description_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    role.recorded_by = *recorded_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    role.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    role.change_commentary = *change_commentary_result;

    auto recorded_at_result = read_timepoint(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    role.recorded_at = *recorded_at_result;

    auto perm_count_result = reader::read_uint32(data);
    if (!perm_count_result) return std::unexpected(perm_count_result.error());
    const auto perm_count = *perm_count_result;

    role.permission_codes.reserve(perm_count);
    for (std::uint32_t i = 0; i < perm_count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        role.permission_codes.push_back(*code_result);
    }

    return role;
}

}

// ============================================================================
// List Roles
// ============================================================================

std::vector<std::byte> list_roles_request::serialize() const {
    return {}; // Empty payload
}

std::expected<list_roles_request, ores::utility::serialization::error_code>
list_roles_request::deserialize(std::span<const std::byte> /*data*/) {
    return list_roles_request{};
}

std::ostream& operator<<(std::ostream& s, const list_roles_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_roles_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(roles.size()));
    for (const auto& role : roles) {
        serialize_role(buffer, role);
    }
    return buffer;
}

std::expected<list_roles_response, ores::utility::serialization::error_code>
list_roles_response::deserialize(std::span<const std::byte> data) {
    list_roles_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.roles.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto role_result = deserialize_role(data);
        if (!role_result) return std::unexpected(role_result.error());
        response.roles.push_back(std::move(*role_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_roles_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// List Permissions
// ============================================================================

std::vector<std::byte> list_permissions_request::serialize() const {
    return {}; // Empty payload
}

std::expected<list_permissions_request, ores::utility::serialization::error_code>
list_permissions_request::deserialize(std::span<const std::byte> /*data*/) {
    return list_permissions_request{};
}

std::ostream& operator<<(std::ostream& s, const list_permissions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_permissions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(permissions.size()));
    for (const auto& perm : permissions) {
        writer::write_uuid(buffer, perm.id);
        writer::write_string(buffer, perm.code);
        writer::write_string(buffer, perm.description);
    }
    return buffer;
}

std::expected<list_permissions_response, ores::utility::serialization::error_code>
list_permissions_response::deserialize(std::span<const std::byte> data) {
    list_permissions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.permissions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::permission perm;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        perm.id = *id_result;

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        perm.code = *code_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        perm.description = *description_result;

        response.permissions.push_back(std::move(perm));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_permissions_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Assign Role
// ============================================================================

std::vector<std::byte> assign_role_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    writer::write_uuid(buffer, role_id);
    return buffer;
}

std::expected<assign_role_request, ores::utility::serialization::error_code>
assign_role_request::deserialize(std::span<const std::byte> data) {
    assign_role_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    auto role_id_result = reader::read_uuid(data);
    if (!role_id_result) return std::unexpected(role_id_result.error());
    request.role_id = *role_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const assign_role_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> assign_role_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    return buffer;
}

std::expected<assign_role_response, ores::utility::serialization::error_code>
assign_role_response::deserialize(std::span<const std::byte> data) {
    assign_role_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const assign_role_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Revoke Role
// ============================================================================

std::vector<std::byte> revoke_role_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    writer::write_uuid(buffer, role_id);
    return buffer;
}

std::expected<revoke_role_request, ores::utility::serialization::error_code>
revoke_role_request::deserialize(std::span<const std::byte> data) {
    revoke_role_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    auto role_id_result = reader::read_uuid(data);
    if (!role_id_result) return std::unexpected(role_id_result.error());
    request.role_id = *role_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const revoke_role_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> revoke_role_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    return buffer;
}

std::expected<revoke_role_response, ores::utility::serialization::error_code>
revoke_role_response::deserialize(std::span<const std::byte> data) {
    revoke_role_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const revoke_role_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Get Account Roles
// ============================================================================

std::vector<std::byte> get_account_roles_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<get_account_roles_request, ores::utility::serialization::error_code>
get_account_roles_request::deserialize(std::span<const std::byte> data) {
    get_account_roles_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_account_roles_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_roles_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(roles.size()));
    for (const auto& role : roles) {
        serialize_role(buffer, role);
    }
    return buffer;
}

std::expected<get_account_roles_response, ores::utility::serialization::error_code>
get_account_roles_response::deserialize(std::span<const std::byte> data) {
    get_account_roles_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.roles.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto role_result = deserialize_role(data);
        if (!role_result) return std::unexpected(role_result.error());
        response.roles.push_back(std::move(*role_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_account_roles_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Get Account Permissions
// ============================================================================

std::vector<std::byte> get_account_permissions_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<get_account_permissions_request, ores::utility::serialization::error_code>
get_account_permissions_request::deserialize(std::span<const std::byte> data) {
    get_account_permissions_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_account_permissions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_permissions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer,
        static_cast<std::uint32_t>(permission_codes.size()));
    for (const auto& code : permission_codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<get_account_permissions_response, ores::utility::serialization::error_code>
get_account_permissions_response::deserialize(std::span<const std::byte> data) {
    get_account_permissions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.permission_codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        response.permission_codes.push_back(*code_result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_account_permissions_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Get Role
// ============================================================================

std::vector<std::byte> get_role_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, identifier);
    return buffer;
}

std::expected<get_role_request, ores::utility::serialization::error_code>
get_role_request::deserialize(std::span<const std::byte> data) {
    get_role_request request;

    auto identifier_result = reader::read_string(data);
    if (!identifier_result) return std::unexpected(identifier_result.error());
    request.identifier = *identifier_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_role_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_role_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, found);
    if (found && role) {
        serialize_role(buffer, *role);
    } else {
        writer::write_string(buffer, error_message);
    }
    return buffer;
}

std::expected<get_role_response, ores::utility::serialization::error_code>
get_role_response::deserialize(std::span<const std::byte> data) {
    get_role_response response;

    auto found_result = reader::read_bool(data);
    if (!found_result) return std::unexpected(found_result.error());
    response.found = *found_result;

    if (response.found) {
        auto role_result = deserialize_role(data);
        if (!role_result) return std::unexpected(role_result.error());
        response.role = std::move(*role_result);
    } else {
        auto error_message_result = reader::read_string(data);
        if (!error_message_result) return std::unexpected(error_message_result.error());
        response.error_message = *error_message_result;
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_role_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Suggest Role Commands
// ============================================================================

std::vector<std::byte> suggest_role_commands_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    writer::write_string(buffer, hostname);
    writer::write_string(buffer, tenant_id);
    return buffer;
}

std::expected<suggest_role_commands_request, ores::utility::serialization::error_code>
suggest_role_commands_request::deserialize(std::span<const std::byte> data) {
    suggest_role_commands_request request;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto hostname_result = reader::read_string(data);
    if (!hostname_result) return std::unexpected(hostname_result.error());
    request.hostname = *hostname_result;

    auto tenant_id_result = reader::read_string(data);
    if (!tenant_id_result) return std::unexpected(tenant_id_result.error());
    request.tenant_id = *tenant_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const suggest_role_commands_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> suggest_role_commands_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(commands.size()));
    for (const auto& command : commands) {
        writer::write_string(buffer, command);
    }
    return buffer;
}

std::expected<suggest_role_commands_response, ores::utility::serialization::error_code>
suggest_role_commands_response::deserialize(std::span<const std::byte> data) {
    suggest_role_commands_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.commands.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto command_result = reader::read_string(data);
        if (!command_result) return std::unexpected(command_result.error());
        response.commands.push_back(*command_result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const suggest_role_commands_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Role By Name Requests (Assign / Revoke)
// ============================================================================

template<typename Tag>
std::vector<std::byte> role_by_name_request_base<Tag>::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, principal);
    writer::write_string(buffer, role_name);
    return buffer;
}

template<typename Tag>
std::expected<role_by_name_request_base<Tag>, ores::utility::serialization::error_code>
role_by_name_request_base<Tag>::deserialize(std::span<const std::byte> data) {
    role_by_name_request_base request;

    auto principal_result = reader::read_string(data);
    if (!principal_result) return std::unexpected(principal_result.error());
    request.principal = *principal_result;

    auto role_name_result = reader::read_string(data);
    if (!role_name_result) return std::unexpected(role_name_result.error());
    request.role_name = *role_name_result;

    return request;
}

template<typename Tag>
std::ostream& operator<<(std::ostream& s, const role_by_name_request_base<Tag>& v) {
    rfl::json::write(v, s);
    return s;
}

template struct role_by_name_request_base<assign_role_by_name_tag>;
template struct role_by_name_request_base<revoke_role_by_name_tag>;
template std::ostream& operator<<(std::ostream&, const assign_role_by_name_request&);
template std::ostream& operator<<(std::ostream&, const revoke_role_by_name_request&);

}
