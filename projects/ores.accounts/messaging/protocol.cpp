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
#include <cstring>
#include <expected>
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.utility/messaging/write.hpp"
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::accounts::messaging {

using namespace ores::utility::messaging;

// create_account_request implementation
std::vector<std::uint8_t> create_account_request::serialize() const {
    std::vector<std::uint8_t> buffer;
    write_string(buffer, username);
    write_string(buffer, password);
    write_string(buffer, totp_secret);
    write_string(buffer, email);
    write_string(buffer, modified_by);
    write_bool(buffer, is_admin);
    return buffer;
}

std::expected<create_account_request, comms::protocol::error_code>
create_account_request::deserialize(std::span<const std::uint8_t> data) {
    create_account_request request;

    auto username_result = read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto password_result = read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.password = *password_result;

    auto totp_secret_result = read_string(data);
    if (!totp_secret_result) return std::unexpected(totp_secret_result.error());
    request.totp_secret = *totp_secret_result;

    auto email_result = read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.email = *email_result;

    auto is_admin_result = read_bool(data);
    if (!is_admin_result) return std::unexpected(is_admin_result.error());
    request.is_admin = *is_admin_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const create_account_request& v)
{
    rfl::json::write(v, s);
    return(s);
}

std::vector<std::uint8_t> create_account_response::serialize() const {
    std::vector<std::uint8_t> buffer;
    write_uuid(buffer, account_id);
    return buffer;
}

std::expected<create_account_response, comms::protocol::error_code>
create_account_response::deserialize(std::span<const std::uint8_t> data) {
    create_account_response response;

    auto account_id_result = read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    response.account_id = *account_id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const create_account_response& v)
{
    rfl::json::write(v, s);
    return(s);
}


std::vector<std::uint8_t> list_accounts_request::serialize() const {
    return {};
}

std::expected<list_accounts_request, comms::protocol::error_code>
list_accounts_request::deserialize(std::span<const std::uint8_t> data) {
    if (!data.empty()) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    return list_accounts_request{};
}

std::ostream& operator<<(std::ostream& s, const list_accounts_request& v)
{
    rfl::json::write(v, s);
    return(s);
}

std::vector<std::uint8_t> list_accounts_response::serialize() const {
    std::vector<std::uint8_t> buffer;

    // Write account count
    write_uint32(buffer, static_cast<std::uint32_t>(accounts.size()));

    // Write each account
    for (const auto& account : accounts) {
        write_uint32(buffer, static_cast<std::uint32_t>(account.version));
        write_string(buffer, account.modified_by);
        write_uuid(buffer, account.id);
        write_string(buffer, account.username);
        write_string(buffer, account.password_hash);
        write_string(buffer, account.password_salt);
        write_string(buffer, account.totp_secret);
        write_string(buffer, account.email);
        write_bool(buffer, account.is_admin);
    }

    return buffer;
}

std::expected<list_accounts_response, comms::protocol::error_code>
list_accounts_response::deserialize(std::span<const std::uint8_t> data) {
    list_accounts_response response;

    // Read account count
    auto count_result = read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Read each account
    response.accounts.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::account account;

        auto version_result = read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        account.version = static_cast<int>(*version_result);

        auto modified_by_result = read_string(data);
        if (!modified_by_result) return std::unexpected(modified_by_result.error());
        account.modified_by = *modified_by_result;

        auto id_result = read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        account.id = *id_result;

        auto username_result = read_string(data);
        if (!username_result) return std::unexpected(username_result.error());
        account.username = *username_result;

        auto password_hash_result = read_string(data);
        if (!password_hash_result) return std::unexpected(password_hash_result.error());
        account.password_hash = *password_hash_result;

        auto password_salt_result = read_string(data);
        if (!password_salt_result) return std::unexpected(password_salt_result.error());
        account.password_salt = *password_salt_result;

        auto totp_secret_result = read_string(data);
        if (!totp_secret_result) return std::unexpected(totp_secret_result.error());
        account.totp_secret = *totp_secret_result;

        auto email_result = read_string(data);
        if (!email_result) return std::unexpected(email_result.error());
        account.email = *email_result;

        auto is_admin_result = read_bool(data);
        if (!is_admin_result) return std::unexpected(is_admin_result.error());
        account.is_admin = *is_admin_result;

        response.accounts.push_back(std::move(account));
    }

    return response;
}
std::ostream& operator<<(std::ostream& s, const list_accounts_response& v)
{
    rfl::json::write(v, s);
    return(s);
}

}
