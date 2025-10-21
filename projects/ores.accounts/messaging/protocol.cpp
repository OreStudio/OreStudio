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
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::accounts::messaging {

namespace {

/**
 * @brief Helper to write a 16-bit integer in network byte order.
 */
void write_uint16(std::vector<std::uint8_t>& buffer, std::uint16_t value) {
    buffer.push_back(static_cast<std::uint8_t>(value >> 8));
    buffer.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

/**
 * @brief Helper to write a 32-bit integer in network byte order.
 */
void write_uint32(std::vector<std::uint8_t>& buffer, std::uint32_t value) {
    buffer.push_back(static_cast<std::uint8_t>(value >> 24));
    buffer.push_back(static_cast<std::uint8_t>((value >> 16) & 0xFF));
    buffer.push_back(static_cast<std::uint8_t>((value >> 8) & 0xFF));
    buffer.push_back(static_cast<std::uint8_t>(value & 0xFF));
}

/**
 * @brief Helper to write a string with 16-bit length prefix.
 */
void write_string(std::vector<std::uint8_t>& buffer, const std::string& str) {
    auto len = static_cast<std::uint16_t>(std::min(str.size(), size_t(65535)));
    write_uint16(buffer, len);
    buffer.insert(buffer.end(), str.begin(), str.begin() + len);
}

/**
 * @brief Helper to write a UUID (16 bytes).
 */
void write_uuid(std::vector<std::uint8_t>& buffer, const boost::uuids::uuid& uuid) {
    buffer.insert(buffer.end(), uuid.begin(), uuid.end());
}

/**
 * @brief Helper to write a boolean (1 byte).
 */
void write_bool(std::vector<std::uint8_t>& buffer, bool value) {
    buffer.push_back(value ? 1 : 0);
}

/**
 * @brief Helper to read a 16-bit integer in network byte order.
 */
std::expected<std::uint16_t, comms::protocol::error_code>
read_uint16(std::span<const std::uint8_t>& data) {
    if (data.size() < 2) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::uint16_t value = (static_cast<std::uint16_t>(data[0]) << 8) |
                          static_cast<std::uint16_t>(data[1]);
    data = data.subspan(2);
    return value;
}

/**
 * @brief Helper to read a 32-bit integer in network byte order.
 */
std::expected<std::uint32_t, comms::protocol::error_code>
read_uint32(std::span<const std::uint8_t>& data) {
    if (data.size() < 4) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::uint32_t value = (static_cast<std::uint32_t>(data[0]) << 24) |
                          (static_cast<std::uint32_t>(data[1]) << 16) |
                          (static_cast<std::uint32_t>(data[2]) << 8) |
                          static_cast<std::uint32_t>(data[3]);
    data = data.subspan(4);
    return value;
}

/**
 * @brief Helper to read a string with 16-bit length prefix.
 */
std::expected<std::string, comms::protocol::error_code>
read_string(std::span<const std::uint8_t>& data) {
    auto len_result = read_uint16(data);
    if (!len_result) {
        return std::unexpected(len_result.error());
    }
    auto len = *len_result;
    if (data.size() < len) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    std::string str(reinterpret_cast<const char*>(data.data()), len);
    data = data.subspan(len);
    return str;
}

/**
 * @brief Helper to read a UUID (16 bytes).
 */
std::expected<boost::uuids::uuid, comms::protocol::error_code>
read_uuid(std::span<const std::uint8_t>& data) {
    if (data.size() < 16) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    boost::uuids::uuid uuid;
    std::copy_n(data.begin(), 16, uuid.begin());
    data = data.subspan(16);
    return uuid;
}

/**
 * @brief Helper to read a boolean (1 byte).
 */
std::expected<bool, comms::protocol::error_code>
read_bool(std::span<const std::uint8_t>& data) {
    if (data.size() < 1) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    bool value = data[0] != 0;
    data = data.subspan(1);
    return value;
}

}

// create_account_request implementation
std::vector<std::uint8_t> create_account_request::serialize() const {
    std::vector<std::uint8_t> buffer;
    write_string(buffer, username);
    write_string(buffer, password_hash);
    write_string(buffer, password_salt);
    write_string(buffer, totp_secret);
    write_string(buffer, email);
    write_bool(buffer, is_admin);
    return buffer;
}

std::expected<create_account_request, comms::protocol::error_code>
create_account_request::deserialize(std::span<const std::uint8_t> data) {
    create_account_request request;

    auto username_result = read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto password_hash_result = read_string(data);
    if (!password_hash_result) return std::unexpected(password_hash_result.error());
    request.password_hash = *password_hash_result;

    auto password_salt_result = read_string(data);
    if (!password_salt_result) return std::unexpected(password_salt_result.error());
    request.password_salt = *password_salt_result;

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

// create_account_response implementation
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

// list_accounts_request implementation
std::vector<std::uint8_t> list_accounts_request::serialize() const {
    // Empty payload - no parameters
    return {};
}

std::expected<list_accounts_request, comms::protocol::error_code>
list_accounts_request::deserialize(std::span<const std::uint8_t> data) {
    // Empty payload expected
    if (!data.empty()) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    return list_accounts_request{};
}

// list_accounts_response implementation
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

}
