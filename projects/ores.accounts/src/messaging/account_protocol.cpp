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
#include "ores.accounts/messaging/account_protocol.hpp"

#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::accounts::messaging {

using namespace ores::comms::messaging;

std::vector<std::byte> create_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    writer::write_string(buffer, password);
    writer::write_string(buffer, totp_secret);
    writer::write_string(buffer, email);
    writer::write_string(buffer, recorded_by);
    writer::write_bool(buffer, is_admin);
    return buffer;
}

std::expected<create_account_request, comms::messaging::error_code>
create_account_request::deserialize(std::span<const std::byte> data) {
    create_account_request request;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto password_result = reader::read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.password = *password_result;

    auto totp_secret_result = reader::read_string(data);
    if (!totp_secret_result) return std::unexpected(totp_secret_result.error());
    request.totp_secret = *totp_secret_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.email = *email_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    request.recorded_by = *recorded_by_result;

    auto is_admin_result = reader::read_bool(data);
    if (!is_admin_result) return std::unexpected(is_admin_result.error());
    request.is_admin = *is_admin_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const create_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> create_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<create_account_response, comms::messaging::error_code>
create_account_response::deserialize(std::span<const std::byte> data) {
    create_account_response response;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    response.account_id = *account_id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const create_account_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_accounts_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<list_accounts_request, comms::messaging::error_code>
list_accounts_request::deserialize(std::span<const std::byte> data) {
    list_accounts_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const list_accounts_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_accounts_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write account count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(accounts.size()));

    // Write each account
    for (const auto& account : accounts) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(account.version));
        writer::write_string(buffer, account.recorded_by);
        writer::write_string(buffer, account.recorded_at);
        writer::write_uuid(buffer, account.id);
        writer::write_string(buffer, account.username);
        writer::write_string(buffer, account.password_hash);
        writer::write_string(buffer, account.password_salt);
        writer::write_string(buffer, account.totp_secret);
        writer::write_string(buffer, account.email);
        writer::write_bool(buffer, account.is_admin);
    }

    return buffer;
}

std::expected<list_accounts_response, comms::messaging::error_code>
list_accounts_response::deserialize(std::span<const std::byte> data) {
    list_accounts_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) {
        return std::unexpected(total_result.error());
    }
    response.total_available_count = *total_result;

    // Read account count in this response
    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    // Read each account
    response.accounts.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::account account;

        auto version_result = reader::read_uint32(data);
        if (!version_result) return std::unexpected(version_result.error());
        account.version = static_cast<int>(*version_result);

        auto recorded_by_result = reader::read_string(data);
        if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
        account.recorded_by = *recorded_by_result;

        auto recorded_at_result = reader::read_string(data);
        if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
        account.recorded_at = *recorded_at_result;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        account.id = *id_result;

        auto username_result = reader::read_string(data);
        if (!username_result) return std::unexpected(username_result.error());
        account.username = *username_result;

        auto password_hash_result = reader::read_string(data);
        if (!password_hash_result) return std::unexpected(password_hash_result.error());
        account.password_hash = *password_hash_result;

        auto password_salt_result = reader::read_string(data);
        if (!password_salt_result) return std::unexpected(password_salt_result.error());
        account.password_salt = *password_salt_result;

        auto totp_secret_result = reader::read_string(data);
        if (!totp_secret_result) return std::unexpected(totp_secret_result.error());
        account.totp_secret = *totp_secret_result;

        auto email_result = reader::read_string(data);
        if (!email_result) return std::unexpected(email_result.error());
        account.email = *email_result;

        auto is_admin_result = reader::read_bool(data);
        if (!is_admin_result) return std::unexpected(is_admin_result.error());
        account.is_admin = *is_admin_result;

        response.accounts.push_back(std::move(account));
    }

    return response;
}
std::ostream& operator<<(std::ostream& s, const list_accounts_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> unlock_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(account_ids.size()));
    for (const auto& id : account_ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<unlock_account_request, comms::messaging::error_code>
unlock_account_request::deserialize(std::span<const std::byte> data) {
    unlock_account_request request;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    request.account_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.account_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const unlock_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const unlock_account_result& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> unlock_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& result : results) {
        writer::write_uuid(buffer, result.account_id);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }
    return buffer;
}

std::expected<unlock_account_response, comms::messaging::error_code>
unlock_account_response::deserialize(std::span<const std::byte> data) {
    unlock_account_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        unlock_account_result result;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        result.account_id = *id_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) return std::unexpected(success_result.error());
        result.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) return std::unexpected(message_result.error());
        result.message = *message_result;

        response.results.push_back(std::move(result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const unlock_account_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<delete_account_request, comms::messaging::error_code>
delete_account_request::deserialize(std::span<const std::byte> data) {
    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) {
        return std::unexpected(account_id_result.error());
    }
    return delete_account_request{*account_id_result};
}

std::ostream& operator<<(std::ostream& s, const delete_account_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_account_response, comms::messaging::error_code>
delete_account_response::deserialize(std::span<const std::byte> data) {
    delete_account_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) {
        return std::unexpected(success_result.error());
    }
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) {
        return std::unexpected(message_result.error());
    }
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_account_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> lock_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(account_ids.size()));
    for (const auto& id : account_ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<lock_account_request, comms::messaging::error_code>
lock_account_request::deserialize(std::span<const std::byte> data) {
    lock_account_request request;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    request.account_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.account_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const lock_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const lock_account_result& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> lock_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& result : results) {
        writer::write_uuid(buffer, result.account_id);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }
    return buffer;
}

std::expected<lock_account_response, comms::messaging::error_code>
lock_account_response::deserialize(std::span<const std::byte> data) {
    lock_account_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        lock_account_result result;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        result.account_id = *id_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) return std::unexpected(success_result.error());
        result.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) return std::unexpected(message_result.error());
        result.message = *message_result;

        response.results.push_back(std::move(result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const lock_account_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> update_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    writer::write_string(buffer, email);
    writer::write_string(buffer, recorded_by);
    writer::write_bool(buffer, is_admin);
    return buffer;
}

std::expected<update_account_request, comms::messaging::error_code>
update_account_request::deserialize(std::span<const std::byte> data) {
    update_account_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.email = *email_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    request.recorded_by = *recorded_by_result;

    auto is_admin_result = reader::read_bool(data);
    if (!is_admin_result) return std::unexpected(is_admin_result.error());
    request.is_admin = *is_admin_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const update_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> update_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    return buffer;
}

std::expected<update_account_response, comms::messaging::error_code>
update_account_response::deserialize(std::span<const std::byte> data) {
    update_account_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const update_account_response& v)
{
    rfl::json::write(v, s);
    return s;
}

}
