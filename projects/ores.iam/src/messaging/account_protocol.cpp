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
#include "ores.iam/messaging/account_protocol.hpp"

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

}

std::vector<std::byte> save_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    writer::write_string(buffer, principal);
    writer::write_string(buffer, password);
    writer::write_string(buffer, totp_secret);
    writer::write_string(buffer, email);
    writer::write_string(buffer, change_reason_code);
    writer::write_string(buffer, change_commentary);
    return buffer;
}

std::expected<save_account_request, ores::utility::serialization::error_code>
save_account_request::deserialize(std::span<const std::byte> data) {
    save_account_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    auto principal_result = reader::read_string(data);
    if (!principal_result) return std::unexpected(principal_result.error());
    request.principal = *principal_result;

    auto password_result = reader::read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.password = *password_result;

    auto totp_secret_result = reader::read_string(data);
    if (!totp_secret_result) return std::unexpected(totp_secret_result.error());
    request.totp_secret = *totp_secret_result;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.email = *email_result;


    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    request.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    request.change_commentary = *change_commentary_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<save_account_response, ores::utility::serialization::error_code>
save_account_response::deserialize(std::span<const std::byte> data) {
    save_account_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    response.account_id = *account_id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_account_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_accounts_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_accounts_request, ores::utility::serialization::error_code>
get_accounts_request::deserialize(std::span<const std::byte> data) {
    get_accounts_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_accounts_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_accounts_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write account count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(accounts.size()));

    // Write each account
    for (const auto& account : accounts) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(account.version));
        write_timepoint(buffer, account.recorded_at);
        writer::write_uuid(buffer, account.id);
        writer::write_string(buffer, account.username);
        writer::write_string(buffer, account.password_hash);
        writer::write_string(buffer, account.password_salt);
        writer::write_string(buffer, account.totp_secret);
        writer::write_string(buffer, account.email);
        writer::write_string(buffer, account.change_reason_code);
        writer::write_string(buffer, account.change_commentary);
        writer::write_string(buffer, account.account_type);
        writer::write_string(buffer, account.modified_by);
        writer::write_string(buffer, account.performed_by);
    }

    return buffer;
}

std::expected<get_accounts_response, ores::utility::serialization::error_code>
get_accounts_response::deserialize(std::span<const std::byte> data) {
    get_accounts_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) {
        return std::unexpected(total_result.error());
    }
    response.total_available_count = *total_result;

    // Read account count in this response
    auto count_result = reader::read_count(data);
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


        auto recorded_at_result = read_timepoint(data);
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

        auto change_reason_code_result = reader::read_string(data);
        if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
        account.change_reason_code = *change_reason_code_result;

        auto change_commentary_result = reader::read_string(data);
        if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
        account.change_commentary = *change_commentary_result;

        auto account_type_result = reader::read_string(data);
        if (!account_type_result) return std::unexpected(account_type_result.error());
        account.account_type = *account_type_result;

        auto modified_by_result = reader::read_string(data);
        if (!modified_by_result) return std::unexpected(modified_by_result.error());
        account.modified_by = *modified_by_result;

        auto performed_by_result = reader::read_string(data);
        if (!performed_by_result) return std::unexpected(performed_by_result.error());
        account.performed_by = *performed_by_result;

        response.accounts.push_back(std::move(account));
    }

    return response;
}
std::ostream& operator<<(std::ostream& s, const get_accounts_response& v)
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

std::expected<unlock_account_request, ores::utility::serialization::error_code>
unlock_account_request::deserialize(std::span<const std::byte> data) {
    unlock_account_request request;

    auto count_result = reader::read_count(data);
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

std::expected<unlock_account_response, ores::utility::serialization::error_code>
unlock_account_response::deserialize(std::span<const std::byte> data) {
    unlock_account_response response;

    auto count_result = reader::read_count(data);
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

std::expected<delete_account_request, ores::utility::serialization::error_code>
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

std::expected<delete_account_response, ores::utility::serialization::error_code>
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

std::expected<lock_account_request, ores::utility::serialization::error_code>
lock_account_request::deserialize(std::span<const std::byte> data) {
    lock_account_request request;

    auto count_result = reader::read_count(data);
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

std::expected<lock_account_response, ores::utility::serialization::error_code>
lock_account_response::deserialize(std::span<const std::byte> data) {
    lock_account_response response;

    auto count_result = reader::read_count(data);
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

std::vector<std::byte> reset_password_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(account_ids.size()));
    for (const auto& id : account_ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<reset_password_request, ores::utility::serialization::error_code>
reset_password_request::deserialize(std::span<const std::byte> data) {
    reset_password_request request;

    auto count_result = reader::read_count(data);
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

std::ostream& operator<<(std::ostream& s, const reset_password_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const reset_password_result& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> reset_password_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& result : results) {
        writer::write_uuid(buffer, result.account_id);
        writer::write_bool(buffer, result.success);
        writer::write_string(buffer, result.message);
    }
    return buffer;
}

std::expected<reset_password_response, ores::utility::serialization::error_code>
reset_password_response::deserialize(std::span<const std::byte> data) {
    reset_password_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        reset_password_result result;

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

std::ostream& operator<<(std::ostream& s, const reset_password_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> change_password_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, new_password);
    return buffer;
}

std::expected<change_password_request, ores::utility::serialization::error_code>
change_password_request::deserialize(std::span<const std::byte> data) {
    change_password_request request;

    auto password_result = reader::read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.new_password = *password_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const change_password_request&)
{
    // Don't log passwords - write placeholder
    s << "{\"new_password\":\"[REDACTED]\"}";
    return s;
}

std::vector<std::byte> change_password_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<change_password_response, ores::utility::serialization::error_code>
change_password_response::deserialize(std::span<const std::byte> data) {
    change_password_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const change_password_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> update_my_email_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, new_email);
    return buffer;
}

std::expected<update_my_email_request, ores::utility::serialization::error_code>
update_my_email_request::deserialize(std::span<const std::byte> data) {
    update_my_email_request request;

    auto email_result = reader::read_string(data);
    if (!email_result) return std::unexpected(email_result.error());
    request.new_email = *email_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const update_my_email_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> update_my_email_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<update_my_email_response, ores::utility::serialization::error_code>
update_my_email_response::deserialize(std::span<const std::byte> data) {
    update_my_email_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const update_my_email_response& v)
{
    rfl::json::write(v, s);
    return s;
}

}
