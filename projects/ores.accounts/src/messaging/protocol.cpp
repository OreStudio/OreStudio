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
#include "ores.accounts/messaging/protocol.hpp"

#include <expected>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/protocol/reader.hpp"
#include "ores.comms/protocol/writer.hpp"

namespace ores::accounts::messaging {

using namespace ores::comms::protocol;

std::vector<std::byte> create_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    writer::write_string(buffer, password);
    writer::write_string(buffer, totp_secret);
    writer::write_string(buffer, email);
    writer::write_string(buffer, modified_by);
    writer::write_bool(buffer, is_admin);
    return buffer;
}

std::expected<create_account_request, comms::protocol::error_code>
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

std::expected<create_account_response, comms::protocol::error_code>
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

std::expected<list_accounts_request, comms::protocol::error_code>
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
        writer::write_string(buffer, account.modified_by);
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

std::expected<list_accounts_response, comms::protocol::error_code>
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

        auto modified_by_result = reader::read_string(data);
        if (!modified_by_result) return std::unexpected(modified_by_result.error());
        account.modified_by = *modified_by_result;

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

std::vector<std::byte> login_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    writer::write_string(buffer, password);
    return buffer;
}

std::expected<login_request, comms::protocol::error_code>
login_request::deserialize(std::span<const std::byte> data) {
    login_request request;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    request.username = *username_result;

    auto password_result = reader::read_string(data);
    if (!password_result) return std::unexpected(password_result.error());
    request.password = *password_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const login_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> login_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    writer::write_uuid(buffer, account_id);
    writer::write_string(buffer, username);
    writer::write_bool(buffer, is_admin);
    return buffer;
}

std::expected<login_response, comms::protocol::error_code>
login_response::deserialize(std::span<const std::byte> data) {
    login_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    response.account_id = *account_id_result;

    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    response.username = *username_result;

    auto is_admin_result = reader::read_bool(data);
    if (!is_admin_result) return std::unexpected(is_admin_result.error());
    response.is_admin = *is_admin_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const login_response& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> unlock_account_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    return buffer;
}

std::expected<unlock_account_request, comms::protocol::error_code>
unlock_account_request::deserialize(std::span<const std::byte> data) {
    unlock_account_request request;

    auto account_id_result = reader::read_uuid(data);
    if (!account_id_result) return std::unexpected(account_id_result.error());
    request.account_id = *account_id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const unlock_account_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> unlock_account_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    return buffer;
}

std::expected<unlock_account_response, comms::protocol::error_code>
unlock_account_response::deserialize(std::span<const std::byte> data) {
    unlock_account_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

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

std::expected<delete_account_request, comms::protocol::error_code>
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

std::expected<delete_account_response, comms::protocol::error_code>
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

std::vector<std::byte> list_login_info_request::serialize() const {
    return {};
}

std::expected<list_login_info_request, comms::protocol::error_code>
list_login_info_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    return list_login_info_request{};
}

std::ostream& operator<<(std::ostream& s, const list_login_info_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_login_info_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, static_cast<std::uint32_t>(login_infos.size()));

    for (const auto& li : login_infos) {
        auto epoch = std::chrono::duration_cast<std::chrono::milliseconds>(
            li.last_login.time_since_epoch()).count();
        writer::write_uint32(buffer, static_cast<std::uint32_t>(epoch >> 32));
        writer::write_uint32(buffer, static_cast<std::uint32_t>(epoch & 0xFFFFFFFF));
        writer::write_uuid(buffer, li.account_id);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(li.failed_logins));
        writer::write_bool(buffer, li.locked);
        writer::write_bool(buffer, li.online);
        writer::write_string(buffer, li.last_ip.to_string());
        writer::write_string(buffer, li.last_attempt_ip.to_string());
    }

    return buffer;
}

std::expected<list_login_info_response, comms::protocol::error_code>
list_login_info_response::deserialize(std::span<const std::byte> data) {
    list_login_info_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.login_infos.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::login_info li;

        auto high_result = reader::read_uint32(data);
        if (!high_result) return std::unexpected(high_result.error());
        auto low_result = reader::read_uint32(data);
        if (!low_result) return std::unexpected(low_result.error());
        std::uint64_t epoch = (static_cast<std::uint64_t>(*high_result) << 32) |
                              static_cast<std::uint64_t>(*low_result);
        li.last_login = std::chrono::system_clock::time_point(
            std::chrono::milliseconds(epoch));

        auto account_id_result = reader::read_uuid(data);
        if (!account_id_result) return std::unexpected(account_id_result.error());
        li.account_id = *account_id_result;

        auto failed_logins_result = reader::read_uint32(data);
        if (!failed_logins_result) return std::unexpected(failed_logins_result.error());
        li.failed_logins = static_cast<int>(*failed_logins_result);

        auto locked_result = reader::read_bool(data);
        if (!locked_result) return std::unexpected(locked_result.error());
        li.locked = *locked_result;

        auto online_result = reader::read_bool(data);
        if (!online_result) return std::unexpected(online_result.error());
        li.online = *online_result;

        auto last_ip_str_result = reader::read_string(data);
        if (!last_ip_str_result) return std::unexpected(last_ip_str_result.error());
        li.last_ip = boost::asio::ip::make_address(*last_ip_str_result);

        auto last_attempt_ip_str_result = reader::read_string(data);
        if (!last_attempt_ip_str_result) return std::unexpected(last_attempt_ip_str_result.error());
        li.last_attempt_ip = boost::asio::ip::make_address(*last_attempt_ip_str_result);

        response.login_infos.push_back(std::move(li));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_login_info_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_feature_flags_request::serialize() const {
    return {};
}

std::expected<list_feature_flags_request, comms::protocol::error_code>
list_feature_flags_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(comms::protocol::error_code::payload_too_large);
    }
    return list_feature_flags_request{};
}

std::ostream& operator<<(std::ostream& s, const list_feature_flags_request& v)
{
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> list_feature_flags_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_uint32(buffer, static_cast<std::uint32_t>(feature_flags.size()));

    for (const auto& ff : feature_flags) {
        writer::write_string(buffer, ff.name);
        writer::write_bool(buffer, ff.enabled);
        writer::write_string(buffer, ff.description);
        writer::write_string(buffer, ff.modified_by);
    }

    return buffer;
}

std::expected<list_feature_flags_response, comms::protocol::error_code>
list_feature_flags_response::deserialize(std::span<const std::byte> data) {
    list_feature_flags_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    response.feature_flags.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::feature_flags ff;

        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        ff.name = *name_result;

        auto enabled_result = reader::read_bool(data);
        if (!enabled_result) return std::unexpected(enabled_result.error());
        ff.enabled = *enabled_result;

        auto description_result = reader::read_string(data);
        if (!description_result) return std::unexpected(description_result.error());
        ff.description = *description_result;

        auto modified_by_result = reader::read_string(data);
        if (!modified_by_result) return std::unexpected(modified_by_result.error());
        ff.modified_by = *modified_by_result;

        response.feature_flags.push_back(std::move(ff));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_feature_flags_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
