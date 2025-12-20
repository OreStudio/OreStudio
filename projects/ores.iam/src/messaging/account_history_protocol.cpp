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
#include "ores.iam/messaging/account_history_protocol.hpp"

#include <chrono>
#include <expected>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

using namespace ores::iam;
using namespace ores::comms::messaging;

namespace {

void write_timepoint(std::vector<std::byte>& buffer,
                     std::chrono::system_clock::time_point tp) {
    auto nanos = std::chrono::duration_cast<std::chrono::nanoseconds>(
        tp.time_since_epoch()).count();
    writer::write_int64(buffer, nanos);
}

std::expected<std::chrono::system_clock::time_point, error_code>
read_timepoint(std::span<const std::byte>& data) {
    auto nanos = reader::read_int64(data);
    if (!nanos) return std::unexpected(nanos.error());
    return std::chrono::system_clock::time_point{
        std::chrono::nanoseconds{*nanos}};
}

/**
 * @brief Helper function to serialize a single account version
 */
void serialize_account_version(std::vector<std::byte>& buffer,
                               const domain::account_version& version) {
    // Write account data
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.data.version));
    writer::write_string(buffer, version.data.recorded_by);
    write_timepoint(buffer, version.data.recorded_at);
    writer::write_uuid(buffer, version.data.id);
    writer::write_string(buffer, version.data.username);
    writer::write_string(buffer, version.data.password_hash);
    writer::write_string(buffer, version.data.password_salt);
    writer::write_string(buffer, version.data.totp_secret);
    writer::write_string(buffer, version.data.email);
    writer::write_bool(buffer, version.data.is_admin);

    // Write version metadata
    writer::write_uint32(buffer, static_cast<std::uint32_t>(version.version_number));
    writer::write_string(buffer, version.recorded_by);
    write_timepoint(buffer, version.recorded_at);
    writer::write_string(buffer, version.change_summary);
}

std::expected<domain::account_version, error_code>
deserialize_account_version(std::span<const std::byte>& data) {
    domain::account_version version;

    // Read account data
    auto acc_version = reader::read_uint32(data);
    if (!acc_version) return std::unexpected(acc_version.error());
    version.data.version = static_cast<int>(*acc_version);

    auto recorded_by = reader::read_string(data);
    if (!recorded_by) return std::unexpected(recorded_by.error());
    version.data.recorded_by = *recorded_by;

    auto recorded_at = read_timepoint(data);
    if (!recorded_at) return std::unexpected(recorded_at.error());
    version.data.recorded_at = *recorded_at;

    auto id = reader::read_uuid(data);
    if (!id) return std::unexpected(id.error());
    version.data.id = *id;

    auto username = reader::read_string(data);
    if (!username) return std::unexpected(username.error());
    version.data.username = *username;

    auto password_hash = reader::read_string(data);
    if (!password_hash) return std::unexpected(password_hash.error());
    version.data.password_hash = *password_hash;

    auto password_salt = reader::read_string(data);
    if (!password_salt) return std::unexpected(password_salt.error());
    version.data.password_salt = *password_salt;

    auto totp_secret = reader::read_string(data);
    if (!totp_secret) return std::unexpected(totp_secret.error());
    version.data.totp_secret = *totp_secret;

    auto email = reader::read_string(data);
    if (!email) return std::unexpected(email.error());
    version.data.email = *email;

    auto is_admin = reader::read_bool(data);
    if (!is_admin) return std::unexpected(is_admin.error());
    version.data.is_admin = *is_admin;

    // Read version metadata
    auto version_number = reader::read_uint32(data);
    if (!version_number) return std::unexpected(version_number.error());
    version.version_number = static_cast<int>(*version_number);

    auto version_recorded_by = reader::read_string(data);
    if (!version_recorded_by) return std::unexpected(version_recorded_by.error());
    version.recorded_by = *version_recorded_by;

    auto version_recorded_at = read_timepoint(data);
    if (!version_recorded_at) return std::unexpected(version_recorded_at.error());
    version.recorded_at = *version_recorded_at;

    auto change_summary = reader::read_string(data);
    if (!change_summary) return std::unexpected(change_summary.error());
    version.change_summary = *change_summary;

    return version;
}

}

namespace ores::iam::messaging {

std::vector<std::byte> get_account_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, username);
    return buffer;
}

std::expected<get_account_history_request, comms::messaging::error_code>
get_account_history_request::deserialize(std::span<const std::byte> data) {
    auto username_result = reader::read_string(data);
    if (!username_result) {
        return std::unexpected(username_result.error());
    }
    return get_account_history_request{*username_result};
}

std::ostream& operator<<(std::ostream& s, const get_account_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_account_history_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write success flag and message
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);

    // Write username
    writer::write_string(buffer, history.username);

    // Write version count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(history.versions.size()));

    // Write each version
    for (const auto& version : history.versions) {
        serialize_account_version(buffer, version);
    }

    return buffer;
}

std::expected<get_account_history_response, comms::messaging::error_code>
get_account_history_response::deserialize(std::span<const std::byte> data) {
    get_account_history_response response;

    // Read success flag and message
    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    // If request failed, don't try to read history data
    if (!response.success) {
        return response;
    }

    // Read username
    auto username_result = reader::read_string(data);
    if (!username_result) return std::unexpected(username_result.error());
    response.history.username = *username_result;

    // Read version count
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    // Read each version
    response.history.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto version_result = deserialize_account_version(data);
        if (!version_result) return std::unexpected(version_result.error());
        response.history.versions.push_back(std::move(*version_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_account_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
