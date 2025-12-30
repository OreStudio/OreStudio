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
#include "ores.iam/messaging/session_protocol.hpp"

#include <expected>
#include <stdexcept>
#include <boost/system/system_error.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::iam::messaging {

using namespace ores::comms::messaging;

namespace {

/**
 * @brief Write a timestamp to the buffer as uint64 milliseconds since epoch.
 */
void write_timestamp(std::vector<std::byte>& buffer,
    const std::chrono::system_clock::time_point& tp) {
    auto epoch = std::chrono::duration_cast<std::chrono::milliseconds>(
        tp.time_since_epoch()).count();
    writer::write_uint32(buffer, static_cast<std::uint32_t>(epoch >> 32));
    writer::write_uint32(buffer, static_cast<std::uint32_t>(epoch & 0xFFFFFFFF));
}

/**
 * @brief Read a timestamp from the buffer.
 */
std::expected<std::chrono::system_clock::time_point, error_code>
read_timestamp(std::span<const std::byte>& data) {
    auto high_result = reader::read_uint32(data);
    if (!high_result) return std::unexpected(high_result.error());
    auto low_result = reader::read_uint32(data);
    if (!low_result) return std::unexpected(low_result.error());

    std::uint64_t epoch = (static_cast<std::uint64_t>(*high_result) << 32) |
                          static_cast<std::uint64_t>(*low_result);
    return std::chrono::system_clock::time_point(
        std::chrono::milliseconds(epoch));
}

/**
 * @brief Write an optional timestamp.
 */
void write_optional_timestamp(std::vector<std::byte>& buffer,
    const std::optional<std::chrono::system_clock::time_point>& tp) {
    if (tp) {
        writer::write_bool(buffer, true);
        write_timestamp(buffer, *tp);
    } else {
        writer::write_bool(buffer, false);
    }
}

/**
 * @brief Read an optional timestamp.
 */
std::expected<std::optional<std::chrono::system_clock::time_point>, error_code>
read_optional_timestamp(std::span<const std::byte>& data) {
    auto has_value = reader::read_bool(data);
    if (!has_value) return std::unexpected(has_value.error());

    if (*has_value) {
        auto tp = read_timestamp(data);
        if (!tp) return std::unexpected(tp.error());
        return *tp;
    }
    return std::nullopt;
}

/**
 * @brief Serialize a session to the buffer.
 */
void write_session(std::vector<std::byte>& buffer, const domain::session& s) {
    writer::write_uuid(buffer, s.id);
    writer::write_uuid(buffer, s.account_id);
    write_timestamp(buffer, s.start_time);
    write_optional_timestamp(buffer, s.end_time);
    writer::write_string(buffer, s.client_ip.to_string());
    writer::write_string(buffer, s.client_identifier);
    writer::write_uint16(buffer, s.client_version_major);
    writer::write_uint16(buffer, s.client_version_minor);
    writer::write_uint64(buffer, s.bytes_sent);
    writer::write_uint64(buffer, s.bytes_received);
    writer::write_string(buffer, s.country_code);
}

/**
 * @brief Deserialize a session from the buffer.
 */
std::expected<domain::session, error_code>
read_session(std::span<const std::byte>& data) {
    domain::session s;

    auto id = reader::read_uuid(data);
    if (!id) return std::unexpected(id.error());
    s.id = *id;

    auto account_id = reader::read_uuid(data);
    if (!account_id) return std::unexpected(account_id.error());
    s.account_id = *account_id;

    auto start_time = read_timestamp(data);
    if (!start_time) return std::unexpected(start_time.error());
    s.start_time = *start_time;

    auto end_time = read_optional_timestamp(data);
    if (!end_time) return std::unexpected(end_time.error());
    s.end_time = *end_time;

    auto client_ip_str = reader::read_string(data);
    if (!client_ip_str) return std::unexpected(client_ip_str.error());
    try {
        s.client_ip = boost::asio::ip::make_address(*client_ip_str);
    } catch (const boost::system::system_error&) {
        s.client_ip = boost::asio::ip::address_v4();
    }

    auto client_identifier = reader::read_string(data);
    if (!client_identifier) return std::unexpected(client_identifier.error());
    s.client_identifier = *client_identifier;

    auto version_major = reader::read_uint16(data);
    if (!version_major) return std::unexpected(version_major.error());
    s.client_version_major = *version_major;

    auto version_minor = reader::read_uint16(data);
    if (!version_minor) return std::unexpected(version_minor.error());
    s.client_version_minor = *version_minor;

    auto bytes_sent = reader::read_uint64(data);
    if (!bytes_sent) return std::unexpected(bytes_sent.error());
    s.bytes_sent = *bytes_sent;

    auto bytes_received = reader::read_uint64(data);
    if (!bytes_received) return std::unexpected(bytes_received.error());
    s.bytes_received = *bytes_received;

    auto country_code = reader::read_string(data);
    if (!country_code) return std::unexpected(country_code.error());
    s.country_code = *country_code;

    return s;
}

/**
 * @brief Serialize session_statistics to the buffer.
 */
void write_session_statistics(std::vector<std::byte>& buffer,
    const domain::session_statistics& s) {
    write_timestamp(buffer, s.period_start);
    write_timestamp(buffer, s.period_end);
    writer::write_uuid(buffer, s.account_id);
    writer::write_uint64(buffer, s.session_count);
    writer::write_string(buffer, std::to_string(s.avg_duration_seconds));
    writer::write_uint64(buffer, s.total_bytes_sent);
    writer::write_uint64(buffer, s.total_bytes_received);
    writer::write_string(buffer, std::to_string(s.avg_bytes_sent));
    writer::write_string(buffer, std::to_string(s.avg_bytes_received));
    writer::write_uint32(buffer, s.unique_countries);
}

/**
 * @brief Deserialize session_statistics from the buffer.
 */
std::expected<domain::session_statistics, error_code>
read_session_statistics(std::span<const std::byte>& data) {
    domain::session_statistics s;

    auto period_start = read_timestamp(data);
    if (!period_start) return std::unexpected(period_start.error());
    s.period_start = *period_start;

    auto period_end = read_timestamp(data);
    if (!period_end) return std::unexpected(period_end.error());
    s.period_end = *period_end;

    auto account_id = reader::read_uuid(data);
    if (!account_id) return std::unexpected(account_id.error());
    s.account_id = *account_id;

    auto session_count = reader::read_uint64(data);
    if (!session_count) return std::unexpected(session_count.error());
    s.session_count = *session_count;

    auto avg_duration_str = reader::read_string(data);
    if (!avg_duration_str) return std::unexpected(avg_duration_str.error());
    try { s.avg_duration_seconds = std::stod(*avg_duration_str); } catch (...) {}

    auto total_bytes_sent = reader::read_uint64(data);
    if (!total_bytes_sent) return std::unexpected(total_bytes_sent.error());
    s.total_bytes_sent = *total_bytes_sent;

    auto total_bytes_received = reader::read_uint64(data);
    if (!total_bytes_received) return std::unexpected(total_bytes_received.error());
    s.total_bytes_received = *total_bytes_received;

    auto avg_bytes_sent_str = reader::read_string(data);
    if (!avg_bytes_sent_str) return std::unexpected(avg_bytes_sent_str.error());
    try { s.avg_bytes_sent = std::stod(*avg_bytes_sent_str); } catch (...) {}

    auto avg_bytes_received_str = reader::read_string(data);
    if (!avg_bytes_received_str) return std::unexpected(avg_bytes_received_str.error());
    try { s.avg_bytes_received = std::stod(*avg_bytes_received_str); } catch (...) {}

    auto unique_countries = reader::read_uint32(data);
    if (!unique_countries) return std::unexpected(unique_countries.error());
    s.unique_countries = *unique_countries;

    return s;
}

} // anonymous namespace

// list_sessions_request

std::vector<std::byte> list_sessions_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    writer::write_uint32(buffer, limit);
    writer::write_uint32(buffer, offset);
    return buffer;
}

std::expected<list_sessions_request, comms::messaging::error_code>
list_sessions_request::deserialize(std::span<const std::byte> data) {
    list_sessions_request request;

    auto account_id = reader::read_uuid(data);
    if (!account_id) return std::unexpected(account_id.error());
    request.account_id = *account_id;

    auto limit = reader::read_uint32(data);
    if (!limit) return std::unexpected(limit.error());
    request.limit = *limit;

    auto offset = reader::read_uint32(data);
    if (!offset) return std::unexpected(offset.error());
    request.offset = *offset;

    return request;
}

std::ostream& operator<<(std::ostream& s, const list_sessions_request& v) {
    rfl::json::write(v, s);
    return s;
}

// list_sessions_response

std::vector<std::byte> list_sessions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(sessions.size()));
    for (const auto& session : sessions) {
        write_session(buffer, session);
    }
    writer::write_uint32(buffer, total_count);
    return buffer;
}

std::expected<list_sessions_response, comms::messaging::error_code>
list_sessions_response::deserialize(std::span<const std::byte> data) {
    list_sessions_response response;

    auto count = reader::read_uint32(data);
    if (!count) return std::unexpected(count.error());

    response.sessions.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        auto session = read_session(data);
        if (!session) return std::unexpected(session.error());
        response.sessions.push_back(std::move(*session));
    }

    auto total_count = reader::read_uint32(data);
    if (!total_count) return std::unexpected(total_count.error());
    response.total_count = *total_count;

    return response;
}

std::ostream& operator<<(std::ostream& s, const list_sessions_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_session_statistics_request

std::vector<std::byte> get_session_statistics_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, account_id);
    write_timestamp(buffer, start_time);
    write_timestamp(buffer, end_time);
    return buffer;
}

std::expected<get_session_statistics_request, comms::messaging::error_code>
get_session_statistics_request::deserialize(std::span<const std::byte> data) {
    get_session_statistics_request request;

    auto account_id = reader::read_uuid(data);
    if (!account_id) return std::unexpected(account_id.error());
    request.account_id = *account_id;

    auto start_time = read_timestamp(data);
    if (!start_time) return std::unexpected(start_time.error());
    request.start_time = *start_time;

    auto end_time = read_timestamp(data);
    if (!end_time) return std::unexpected(end_time.error());
    request.end_time = *end_time;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_session_statistics_request& v) {
    rfl::json::write(v, s);
    return s;
}

// get_session_statistics_response

std::vector<std::byte> get_session_statistics_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(statistics.size()));
    for (const auto& stat : statistics) {
        write_session_statistics(buffer, stat);
    }
    return buffer;
}

std::expected<get_session_statistics_response, comms::messaging::error_code>
get_session_statistics_response::deserialize(std::span<const std::byte> data) {
    get_session_statistics_response response;

    auto count = reader::read_uint32(data);
    if (!count) return std::unexpected(count.error());

    response.statistics.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        auto stat = read_session_statistics(data);
        if (!stat) return std::unexpected(stat.error());
        response.statistics.push_back(std::move(*stat));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_session_statistics_response& v) {
    rfl::json::write(v, s);
    return s;
}

// get_active_sessions_request

std::vector<std::byte> get_active_sessions_request::serialize() const {
    return {};
}

std::expected<get_active_sessions_request, comms::messaging::error_code>
get_active_sessions_request::deserialize(std::span<const std::byte> data) {
    (void)data;
    return get_active_sessions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_active_sessions_request&) {
    s << "get_active_sessions_request{}";
    return s;
}

// get_active_sessions_response

std::vector<std::byte> get_active_sessions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(sessions.size()));
    for (const auto& session : sessions) {
        write_session(buffer, session);
    }
    return buffer;
}

std::expected<get_active_sessions_response, comms::messaging::error_code>
get_active_sessions_response::deserialize(std::span<const std::byte> data) {
    get_active_sessions_response response;

    auto count = reader::read_uint32(data);
    if (!count) return std::unexpected(count.error());

    response.sessions.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        auto session = read_session(data);
        if (!session) return std::unexpected(session.error());
        response.sessions.push_back(std::move(*session));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_active_sessions_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
