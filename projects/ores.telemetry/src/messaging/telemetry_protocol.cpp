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
#include "ores.telemetry/messaging/telemetry_protocol.hpp"

#include <ostream>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>

namespace ores::telemetry::messaging {

using ores::utility::serialization::error_code;

namespace {

/*
 * Serialization helpers (mirror log_records_protocol.cpp).
 */

void write_uint8(std::vector<std::byte>& buffer, std::uint8_t value) {
    buffer.push_back(static_cast<std::byte>(value));
}

void write_uint16(std::vector<std::byte>& buffer, std::uint16_t value) {
    buffer.push_back(static_cast<std::byte>(value >> 8));
    buffer.push_back(static_cast<std::byte>(value & 0xFF));
}

void write_uint32(std::vector<std::byte>& buffer, std::uint32_t value) {
    buffer.push_back(static_cast<std::byte>(value >> 24));
    buffer.push_back(static_cast<std::byte>((value >> 16) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 8) & 0xFF));
    buffer.push_back(static_cast<std::byte>(value & 0xFF));
}

void write_uint64(std::vector<std::byte>& buffer, std::uint64_t value) {
    buffer.push_back(static_cast<std::byte>(value >> 56));
    buffer.push_back(static_cast<std::byte>((value >> 48) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 40) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 32) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 24) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 16) & 0xFF));
    buffer.push_back(static_cast<std::byte>((value >> 8) & 0xFF));
    buffer.push_back(static_cast<std::byte>(value & 0xFF));
}

void write_bool(std::vector<std::byte>& buffer, bool value) {
    buffer.push_back(static_cast<std::byte>(value ? 1 : 0));
}

void write_string(std::vector<std::byte>& buffer, const std::string& str) {
    write_uint16(buffer, static_cast<std::uint16_t>(str.size()));
    for (char c : str) {
        buffer.push_back(static_cast<std::byte>(c));
    }
}

void write_uuid(std::vector<std::byte>& buffer, const boost::uuids::uuid& id) {
    auto str = boost::lexical_cast<std::string>(id);
    write_string(buffer, str);
}

void write_optional_uuid(std::vector<std::byte>& buffer,
    const std::optional<boost::uuids::uuid>& id) {
    write_bool(buffer, id.has_value());
    if (id) {
        write_uuid(buffer, *id);
    }
}

void write_timestamp(std::vector<std::byte>& buffer,
    const std::chrono::system_clock::time_point& tp) {
    auto epoch_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        tp.time_since_epoch()).count();
    write_uint64(buffer, static_cast<std::uint64_t>(epoch_ms));
}

std::expected<std::uint8_t, ores::utility::serialization::error_code>
read_uint8(std::span<const std::byte>& data) {
    if (data.empty()) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint8_t value = std::to_integer<std::uint8_t>(data[0]);
    data = data.subspan(1);
    return value;
}

std::expected<std::uint16_t, ores::utility::serialization::error_code>
read_uint16(std::span<const std::byte>& data) {
    if (data.size() < 2) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint16_t value = (static_cast<std::uint16_t>(data[0]) << 8) |
                          static_cast<std::uint16_t>(data[1]);
    data = data.subspan(2);
    return value;
}

std::expected<std::uint32_t, ores::utility::serialization::error_code>
read_uint32(std::span<const std::byte>& data) {
    if (data.size() < 4) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint32_t value = (static_cast<std::uint32_t>(data[0]) << 24) |
                          (static_cast<std::uint32_t>(data[1]) << 16) |
                          (static_cast<std::uint32_t>(data[2]) << 8) |
                          static_cast<std::uint32_t>(data[3]);
    data = data.subspan(4);
    return value;
}

std::expected<std::uint64_t, ores::utility::serialization::error_code>
read_uint64(std::span<const std::byte>& data) {
    if (data.size() < 8) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint64_t value = (static_cast<std::uint64_t>(data[0]) << 56) |
                          (static_cast<std::uint64_t>(data[1]) << 48) |
                          (static_cast<std::uint64_t>(data[2]) << 40) |
                          (static_cast<std::uint64_t>(data[3]) << 32) |
                          (static_cast<std::uint64_t>(data[4]) << 24) |
                          (static_cast<std::uint64_t>(data[5]) << 16) |
                          (static_cast<std::uint64_t>(data[6]) << 8) |
                          static_cast<std::uint64_t>(data[7]);
    data = data.subspan(8);
    return value;
}

std::expected<bool, ores::utility::serialization::error_code>
read_bool(std::span<const std::byte>& data) {
    if (data.empty()) {
        return std::unexpected(error_code::payload_incomplete);
    }
    bool value = std::to_integer<std::uint8_t>(data[0]) != 0;
    data = data.subspan(1);
    return value;
}

std::expected<std::string, ores::utility::serialization::error_code>
read_string(std::span<const std::byte>& data) {
    auto len_result = read_uint16(data);
    if (!len_result) {
        return std::unexpected(len_result.error());
    }
    auto len = *len_result;
    if (data.size() < len) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::string str(reinterpret_cast<const char*>(data.data()), len);
    data = data.subspan(len);
    return str;
}

std::expected<boost::uuids::uuid, ores::utility::serialization::error_code>
read_uuid(std::span<const std::byte>& data) {
    auto str_result = read_string(data);
    if (!str_result) {
        return std::unexpected(str_result.error());
    }
    try {
        return boost::lexical_cast<boost::uuids::uuid>(*str_result);
    } catch (...) {
        return std::unexpected(error_code::invalid_request);
    }
}

std::expected<std::optional<boost::uuids::uuid>, ores::utility::serialization::error_code>
read_optional_uuid(std::span<const std::byte>& data) {
    auto has_value = read_bool(data);
    if (!has_value) {
        return std::unexpected(has_value.error());
    }
    if (!*has_value) {
        return std::nullopt;
    }
    auto uuid_result = read_uuid(data);
    if (!uuid_result) {
        return std::unexpected(uuid_result.error());
    }
    return *uuid_result;
}

std::expected<std::chrono::system_clock::time_point, ores::utility::serialization::error_code>
read_timestamp(std::span<const std::byte>& data) {
    auto ts_result = read_uint64(data);
    if (!ts_result) {
        return std::unexpected(ts_result.error());
    }
    return std::chrono::system_clock::time_point(
        std::chrono::milliseconds(*ts_result));
}

std::expected<std::optional<std::string>, ores::utility::serialization::error_code>
read_optional_string(std::span<const std::byte>& data) {
    auto has_value = read_bool(data);
    if (!has_value) {
        return std::unexpected(has_value.error());
    }
    if (!*has_value) {
        return std::nullopt;
    }
    auto str_result = read_string(data);
    if (!str_result) {
        return std::unexpected(str_result.error());
    }
    return *str_result;
}

void write_optional_string(std::vector<std::byte>& buffer,
    const std::optional<std::string>& str) {
    write_bool(buffer, str.has_value());
    if (str) {
        write_string(buffer, *str);
    }
}

} // anonymous namespace

/*
 * submit_telemetry_response
 */

std::vector<std::byte> submit_telemetry_response::serialize() const {
    std::vector<std::byte> buffer;
    write_bool(buffer, success);
    write_uint32(buffer, entries_accepted);
    write_string(buffer, message);
    return buffer;
}

std::expected<submit_telemetry_response, ores::utility::serialization::error_code>
submit_telemetry_response::deserialize(std::span<const std::byte> data) {
    submit_telemetry_response response;

    auto success_result = read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto count_result = read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    response.entries_accepted = *count_result;

    auto msg_result = read_string(data);
    if (!msg_result) return std::unexpected(msg_result.error());
    response.message = std::move(*msg_result);

    return response;
}

std::ostream& operator<<(std::ostream& s, const submit_telemetry_response& v) {
    s << "submit_telemetry_response{success=" << v.success
      << ", entries_accepted=" << v.entries_accepted
      << ", message=" << v.message << "}";
    return s;
}

/*
 * get_telemetry_logs_request
 */

std::vector<std::byte> get_telemetry_logs_request::serialize() const {
    std::vector<std::byte> buffer;

    write_timestamp(buffer, query.start_time);
    write_timestamp(buffer, query.end_time);

    // Optional filters
    write_bool(buffer, query.source.has_value());
    if (query.source) {
        write_uint8(buffer, static_cast<std::uint8_t>(*query.source));
    }

    write_optional_string(buffer, query.source_name);
    write_optional_uuid(buffer, query.session_id);
    write_optional_uuid(buffer, query.account_id);
    write_optional_string(buffer, query.level);
    write_optional_string(buffer, query.min_level);
    write_optional_string(buffer, query.component);
    write_optional_string(buffer, query.tag);
    write_optional_string(buffer, query.message_contains);

    write_uint32(buffer, query.limit);
    write_uint32(buffer, query.offset);

    return buffer;
}

std::expected<get_telemetry_logs_request, ores::utility::serialization::error_code>
get_telemetry_logs_request::deserialize(std::span<const std::byte> data) {
    get_telemetry_logs_request request;

    auto start_result = read_timestamp(data);
    if (!start_result) return std::unexpected(start_result.error());
    request.query.start_time = *start_result;

    auto end_result = read_timestamp(data);
    if (!end_result) return std::unexpected(end_result.error());
    request.query.end_time = *end_result;

    // Optional source
    auto has_source = read_bool(data);
    if (!has_source) return std::unexpected(has_source.error());
    if (*has_source) {
        auto source_result = read_uint8(data);
        if (!source_result) return std::unexpected(source_result.error());
        request.query.source = static_cast<domain::telemetry_source>(*source_result);
    }

    auto source_name = read_optional_string(data);
    if (!source_name) return std::unexpected(source_name.error());
    request.query.source_name = std::move(*source_name);

    auto session_id = read_optional_uuid(data);
    if (!session_id) return std::unexpected(session_id.error());
    request.query.session_id = *session_id;

    auto account_id = read_optional_uuid(data);
    if (!account_id) return std::unexpected(account_id.error());
    request.query.account_id = *account_id;

    auto level = read_optional_string(data);
    if (!level) return std::unexpected(level.error());
    request.query.level = std::move(*level);

    auto min_level = read_optional_string(data);
    if (!min_level) return std::unexpected(min_level.error());
    request.query.min_level = std::move(*min_level);

    auto component = read_optional_string(data);
    if (!component) return std::unexpected(component.error());
    request.query.component = std::move(*component);

    auto tag = read_optional_string(data);
    if (!tag) return std::unexpected(tag.error());
    request.query.tag = std::move(*tag);

    auto message_contains = read_optional_string(data);
    if (!message_contains) return std::unexpected(message_contains.error());
    request.query.message_contains = std::move(*message_contains);

    auto limit_result = read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.query.limit = *limit_result;

    auto offset_result = read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.query.offset = *offset_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_telemetry_logs_request& v) {
    s << "get_telemetry_logs_request{limit=" << v.query.limit
      << ", offset=" << v.query.offset << "}";
    return s;
}

/*
 * get_telemetry_logs_response
 */

std::vector<std::byte> get_telemetry_logs_response::serialize() const {
    std::vector<std::byte> buffer;

    write_bool(buffer, success);
    write_uint64(buffer, total_count);
    write_string(buffer, message);

    write_uint32(buffer, static_cast<std::uint32_t>(entries.size()));
    for (const auto& entry : entries) {
        write_uuid(buffer, entry.id);
        write_timestamp(buffer, entry.timestamp);
        write_uint8(buffer, static_cast<std::uint8_t>(entry.source));
        write_string(buffer, entry.source_name);
        write_optional_uuid(buffer, entry.session_id);
        write_optional_uuid(buffer, entry.account_id);
        write_string(buffer, entry.level);
        write_string(buffer, entry.component);
        write_string(buffer, entry.message);
        write_string(buffer, entry.tag);
        write_timestamp(buffer, entry.recorded_at);
    }

    return buffer;
}

std::expected<get_telemetry_logs_response, ores::utility::serialization::error_code>
get_telemetry_logs_response::deserialize(std::span<const std::byte> data) {
    get_telemetry_logs_response response;

    auto success_result = read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto total_result = read_uint64(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_count = *total_result;

    auto msg_result = read_string(data);
    if (!msg_result) return std::unexpected(msg_result.error());
    response.message = std::move(*msg_result);

    auto count_result = read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.entries.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::telemetry_log_entry entry;

        auto id_result = read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        entry.id = *id_result;

        auto ts_result = read_timestamp(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        entry.timestamp = *ts_result;

        auto source_result = read_uint8(data);
        if (!source_result) return std::unexpected(source_result.error());
        entry.source = static_cast<domain::telemetry_source>(*source_result);

        auto source_name = read_string(data);
        if (!source_name) return std::unexpected(source_name.error());
        entry.source_name = std::move(*source_name);

        auto session_id = read_optional_uuid(data);
        if (!session_id) return std::unexpected(session_id.error());
        entry.session_id = *session_id;

        auto account_id = read_optional_uuid(data);
        if (!account_id) return std::unexpected(account_id.error());
        entry.account_id = *account_id;

        auto level = read_string(data);
        if (!level) return std::unexpected(level.error());
        entry.level = std::move(*level);

        auto component = read_string(data);
        if (!component) return std::unexpected(component.error());
        entry.component = std::move(*component);

        auto message = read_string(data);
        if (!message) return std::unexpected(message.error());
        entry.message = std::move(*message);

        auto tag = read_string(data);
        if (!tag) return std::unexpected(tag.error());
        entry.tag = std::move(*tag);

        auto recorded = read_timestamp(data);
        if (!recorded) return std::unexpected(recorded.error());
        entry.recorded_at = *recorded;

        response.entries.push_back(std::move(entry));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_telemetry_logs_response& v) {
    s << "get_telemetry_logs_response{success=" << v.success
      << ", entries=" << v.entries.size()
      << ", total_count=" << v.total_count << "}";
    return s;
}

/*
 * get_telemetry_stats_request
 */

std::vector<std::byte> get_telemetry_stats_request::serialize() const {
    std::vector<std::byte> buffer;

    write_timestamp(buffer, query.start_time);
    write_timestamp(buffer, query.end_time);
    write_uint8(buffer, static_cast<std::uint8_t>(query.granularity));

    write_bool(buffer, query.source.has_value());
    if (query.source) {
        write_uint8(buffer, static_cast<std::uint8_t>(*query.source));
    }

    write_optional_string(buffer, query.source_name);
    write_optional_string(buffer, query.level);
    write_optional_string(buffer, query.component);

    return buffer;
}

std::expected<get_telemetry_stats_request, ores::utility::serialization::error_code>
get_telemetry_stats_request::deserialize(std::span<const std::byte> data) {
    get_telemetry_stats_request request;

    auto start_result = read_timestamp(data);
    if (!start_result) return std::unexpected(start_result.error());
    request.query.start_time = *start_result;

    auto end_result = read_timestamp(data);
    if (!end_result) return std::unexpected(end_result.error());
    request.query.end_time = *end_result;

    auto gran_result = read_uint8(data);
    if (!gran_result) return std::unexpected(gran_result.error());
    request.query.granularity = static_cast<domain::stats_granularity>(*gran_result);

    auto has_source = read_bool(data);
    if (!has_source) return std::unexpected(has_source.error());
    if (*has_source) {
        auto source_result = read_uint8(data);
        if (!source_result) return std::unexpected(source_result.error());
        request.query.source = static_cast<domain::telemetry_source>(*source_result);
    }

    auto source_name = read_optional_string(data);
    if (!source_name) return std::unexpected(source_name.error());
    request.query.source_name = std::move(*source_name);

    auto level = read_optional_string(data);
    if (!level) return std::unexpected(level.error());
    request.query.level = std::move(*level);

    auto component = read_optional_string(data);
    if (!component) return std::unexpected(component.error());
    request.query.component = std::move(*component);

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_telemetry_stats_request& v) {
    s << "get_telemetry_stats_request{granularity="
      << static_cast<int>(v.query.granularity) << "}";
    return s;
}

/*
 * get_telemetry_stats_response
 */

std::vector<std::byte> get_telemetry_stats_response::serialize() const {
    std::vector<std::byte> buffer;

    write_bool(buffer, success);
    write_string(buffer, message);

    write_uint32(buffer, static_cast<std::uint32_t>(stats.size()));
    for (const auto& s : stats) {
        write_timestamp(buffer, s.period_start);
        write_uint8(buffer, static_cast<std::uint8_t>(s.source));
        write_string(buffer, s.source_name);
        write_string(buffer, s.component);
        write_string(buffer, s.level);
        write_uint64(buffer, s.log_count);
        write_uint32(buffer, s.unique_sessions);
        write_uint32(buffer, s.unique_accounts);
    }

    return buffer;
}

std::expected<get_telemetry_stats_response, ores::utility::serialization::error_code>
get_telemetry_stats_response::deserialize(std::span<const std::byte> data) {
    get_telemetry_stats_response response;

    auto success_result = read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto msg_result = read_string(data);
    if (!msg_result) return std::unexpected(msg_result.error());
    response.message = std::move(*msg_result);

    auto count_result = read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.stats.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        domain::telemetry_stats stat;

        auto period_result = read_timestamp(data);
        if (!period_result) return std::unexpected(period_result.error());
        stat.period_start = *period_result;

        auto source_result = read_uint8(data);
        if (!source_result) return std::unexpected(source_result.error());
        stat.source = static_cast<domain::telemetry_source>(*source_result);

        auto source_name = read_string(data);
        if (!source_name) return std::unexpected(source_name.error());
        stat.source_name = std::move(*source_name);

        auto component = read_string(data);
        if (!component) return std::unexpected(component.error());
        stat.component = std::move(*component);

        auto level = read_string(data);
        if (!level) return std::unexpected(level.error());
        stat.level = std::move(*level);

        auto count_res = read_uint64(data);
        if (!count_res) return std::unexpected(count_res.error());
        stat.log_count = *count_res;

        auto sessions = read_uint32(data);
        if (!sessions) return std::unexpected(sessions.error());
        stat.unique_sessions = *sessions;

        auto accounts = read_uint32(data);
        if (!accounts) return std::unexpected(accounts.error());
        stat.unique_accounts = *accounts;

        response.stats.push_back(std::move(stat));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_telemetry_stats_response& v) {
    s << "get_telemetry_stats_response{success=" << v.success
      << ", stats=" << v.stats.size() << "}";
    return s;
}

}
