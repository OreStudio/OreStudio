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
#include "ores.telemetry/messaging/log_records_protocol.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>

namespace ores::telemetry::messaging {

using comms::messaging::error_code;

namespace {

/*
 * Local serialization helpers to avoid circular dependency with ores.comms.
 * These mirror the ores.comms reader/writer utilities but are self-contained.
 */

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

void write_uint8(std::vector<std::byte>& buffer, std::uint8_t value) {
    buffer.push_back(static_cast<std::byte>(value));
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

void write_bytes(std::vector<std::byte>& buffer,
                 std::span<const std::byte> bytes) {
    buffer.insert(buffer.end(), bytes.begin(), bytes.end());
}

std::expected<std::uint16_t, error_code>
read_uint16(std::span<const std::byte>& data) {
    if (data.size() < 2) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint16_t value = (static_cast<std::uint16_t>(data[0]) << 8) |
                          static_cast<std::uint16_t>(data[1]);
    data = data.subspan(2);
    return value;
}

std::expected<std::uint32_t, error_code>
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

std::expected<std::uint64_t, error_code>
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

std::expected<std::uint8_t, error_code>
read_uint8(std::span<const std::byte>& data) {
    if (data.empty()) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::uint8_t value = std::to_integer<std::uint8_t>(data[0]);
    data = data.subspan(1);
    return value;
}

std::expected<bool, error_code>
read_bool(std::span<const std::byte>& data) {
    if (data.empty()) {
        return std::unexpected(error_code::payload_incomplete);
    }
    bool value = std::to_integer<std::uint8_t>(data[0]) != 0;
    data = data.subspan(1);
    return value;
}

std::expected<std::string, error_code>
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

std::expected<void, error_code>
read_bytes(std::span<const std::byte>& data,
           std::span<std::byte> dest) {
    if (data.size() < dest.size()) {
        return std::unexpected(error_code::payload_incomplete);
    }
    std::copy(data.begin(), data.begin() + dest.size(), dest.begin());
    data = data.subspan(dest.size());
    return {};
}

} // anonymous namespace

std::vector<std::byte> submit_log_records_request::serialize() const {
    std::vector<std::byte> buffer;

    /*
     * Write record count.
     */
    write_uint32(buffer, static_cast<std::uint32_t>(records.size()));

    /*
     * Write each record.
     */
    for (const auto& rec : records) {
        /*
         * Timestamp as milliseconds since epoch.
         */
        auto epoch_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            rec.timestamp.time_since_epoch()).count();
        write_uint64(buffer, static_cast<std::uint64_t>(epoch_ms));

        /*
         * Severity level.
         */
        write_uint8(buffer, static_cast<std::uint8_t>(rec.severity));

        /*
         * Body.
         */
        write_string(buffer, rec.body);

        /*
         * Logger name.
         */
        write_string(buffer, rec.logger_name);

        /*
         * Trace ID (optional).
         */
        write_bool(buffer, rec.trace.has_value());
        if (rec.trace) {
            write_bytes(buffer, std::span<const std::byte>(rec.trace->bytes));
        }

        /*
         * Span ID (optional).
         */
        write_bool(buffer, rec.span.has_value());
        if (rec.span) {
            write_bytes(buffer, std::span<const std::byte>(rec.span->bytes));
        }

        /*
         * Service name from resource.
         */
        std::string service_name;
        if (rec.source_resource) {
            auto sn = rec.source_resource->service_name();
            if (sn) {
                service_name = *sn;
            }
        }
        write_string(buffer, service_name);
    }

    return buffer;
}

std::expected<submit_log_records_request, comms::messaging::error_code>
submit_log_records_request::deserialize(std::span<const std::byte> data) {
    submit_log_records_request request;

    /*
     * Read record count.
     */
    auto count_result = read_uint32(data);
    if (!count_result) {
        return std::unexpected(count_result.error());
    }
    auto count = *count_result;

    request.records.reserve(count);

    for (std::uint32_t i = 0; i < count; ++i) {
        domain::log_record rec;

        /*
         * Timestamp.
         */
        auto ts_result = read_uint64(data);
        if (!ts_result) {
            return std::unexpected(ts_result.error());
        }
        rec.timestamp = std::chrono::system_clock::time_point(
            std::chrono::milliseconds(*ts_result));

        /*
         * Severity level.
         */
        auto sev_result = read_uint8(data);
        if (!sev_result) {
            return std::unexpected(sev_result.error());
        }
        rec.severity = static_cast<domain::severity_level>(*sev_result);

        /*
         * Body.
         */
        auto body_result = read_string(data);
        if (!body_result) {
            return std::unexpected(body_result.error());
        }
        rec.body = std::move(*body_result);

        /*
         * Logger name.
         */
        auto logger_result = read_string(data);
        if (!logger_result) {
            return std::unexpected(logger_result.error());
        }
        rec.logger_name = std::move(*logger_result);

        /*
         * Trace ID (optional).
         */
        auto has_trace_result = read_bool(data);
        if (!has_trace_result) {
            return std::unexpected(has_trace_result.error());
        }
        if (*has_trace_result) {
            domain::trace_id tid;
            auto read_result = read_bytes(data, std::span<std::byte>(tid.bytes));
            if (!read_result) {
                return std::unexpected(read_result.error());
            }
            rec.trace = tid;
        }

        /*
         * Span ID (optional).
         */
        auto has_span_result = read_bool(data);
        if (!has_span_result) {
            return std::unexpected(has_span_result.error());
        }
        if (*has_span_result) {
            domain::span_id sid;
            auto read_result = read_bytes(data, std::span<std::byte>(sid.bytes));
            if (!read_result) {
                return std::unexpected(read_result.error());
            }
            rec.span = sid;
        }

        /*
         * Service name - we read it but don't store it as we don't
         * reconstruct the full resource on the server side.
         */
        auto service_result = read_string(data);
        if (!service_result) {
            return std::unexpected(service_result.error());
        }
        // Note: service_name is dropped - the server will use its own
        // resource assignment for storage.

        request.records.push_back(std::move(rec));
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const submit_log_records_request& v) {
    s << "submit_log_records_request{count=" << v.records.size() << "}";
    return s;
}

}
