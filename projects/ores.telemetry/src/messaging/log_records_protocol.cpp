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
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::telemetry::messaging {

using ores::utility::serialization::reader;
using ores::utility::serialization::writer;
using ores::utility::serialization::error_code;

namespace {

void write_bytes(std::vector<std::byte>& buffer,
                 std::span<const std::byte> bytes) {
    buffer.insert(buffer.end(), bytes.begin(), bytes.end());
}

std::expected<void, ores::utility::serialization::error_code>
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
    writer::write_uint32(buffer, static_cast<std::uint32_t>(records.size()));

    /*
     * Write each record.
     */
    for (const auto& rec : records) {
        /*
         * Timestamp as milliseconds since epoch.
         */
        auto epoch_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            rec.timestamp.time_since_epoch()).count();
        writer::write_uint64(buffer, static_cast<std::uint64_t>(epoch_ms));

        /*
         * Severity level.
         */
        writer::write_uint8(buffer, static_cast<std::uint8_t>(rec.severity));

        /*
         * Body.
         */
        writer::write_string(buffer, rec.body);

        /*
         * Logger name.
         */
        writer::write_string(buffer, rec.logger_name);

        /*
         * Trace ID (optional).
         */
        writer::write_bool(buffer, rec.trace.has_value());
        if (rec.trace) {
            write_bytes(buffer, std::span<const std::byte>(rec.trace->bytes));
        }

        /*
         * Span ID (optional).
         */
        writer::write_bool(buffer, rec.span.has_value());
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
        writer::write_string(buffer, service_name);
    }

    return buffer;
}

std::expected<submit_log_records_request, ores::utility::serialization::error_code>
submit_log_records_request::deserialize(std::span<const std::byte> data) {
    submit_log_records_request request;

    /*
     * Read record count.
     */
    auto count_result = reader::read_count(data);
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
        auto ts_result = reader::read_uint64(data);
        if (!ts_result) {
            return std::unexpected(ts_result.error());
        }
        rec.timestamp = std::chrono::system_clock::time_point(
            std::chrono::milliseconds(*ts_result));

        /*
         * Severity level.
         */
        auto sev_result = reader::read_uint8(data);
        if (!sev_result) {
            return std::unexpected(sev_result.error());
        }
        rec.severity = static_cast<logging::severity_level>(*sev_result);

        /*
         * Body.
         */
        auto body_result = reader::read_string(data);
        if (!body_result) {
            return std::unexpected(body_result.error());
        }
        rec.body = std::move(*body_result);

        /*
         * Logger name.
         */
        auto logger_result = reader::read_string(data);
        if (!logger_result) {
            return std::unexpected(logger_result.error());
        }
        rec.logger_name = std::move(*logger_result);

        /*
         * Trace ID (optional).
         */
        auto has_trace_result = reader::read_bool(data);
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
        auto has_span_result = reader::read_bool(data);
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
         * Service name - reconstruct a minimal resource to preserve the
         * source_name for the handler to use.
         */
        auto service_result = reader::read_string(data);
        if (!service_result) {
            return std::unexpected(service_result.error());
        }
        if (!service_result->empty()) {
            auto res = std::make_shared<domain::resource>();
            res->attrs["service.name"] = *service_result;
            rec.source_resource = res;
        }

        request.records.push_back(std::move(rec));
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const submit_log_records_request& v) {
    s << "submit_log_records_request{count=" << v.records.size() << "}";
    return s;
}

}
