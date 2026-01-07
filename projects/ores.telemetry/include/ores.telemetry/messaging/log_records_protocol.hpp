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
#ifndef ORES_TELEMETRY_MESSAGING_LOG_RECORDS_PROTOCOL_HPP
#define ORES_TELEMETRY_MESSAGING_LOG_RECORDS_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.telemetry/domain/log_record.hpp"

namespace ores::telemetry::messaging {

using error_code = ores::utility::serialization::error_code;

/**
 * @brief Request to submit a batch of log records to the server.
 *
 * This is a fire-and-forget message. The client does not wait for a response
 * and continues processing immediately. The server stores the records in the
 * telemetry database for analysis and correlation.
 *
 * Records should be batched for efficiency. Typical batch sizes are 50-100
 * records, or send when a flush interval (e.g., 5 seconds) has elapsed.
 */
struct submit_log_records_request final {
    /**
     * @brief The batch of log records to submit.
     */
    std::vector<domain::log_record> records;

    /**
     * @brief Serialize request to bytes.
     *
     * Format:
     * - 4 bytes: record count (uint32)
     * For each record:
     * - 8 bytes: timestamp (milliseconds since epoch, uint64)
     * - 1 byte: severity level (uint8)
     * - 2 bytes: body length
     * - N bytes: body (UTF-8)
     * - 2 bytes: logger_name length
     * - N bytes: logger_name (UTF-8)
     * - 1 byte: has_trace (boolean)
     * - 32 bytes: trace_id hex string (if has_trace)
     * - 1 byte: has_span (boolean)
     * - 16 bytes: span_id hex string (if has_span)
     * - 2 bytes: service_name length
     * - N bytes: service_name (UTF-8, from resource if present)
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize request from bytes.
     */
    static std::expected<submit_log_records_request, error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const submit_log_records_request& v);

}

#endif
