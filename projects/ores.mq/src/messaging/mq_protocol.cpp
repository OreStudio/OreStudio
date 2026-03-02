/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.mq/messaging/mq_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::mq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

void write_queue_info(std::vector<std::byte>& buffer,
    const pgmq::queue_info& qi) {
    writer::write_string(buffer, qi.queue_name);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point_utc(qi.created_at));
    writer::write_bool(buffer, qi.is_unlogged);
    writer::write_bool(buffer, qi.is_partitioned);
}

std::expected<pgmq::queue_info, error_code>
read_queue_info(std::span<const std::byte>& data) {
    pgmq::queue_info qi;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    qi.queue_name = *name_result;

    auto created_at_result = reader::read_string(data);
    if (!created_at_result) return std::unexpected(created_at_result.error());
    try {
        qi.created_at = ores::platform::time::datetime::parse_time_point(*created_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto is_unlogged_result = reader::read_bool(data);
    if (!is_unlogged_result) return std::unexpected(is_unlogged_result.error());
    qi.is_unlogged = *is_unlogged_result;

    auto is_partitioned_result = reader::read_bool(data);
    if (!is_partitioned_result) return std::unexpected(is_partitioned_result.error());
    qi.is_partitioned = *is_partitioned_result;

    return qi;
}

void write_queue_metrics(std::vector<std::byte>& buffer,
    const pgmq::queue_metrics& qm) {
    writer::write_string(buffer, qm.queue_name);
    writer::write_int64(buffer, qm.queue_length);
    if (qm.newest_msg_age_sec.has_value()) {
        writer::write_bool(buffer, true);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(*qm.newest_msg_age_sec));
    } else {
        writer::write_bool(buffer, false);
    }
    if (qm.oldest_msg_age_sec.has_value()) {
        writer::write_bool(buffer, true);
        writer::write_uint32(buffer, static_cast<std::uint32_t>(*qm.oldest_msg_age_sec));
    } else {
        writer::write_bool(buffer, false);
    }
    writer::write_int64(buffer, qm.total_messages);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point_utc(qm.scrape_time));
}

std::expected<pgmq::queue_metrics, error_code>
read_queue_metrics(std::span<const std::byte>& data) {
    pgmq::queue_metrics qm;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    qm.queue_name = *name_result;

    auto length_result = reader::read_int64(data);
    if (!length_result) return std::unexpected(length_result.error());
    qm.queue_length = *length_result;

    auto has_newest_result = reader::read_bool(data);
    if (!has_newest_result) return std::unexpected(has_newest_result.error());
    if (*has_newest_result) {
        auto newest_result = reader::read_uint32(data);
        if (!newest_result) return std::unexpected(newest_result.error());
        qm.newest_msg_age_sec = static_cast<int32_t>(*newest_result);
    }

    auto has_oldest_result = reader::read_bool(data);
    if (!has_oldest_result) return std::unexpected(has_oldest_result.error());
    if (*has_oldest_result) {
        auto oldest_result = reader::read_uint32(data);
        if (!oldest_result) return std::unexpected(oldest_result.error());
        qm.oldest_msg_age_sec = static_cast<int32_t>(*oldest_result);
    }

    auto total_result = reader::read_int64(data);
    if (!total_result) return std::unexpected(total_result.error());
    qm.total_messages = *total_result;

    auto scrape_time_result = reader::read_string(data);
    if (!scrape_time_result) return std::unexpected(scrape_time_result.error());
    try {
        qm.scrape_time = ores::platform::time::datetime::parse_time_point(*scrape_time_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return qm;
}

} // anonymous namespace

// ============================================================================
// get_queues_request
// ============================================================================

std::vector<std::byte> get_queues_request::serialize() const {
    return {};
}

std::expected<get_queues_request, error_code>
get_queues_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_queues_request{};
}

std::ostream& operator<<(std::ostream& s, const get_queues_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queues_response
// ============================================================================

std::vector<std::byte> get_queues_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(queues.size()));
    for (const auto& qi : queues) {
        write_queue_info(buffer, qi);
    }
    return buffer;
}

std::expected<get_queues_response, error_code>
get_queues_response::deserialize(std::span<const std::byte> data) {
    get_queues_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.queues.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_info(data);
        if (!result) return std::unexpected(result.error());
        response.queues.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_queues_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_metrics_request
// ============================================================================

std::vector<std::byte> get_queue_metrics_request::serialize() const {
    return {};
}

std::expected<get_queue_metrics_request, error_code>
get_queue_metrics_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_queue_metrics_request{};
}

std::ostream& operator<<(std::ostream& s, const get_queue_metrics_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_metrics_response
// ============================================================================

std::vector<std::byte> get_queue_metrics_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(metrics.size()));
    for (const auto& qm : metrics) {
        write_queue_metrics(buffer, qm);
    }
    return buffer;
}

std::expected<get_queue_metrics_response, error_code>
get_queue_metrics_response::deserialize(std::span<const std::byte> data) {
    get_queue_metrics_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.metrics.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_metrics(data);
        if (!result) return std::unexpected(result.error());
        response.metrics.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_queue_metrics_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
