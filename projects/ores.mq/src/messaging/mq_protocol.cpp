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

// ============================================================================
// get_queue_metric_samples_request
// ============================================================================

std::vector<std::byte> get_queue_metric_samples_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    if (from) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point_utc(*from));
    } else {
        writer::write_bool(buffer, false);
    }
    if (to) {
        writer::write_bool(buffer, true);
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point_utc(*to));
    } else {
        writer::write_bool(buffer, false);
    }
    return buffer;
}

std::expected<get_queue_metric_samples_request, error_code>
get_queue_metric_samples_request::deserialize(std::span<const std::byte> data) {
    get_queue_metric_samples_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto has_from_result = reader::read_bool(data);
    if (!has_from_result) return std::unexpected(has_from_result.error());
    if (*has_from_result) {
        auto ts_result = reader::read_string(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        try {
            request.from = ores::platform::time::datetime::parse_time_point(*ts_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto has_to_result = reader::read_bool(data);
    if (!has_to_result) return std::unexpected(has_to_result.error());
    if (*has_to_result) {
        auto ts_result = reader::read_string(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        try {
            request.to = ores::platform::time::datetime::parse_time_point(*ts_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    return request;
}

std::ostream& operator<<(std::ostream& s,
    const get_queue_metric_samples_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_queue_metric_samples_response
// ============================================================================

std::vector<std::byte> get_queue_metric_samples_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_string(buffer, queue_name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(samples.size()));
    for (const auto& s : samples) {
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point_utc(s.sample_time));
        writer::write_int64(buffer, s.queue_length);
        writer::write_int64(buffer, s.total_messages);
    }
    return buffer;
}

std::expected<get_queue_metric_samples_response, error_code>
get_queue_metric_samples_response::deserialize(std::span<const std::byte> data) {
    get_queue_metric_samples_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    response.queue_name = *name_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.samples.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        pgmq::metrics_sample s;

        auto ts_result = reader::read_string(data);
        if (!ts_result) return std::unexpected(ts_result.error());
        try {
            s.sample_time =
                ores::platform::time::datetime::parse_time_point(*ts_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }

        auto ql_result = reader::read_int64(data);
        if (!ql_result) return std::unexpected(ql_result.error());
        s.queue_length = *ql_result;

        auto tm_result = reader::read_int64(data);
        if (!tm_result) return std::unexpected(tm_result.error());
        s.total_messages = *tm_result;

        response.samples.push_back(std::move(s));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s,
    const get_queue_metric_samples_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Shared helpers for queue_message
// ============================================================================

namespace {

void write_queue_message(std::vector<std::byte>& buffer,
    const queue_message& m) {
    writer::write_int64(buffer, m.msg_id);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(m.read_ct));
    writer::write_string(buffer, m.enqueued_at);
    writer::write_string(buffer, m.vt);
    writer::write_string(buffer, m.payload);
}

std::expected<queue_message, error_code>
read_queue_message(std::span<const std::byte>& data) {
    queue_message m;

    auto id_result = reader::read_int64(data);
    if (!id_result) return std::unexpected(id_result.error());
    m.msg_id = *id_result;

    auto rct_result = reader::read_uint32(data);
    if (!rct_result) return std::unexpected(rct_result.error());
    m.read_ct = static_cast<std::int32_t>(*rct_result);

    auto enq_result = reader::read_string(data);
    if (!enq_result) return std::unexpected(enq_result.error());
    m.enqueued_at = *enq_result;

    auto vt_result = reader::read_string(data);
    if (!vt_result) return std::unexpected(vt_result.error());
    m.vt = *vt_result;

    auto payload_result = reader::read_string(data);
    if (!payload_result) return std::unexpected(payload_result.error());
    m.payload = *payload_result;

    return m;
}

std::expected<std::vector<queue_message>, error_code>
read_queue_messages(std::span<const std::byte>& data) {
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    std::vector<queue_message> messages;
    messages.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_queue_message(data);
        if (!result) return std::unexpected(result.error());
        messages.push_back(std::move(*result));
    }
    return messages;
}

} // anonymous namespace

// ============================================================================
// create_queue_request
// ============================================================================

std::vector<std::byte> create_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_bool(buffer, is_unlogged);
    return buffer;
}

std::expected<create_queue_request, error_code>
create_queue_request::deserialize(std::span<const std::byte> data) {
    create_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto unlogged_result = reader::read_bool(data);
    if (!unlogged_result) return std::unexpected(unlogged_result.error());
    request.is_unlogged = *unlogged_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const create_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// create_queue_response
// ============================================================================

std::vector<std::byte> create_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<create_queue_response, error_code>
create_queue_response::deserialize(std::span<const std::byte> data) {
    create_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const create_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// drop_queue_request
// ============================================================================

std::vector<std::byte> drop_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    return buffer;
}

std::expected<drop_queue_request, error_code>
drop_queue_request::deserialize(std::span<const std::byte> data) {
    drop_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const drop_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// drop_queue_response
// ============================================================================

std::vector<std::byte> drop_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<drop_queue_response, error_code>
drop_queue_response::deserialize(std::span<const std::byte> data) {
    drop_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const drop_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// purge_queue_request
// ============================================================================

std::vector<std::byte> purge_queue_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    return buffer;
}

std::expected<purge_queue_request, error_code>
purge_queue_request::deserialize(std::span<const std::byte> data) {
    purge_queue_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const purge_queue_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// purge_queue_response
// ============================================================================

std::vector<std::byte> purge_queue_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_int64(buffer, purged_count);
    return buffer;
}

std::expected<purge_queue_response, error_code>
purge_queue_response::deserialize(std::span<const std::byte> data) {
    purge_queue_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_int64(data);
    if (!count_result) return std::unexpected(count_result.error());
    response.purged_count = *count_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const purge_queue_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// send_message_request
// ============================================================================

std::vector<std::byte> send_message_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_string(buffer, payload);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(delay_seconds));
    return buffer;
}

std::expected<send_message_request, error_code>
send_message_request::deserialize(std::span<const std::byte> data) {
    send_message_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto payload_result = reader::read_string(data);
    if (!payload_result) return std::unexpected(payload_result.error());
    request.payload = *payload_result;

    auto delay_result = reader::read_uint32(data);
    if (!delay_result) return std::unexpected(delay_result.error());
    request.delay_seconds = static_cast<std::int32_t>(*delay_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const send_message_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// send_message_response
// ============================================================================

std::vector<std::byte> send_message_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_int64(buffer, msg_id);
    return buffer;
}

std::expected<send_message_response, error_code>
send_message_response::deserialize(std::span<const std::byte> data) {
    send_message_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto id_result = reader::read_int64(data);
    if (!id_result) return std::unexpected(id_result.error());
    response.msg_id = *id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const send_message_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// read_messages_request
// ============================================================================

std::vector<std::byte> read_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(count));
    writer::write_uint32(buffer, static_cast<std::uint32_t>(vt_seconds));
    return buffer;
}

std::expected<read_messages_request, error_code>
read_messages_request::deserialize(std::span<const std::byte> data) {
    read_messages_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    request.count = static_cast<std::int32_t>(*count_result);

    auto vt_result = reader::read_uint32(data);
    if (!vt_result) return std::unexpected(vt_result.error());
    request.vt_seconds = static_cast<std::int32_t>(*vt_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const read_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// read_messages_response
// ============================================================================

std::vector<std::byte> read_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(messages.size()));
    for (const auto& m : messages)
        write_queue_message(buffer, m);
    return buffer;
}

std::expected<read_messages_response, error_code>
read_messages_response::deserialize(std::span<const std::byte> data) {
    read_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto msgs = read_queue_messages(data);
    if (!msgs) return std::unexpected(msgs.error());
    response.messages = std::move(*msgs);

    return response;
}

std::ostream& operator<<(std::ostream& s, const read_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// pop_messages_request
// ============================================================================

std::vector<std::byte> pop_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(count));
    return buffer;
}

std::expected<pop_messages_request, error_code>
pop_messages_request::deserialize(std::span<const std::byte> data) {
    pop_messages_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    request.count = static_cast<std::int32_t>(*count_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const pop_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// pop_messages_response
// ============================================================================

std::vector<std::byte> pop_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(messages.size()));
    for (const auto& m : messages)
        write_queue_message(buffer, m);
    return buffer;
}

std::expected<pop_messages_response, error_code>
pop_messages_response::deserialize(std::span<const std::byte> data) {
    pop_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto msgs = read_queue_messages(data);
    if (!msgs) return std::unexpected(msgs.error());
    response.messages = std::move(*msgs);

    return response;
}

std::ostream& operator<<(std::ostream& s, const pop_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// delete_messages_request
// ============================================================================

std::vector<std::byte> delete_messages_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, queue_name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(msg_ids.size()));
    for (const auto id : msg_ids)
        writer::write_int64(buffer, id);
    return buffer;
}

std::expected<delete_messages_request, error_code>
delete_messages_request::deserialize(std::span<const std::byte> data) {
    delete_messages_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.queue_name = *name_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.msg_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_int64(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.msg_ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_messages_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// delete_messages_response
// ============================================================================

std::vector<std::byte> delete_messages_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(deleted_count));
    return buffer;
}

std::expected<delete_messages_response, error_code>
delete_messages_response::deserialize(std::span<const std::byte> data) {
    delete_messages_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    response.deleted_count = static_cast<std::int32_t>(*count_result);

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_messages_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
