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
#include "ores.comms/messaging/subscription_protocol.hpp"

#include <ostream>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::comms::messaging {

using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

// subscribe_request

std::vector<std::byte> subscribe_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, event_type);
    return buffer;
}

std::expected<subscribe_request, ores::utility::serialization::error_code>
subscribe_request::deserialize(std::span<const std::byte> data) {
    subscribe_request req;

    auto event_type = reader::read_string(data);
    if (!event_type) return std::unexpected(event_type.error());
    req.event_type = *event_type;

    return req;
}

std::ostream& operator<<(std::ostream& s, const subscribe_request& v) {
    return s << "subscribe_request{event_type=" << v.event_type << "}";
}

// subscribe_response

std::vector<std::byte> subscribe_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<subscribe_response, ores::utility::serialization::error_code>
subscribe_response::deserialize(std::span<const std::byte> data) {
    subscribe_response resp;

    auto success = reader::read_bool(data);
    if (!success) return std::unexpected(success.error());
    resp.success = *success;

    auto message = reader::read_string(data);
    if (!message) return std::unexpected(message.error());
    resp.message = *message;

    return resp;
}

std::ostream& operator<<(std::ostream& s, const subscribe_response& v) {
    return s << "subscribe_response{success=" << v.success
             << ", message=" << v.message << "}";
}

// unsubscribe_request

std::vector<std::byte> unsubscribe_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, event_type);
    return buffer;
}

std::expected<unsubscribe_request, ores::utility::serialization::error_code>
unsubscribe_request::deserialize(std::span<const std::byte> data) {
    unsubscribe_request req;

    auto event_type = reader::read_string(data);
    if (!event_type) return std::unexpected(event_type.error());
    req.event_type = *event_type;

    return req;
}

std::ostream& operator<<(std::ostream& s, const unsubscribe_request& v) {
    return s << "unsubscribe_request{event_type=" << v.event_type << "}";
}

// unsubscribe_response

std::vector<std::byte> unsubscribe_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<unsubscribe_response, ores::utility::serialization::error_code>
unsubscribe_response::deserialize(std::span<const std::byte> data) {
    unsubscribe_response resp;

    auto success = reader::read_bool(data);
    if (!success) return std::unexpected(success.error());
    resp.success = *success;

    auto message = reader::read_string(data);
    if (!message) return std::unexpected(message.error());
    resp.message = *message;

    return resp;
}

std::ostream& operator<<(std::ostream& s, const unsubscribe_response& v) {
    return s << "unsubscribe_response{success=" << v.success
             << ", message=" << v.message << "}";
}

// notification_message

std::vector<std::byte> notification_message::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, event_type);

    // Serialize timestamp as milliseconds since epoch
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        timestamp.time_since_epoch()).count();
    writer::write_int64(buffer, ms);

    // Serialize entity_ids
    writer::write_uint32(buffer, static_cast<std::uint32_t>(entity_ids.size()));
    for (const auto& id : entity_ids) {
        writer::write_string(buffer, id);
    }

    // Serialize tenant_id (for debugging multi-tenancy)
    writer::write_string(buffer, tenant_id);

    // Serialize payload_type (always present in new format)
    writer::write_uint8(buffer, static_cast<std::uint8_t>(pt));

    // Serialize payload bytes if present
    if (pt != payload_type::none && payload) {
        writer::write_uint32(buffer, static_cast<std::uint32_t>(payload->size()));
        buffer.insert(buffer.end(), payload->begin(), payload->end());
    }

    return buffer;
}

std::expected<notification_message, ores::utility::serialization::error_code>
notification_message::deserialize(std::span<const std::byte> data) {
    notification_message msg;

    auto event_type = reader::read_string(data);
    if (!event_type) return std::unexpected(event_type.error());
    msg.event_type = *event_type;

    auto ms = reader::read_int64(data);
    if (!ms) return std::unexpected(ms.error());
    msg.timestamp = std::chrono::system_clock::time_point(
        std::chrono::milliseconds(*ms));

    // Deserialize entity_ids
    auto count = reader::read_count(data);
    if (!count) return std::unexpected(count.error());

    msg.entity_ids.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        auto id = reader::read_string(data);
        if (!id) return std::unexpected(id.error());
        msg.entity_ids.push_back(*id);
    }

    // Deserialize tenant_id (for debugging multi-tenancy)
    auto tenant_id = reader::read_string(data);
    if (!tenant_id) return std::unexpected(tenant_id.error());
    msg.tenant_id = *tenant_id;

    // Payload fields are optional (backward compat: old senders don't write them)
    if (data.empty()) {
        return msg;
    }

    auto pt_raw = reader::read_uint8(data);
    if (!pt_raw) return std::unexpected(pt_raw.error());
    msg.pt = static_cast<payload_type>(*pt_raw);

    if (msg.pt != payload_type::none && !data.empty()) {
        auto len = reader::read_uint32(data);
        if (!len) return std::unexpected(len.error());
        if (data.size() < *len) {
            return std::unexpected(
                ores::utility::serialization::error_code::payload_incomplete);
        }
        msg.payload = std::vector<std::byte>(data.data(), data.data() + *len);
        data = data.subspan(*len);
    }

    return msg;
}

std::ostream& operator<<(std::ostream& s, const notification_message& v) {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        v.timestamp.time_since_epoch()).count();
    s << "notification_message{event_type=" << v.event_type
      << ", timestamp=" << ms << "ms, entity_ids=[";
    for (std::size_t i = 0; i < v.entity_ids.size(); ++i) {
        if (i > 0) s << ", ";
        s << v.entity_ids[i];
    }
    s << "], tenant_id=" << v.tenant_id
      << ", pt=" << static_cast<int>(v.pt)
      << ", has_payload=" << v.payload.has_value() << "}";
    return s;
}

// database_status_message

std::vector<std::byte> database_status_message::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, available);
    writer::write_string(buffer, error_message);

    // Serialize timestamp as milliseconds since epoch
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        timestamp.time_since_epoch()).count();
    writer::write_int64(buffer, ms);

    return buffer;
}

std::expected<database_status_message, ores::utility::serialization::error_code>
database_status_message::deserialize(std::span<const std::byte> data) {
    database_status_message msg;

    auto available = reader::read_bool(data);
    if (!available) return std::unexpected(available.error());
    msg.available = *available;

    auto error_message = reader::read_string(data);
    if (!error_message) return std::unexpected(error_message.error());
    msg.error_message = *error_message;

    auto ms = reader::read_int64(data);
    if (!ms) return std::unexpected(ms.error());
    msg.timestamp = std::chrono::system_clock::time_point(
        std::chrono::milliseconds(*ms));

    return msg;
}

std::ostream& operator<<(std::ostream& s, const database_status_message& v) {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        v.timestamp.time_since_epoch()).count();
    return s << "database_status_message{available=" << v.available
             << ", error_message=" << v.error_message
             << ", timestamp=" << ms << "ms}";
}

// list_event_channels_request

std::vector<std::byte> list_event_channels_request::serialize() const {
    // Empty payload - no fields to serialize
    return {};
}

std::expected<list_event_channels_request, ores::utility::serialization::error_code>
list_event_channels_request::deserialize(std::span<const std::byte> /*data*/) {
    // Empty payload - nothing to deserialize
    return list_event_channels_request{};
}

std::ostream& operator<<(std::ostream& s, const list_event_channels_request& /*v*/) {
    return s << "list_event_channels_request{}";
}

// list_event_channels_response

std::vector<std::byte> list_event_channels_response::serialize() const {
    std::vector<std::byte> buffer;

    // Serialize channel count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(channels.size()));

    // Serialize each channel
    for (const auto& channel : channels) {
        writer::write_string(buffer, channel.name);
        writer::write_string(buffer, channel.description);
    }

    return buffer;
}

std::expected<list_event_channels_response, ores::utility::serialization::error_code>
list_event_channels_response::deserialize(std::span<const std::byte> data) {
    list_event_channels_response resp;

    auto count = reader::read_count(data);
    if (!count) return std::unexpected(count.error());

    resp.channels.reserve(*count);
    for (std::uint32_t i = 0; i < *count; ++i) {
        eventing::domain::event_channel_info channel;

        auto name = reader::read_string(data);
        if (!name) return std::unexpected(name.error());
        channel.name = *name;

        auto description = reader::read_string(data);
        if (!description) return std::unexpected(description.error());
        channel.description = *description;

        resp.channels.push_back(std::move(channel));
    }

    return resp;
}

std::ostream& operator<<(std::ostream& s, const list_event_channels_response& v) {
    s << "list_event_channels_response{channels=[";
    for (std::size_t i = 0; i < v.channels.size(); ++i) {
        if (i > 0) s << ", ";
        s << "{name=" << v.channels[i].name
          << ", description=" << v.channels[i].description << "}";
    }
    return s << "]}";
}

}
