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
#include "ores.comms/messaging/reader.hpp"
#include "ores.comms/messaging/writer.hpp"

namespace ores::comms::messaging {

// subscribe_request

std::vector<std::byte> subscribe_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, event_type);
    return buffer;
}

std::expected<subscribe_request, error_code>
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

std::expected<subscribe_response, error_code>
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

std::expected<unsubscribe_request, error_code>
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

std::expected<unsubscribe_response, error_code>
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

    return buffer;
}

std::expected<notification_message, error_code>
notification_message::deserialize(std::span<const std::byte> data) {
    notification_message msg;

    auto event_type = reader::read_string(data);
    if (!event_type) return std::unexpected(event_type.error());
    msg.event_type = *event_type;

    auto ms = reader::read_int64(data);
    if (!ms) return std::unexpected(ms.error());
    msg.timestamp = std::chrono::system_clock::time_point(
        std::chrono::milliseconds(*ms));

    return msg;
}

std::ostream& operator<<(std::ostream& s, const notification_message& v) {
    auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        v.timestamp.time_since_epoch()).count();
    return s << "notification_message{event_type=" << v.event_type
             << ", timestamp=" << ms << "ms}";
}

}
