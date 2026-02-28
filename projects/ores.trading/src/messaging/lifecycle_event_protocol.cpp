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
#include "ores.trading/messaging/lifecycle_event_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::trading::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Lifecycle Event helpers
// ============================================================================

void write_lifecycle_event(std::vector<std::byte>& buffer,
    const domain::lifecycle_event& le) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(le.version));
    writer::write_string(buffer, le.code);
    writer::write_string(buffer, le.description);
    writer::write_bool(buffer, le.fsm_state_id.has_value());
    if (le.fsm_state_id.has_value()) {
        writer::write_uuid(buffer, *le.fsm_state_id);
    }
    writer::write_string(buffer, le.modified_by);
    writer::write_string(buffer, le.performed_by);
    writer::write_string(buffer, le.change_reason_code);
    writer::write_string(buffer, le.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(le.recorded_at));
}

std::expected<domain::lifecycle_event, error_code>
read_lifecycle_event(std::span<const std::byte>& data) {
    domain::lifecycle_event le;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    le.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    le.code = *code_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    le.description = *description_result;

    auto fsm_state_id_present_result = reader::read_bool(data);
    if (!fsm_state_id_present_result) return std::unexpected(fsm_state_id_present_result.error());
    if (*fsm_state_id_present_result) {
        auto fsm_state_id_result = reader::read_uuid(data);
        if (!fsm_state_id_result) return std::unexpected(fsm_state_id_result.error());
        le.fsm_state_id = *fsm_state_id_result;
    }

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    le.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    le.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    le.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    le.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        le.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return le;
}

} // anonymous namespace

// ============================================================================
// Lifecycle Event Messages Implementation
// ============================================================================

std::vector<std::byte> get_lifecycle_events_request::serialize() const {
    return {};
}

std::expected<get_lifecycle_events_request, error_code>
get_lifecycle_events_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_lifecycle_events_request{};
}

std::ostream& operator<<(std::ostream& s, const get_lifecycle_events_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_lifecycle_events_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(events.size()));
    for (const auto& le : events) {
        write_lifecycle_event(buffer, le);
    }
    return buffer;
}

std::expected<get_lifecycle_events_response, error_code>
get_lifecycle_events_response::deserialize(std::span<const std::byte> data) {
    get_lifecycle_events_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.events.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_lifecycle_event(data);
        if (!result) return std::unexpected(result.error());
        response.events.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_lifecycle_events_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_lifecycle_event_request
save_lifecycle_event_request::from(domain::lifecycle_event event) {
    return save_lifecycle_event_request{std::vector<domain::lifecycle_event>{std::move(event)}};
}

save_lifecycle_event_request
save_lifecycle_event_request::from(std::vector<domain::lifecycle_event> events) {
    return save_lifecycle_event_request{std::move(events)};
}

std::vector<std::byte> save_lifecycle_event_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(events.size()));
    for (const auto& e : events)
        write_lifecycle_event(buffer, e);
    return buffer;
}

std::expected<save_lifecycle_event_request, error_code>
save_lifecycle_event_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    save_lifecycle_event_request request;
    request.events.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_lifecycle_event(data);
        if (!e) return std::unexpected(e.error());
        request.events.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_lifecycle_event_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_lifecycle_event_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_lifecycle_event_response, error_code>
save_lifecycle_event_response::deserialize(std::span<const std::byte> data) {
    save_lifecycle_event_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_lifecycle_event_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_lifecycle_event_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_lifecycle_event_request, error_code>
delete_lifecycle_event_request::deserialize(std::span<const std::byte> data) {
    delete_lifecycle_event_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.codes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        request.codes.push_back(*code_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_lifecycle_event_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_lifecycle_event_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_lifecycle_event_response, error_code>
delete_lifecycle_event_response::deserialize(std::span<const std::byte> data) {
    delete_lifecycle_event_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_lifecycle_event_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_lifecycle_event_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_lifecycle_event_history_request, error_code>
get_lifecycle_event_history_request::deserialize(std::span<const std::byte> data) {
    get_lifecycle_event_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_lifecycle_event_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_lifecycle_event_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_lifecycle_event(buffer, v);
    }
    return buffer;
}

std::expected<get_lifecycle_event_history_response, error_code>
get_lifecycle_event_history_response::deserialize(std::span<const std::byte> data) {
    get_lifecycle_event_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.versions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_lifecycle_event(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_lifecycle_event_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
