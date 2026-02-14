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
#include "ores.refdata/messaging/party_status_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::refdata::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Party Status helpers
// ============================================================================

void write_party_status(std::vector<std::byte>& buffer,
    const domain::party_status& ps) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ps.version));
    writer::write_string(buffer, ps.code);
    writer::write_string(buffer, ps.name);
    writer::write_string(buffer, ps.description);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ps.display_order));
    writer::write_string(buffer, ps.change_reason_code);
    writer::write_string(buffer, ps.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(ps.recorded_at));
}

std::expected<domain::party_status, error_code>
read_party_status(std::span<const std::byte>& data) {
    domain::party_status ps;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    ps.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    ps.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    ps.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    ps.description = *description_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    ps.display_order = static_cast<int>(*display_order_result);



    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    ps.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    ps.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        ps.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return ps;
}

} // anonymous namespace

// ============================================================================
// Party Status Messages Implementation
// ============================================================================

std::vector<std::byte> get_party_statuses_request::serialize() const {
    return {};
}

std::expected<get_party_statuses_request, error_code>
get_party_statuses_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_party_statuses_request{};
}

std::ostream& operator<<(std::ostream& s, const get_party_statuses_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_statuses_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(statuses.size()));
    for (const auto& ps : statuses) {
        write_party_status(buffer, ps);
    }
    return buffer;
}

std::expected<get_party_statuses_response, error_code>
get_party_statuses_response::deserialize(std::span<const std::byte> data) {
    get_party_statuses_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.statuses.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_party_status(data);
        if (!result) return std::unexpected(result.error());
        response.statuses.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_party_statuses_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_party_status_request::serialize() const {
    std::vector<std::byte> buffer;
    write_party_status(buffer, status);
    return buffer;
}

std::expected<save_party_status_request, error_code>
save_party_status_request::deserialize(std::span<const std::byte> data) {
    save_party_status_request request;

    auto result = read_party_status(data);
    if (!result) return std::unexpected(result.error());
    request.status = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_party_status_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_party_status_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_party_status_response, error_code>
save_party_status_response::deserialize(std::span<const std::byte> data) {
    save_party_status_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_party_status_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_party_status_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_party_status_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_party_status_request, error_code>
delete_party_status_request::deserialize(std::span<const std::byte> data) {
    delete_party_status_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_party_status_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_party_status_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_party_status_response, error_code>
delete_party_status_response::deserialize(std::span<const std::byte> data) {
    delete_party_status_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_party_status_result r;

        auto code_result = reader::read_string(data);
        if (!code_result) return std::unexpected(code_result.error());
        r.code = *code_result;

        auto success_result = reader::read_bool(data);
        if (!success_result) return std::unexpected(success_result.error());
        r.success = *success_result;

        auto message_result = reader::read_string(data);
        if (!message_result) return std::unexpected(message_result.error());
        r.message = *message_result;

        response.results.push_back(std::move(r));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_party_status_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_status_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_party_status_history_request, error_code>
get_party_status_history_request::deserialize(std::span<const std::byte> data) {
    get_party_status_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_party_status_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_status_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_party_status(buffer, v);
    }
    return buffer;
}

std::expected<get_party_status_history_response, error_code>
get_party_status_history_response::deserialize(std::span<const std::byte> data) {
    get_party_status_history_response response;

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
        auto result = read_party_status(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_party_status_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
