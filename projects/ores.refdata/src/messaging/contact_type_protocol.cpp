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
#include "ores.refdata/messaging/contact_type_protocol.hpp"

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
// Contact Type helpers
// ============================================================================

void write_contact_type(std::vector<std::byte>& buffer,
    const domain::contact_type& ct) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ct.version));
    writer::write_string(buffer, ct.code);
    writer::write_string(buffer, ct.name);
    writer::write_string(buffer, ct.description);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ct.display_order));
    writer::write_string(buffer, ct.recorded_by);
    writer::write_string(buffer, ct.performed_by);
    writer::write_string(buffer, ct.change_reason_code);
    writer::write_string(buffer, ct.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(ct.recorded_at));
}

std::expected<domain::contact_type, error_code>
read_contact_type(std::span<const std::byte>& data) {
    domain::contact_type ct;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    ct.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    ct.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    ct.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    ct.description = *description_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    ct.display_order = static_cast<int>(*display_order_result);

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    ct.recorded_by = *recorded_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    ct.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    ct.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    ct.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        ct.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return ct;
}

} // anonymous namespace

// ============================================================================
// Contact Type Messages Implementation
// ============================================================================

std::vector<std::byte> get_contact_types_request::serialize() const {
    return {};
}

std::expected<get_contact_types_request, error_code>
get_contact_types_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_contact_types_request{};
}

std::ostream& operator<<(std::ostream& s, const get_contact_types_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_contact_types_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(types.size()));
    for (const auto& ct : types) {
        write_contact_type(buffer, ct);
    }
    return buffer;
}

std::expected<get_contact_types_response, error_code>
get_contact_types_response::deserialize(std::span<const std::byte> data) {
    get_contact_types_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.types.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_contact_type(data);
        if (!result) return std::unexpected(result.error());
        response.types.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_contact_types_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_contact_type_request::serialize() const {
    std::vector<std::byte> buffer;
    write_contact_type(buffer, type);
    return buffer;
}

std::expected<save_contact_type_request, error_code>
save_contact_type_request::deserialize(std::span<const std::byte> data) {
    save_contact_type_request request;

    auto result = read_contact_type(data);
    if (!result) return std::unexpected(result.error());
    request.type = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_contact_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_contact_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_contact_type_response, error_code>
save_contact_type_response::deserialize(std::span<const std::byte> data) {
    save_contact_type_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_contact_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_contact_type_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_contact_type_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_contact_type_request, error_code>
delete_contact_type_request::deserialize(std::span<const std::byte> data) {
    delete_contact_type_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_contact_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_contact_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_contact_type_response, error_code>
delete_contact_type_response::deserialize(std::span<const std::byte> data) {
    delete_contact_type_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_contact_type_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_contact_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_contact_type_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_contact_type_history_request, error_code>
get_contact_type_history_request::deserialize(std::span<const std::byte> data) {
    get_contact_type_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_contact_type_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_contact_type_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_contact_type(buffer, v);
    }
    return buffer;
}

std::expected<get_contact_type_history_response, error_code>
get_contact_type_history_response::deserialize(std::span<const std::byte> data) {
    get_contact_type_history_response response;

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
        auto result = read_contact_type(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_contact_type_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
