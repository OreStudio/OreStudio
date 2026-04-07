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
#include "ores.dq.api/messaging/badge_definition_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::dq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Badge Definition helpers
// ============================================================================

void write_badge_definition(std::vector<std::byte>& buffer,
    const domain::badge_definition& bd) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bd.version));
    writer::write_string(buffer, bd.code);
    writer::write_string(buffer, bd.name);
    writer::write_string(buffer, bd.description);
    writer::write_string(buffer, bd.background_colour);
    writer::write_string(buffer, bd.text_colour);
    writer::write_string(buffer, bd.severity_code);
    writer::write_string(buffer, bd.css_class);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bd.display_order));
    writer::write_string(buffer, bd.modified_by);
    writer::write_string(buffer, bd.performed_by);
    writer::write_string(buffer, bd.change_reason_code);
    writer::write_string(buffer, bd.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(bd.recorded_at));
}

std::expected<domain::badge_definition, error_code>
read_badge_definition(std::span<const std::byte>& data) {
    domain::badge_definition bd;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    bd.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    bd.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    bd.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    bd.description = *description_result;

    auto background_colour_result = reader::read_string(data);
    if (!background_colour_result) return std::unexpected(background_colour_result.error());
    bd.background_colour = *background_colour_result;

    auto text_colour_result = reader::read_string(data);
    if (!text_colour_result) return std::unexpected(text_colour_result.error());
    bd.text_colour = *text_colour_result;

    auto severity_code_result = reader::read_string(data);
    if (!severity_code_result) return std::unexpected(severity_code_result.error());
    bd.severity_code = *severity_code_result;

    auto css_class_result = reader::read_string(data);
    if (!css_class_result) return std::unexpected(css_class_result.error());
    bd.css_class = *css_class_result;

    auto display_order_result = reader::read_uint32(data);
    if (!display_order_result) return std::unexpected(display_order_result.error());
    bd.display_order = static_cast<int>(*display_order_result);

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    bd.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    bd.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    bd.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    bd.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        bd.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return bd;
}

} // anonymous namespace

// ============================================================================
// Badge Definition Messages Implementation
// ============================================================================

std::vector<std::byte> get_badge_definitions_request::serialize() const {
    return {};
}

std::expected<get_badge_definitions_request, error_code>
get_badge_definitions_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_badge_definitions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_badge_definitions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_badge_definitions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(definitions.size()));
    for (const auto& bd : definitions) {
        write_badge_definition(buffer, bd);
    }
    return buffer;
}

std::expected<get_badge_definitions_response, error_code>
get_badge_definitions_response::deserialize(std::span<const std::byte> data) {
    get_badge_definitions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.definitions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_badge_definition(data);
        if (!result) return std::unexpected(result.error());
        response.definitions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_badge_definitions_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_badge_definition_request::serialize() const {
    std::vector<std::byte> buffer;
    write_badge_definition(buffer, definition);
    return buffer;
}

std::expected<save_badge_definition_request, error_code>
save_badge_definition_request::deserialize(std::span<const std::byte> data) {
    save_badge_definition_request request;

    auto result = read_badge_definition(data);
    if (!result) return std::unexpected(result.error());
    request.definition = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_badge_definition_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_badge_definition_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_badge_definition_response, error_code>
save_badge_definition_response::deserialize(std::span<const std::byte> data) {
    save_badge_definition_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_badge_definition_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_badge_definition_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_badge_definition_request, error_code>
delete_badge_definition_request::deserialize(std::span<const std::byte> data) {
    delete_badge_definition_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_badge_definition_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_badge_definition_response, error_code>
delete_badge_definition_response::deserialize(std::span<const std::byte> data) {
    delete_badge_definition_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_badge_definition_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_badge_definition_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_badge_definition_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_badge_definition_history_request, error_code>
get_badge_definition_history_request::deserialize(std::span<const std::byte> data) {
    get_badge_definition_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_badge_definition_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_badge_definition_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_badge_definition(buffer, v);
    }
    return buffer;
}

std::expected<get_badge_definition_history_response, error_code>
get_badge_definition_history_response::deserialize(std::span<const std::byte> data) {
    get_badge_definition_history_response response;

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
        auto result = read_badge_definition(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_badge_definition_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
