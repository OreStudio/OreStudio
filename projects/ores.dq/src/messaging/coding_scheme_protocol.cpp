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
#include "ores.dq/messaging/coding_scheme_protocol.hpp"

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
// Coding Scheme helpers
// ============================================================================

void write_coding_scheme(std::vector<std::byte>& buffer, const domain::coding_scheme& c) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(c.version));
    writer::write_string(buffer, c.code);
    writer::write_string(buffer, c.name);
    writer::write_string(buffer, c.authority_type);
    writer::write_string(buffer, c.subject_area_name);
    writer::write_string(buffer, c.domain_name);

    writer::write_bool(buffer, c.uri.has_value());
    if (c.uri.has_value()) {
        writer::write_string(buffer, *c.uri);
    }

    writer::write_string(buffer, c.description);
    writer::write_string(buffer, c.recorded_by);
    writer::write_string(buffer, c.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(c.recorded_at));
}

std::expected<domain::coding_scheme, error_code>
read_coding_scheme(std::span<const std::byte>& data) {
    domain::coding_scheme c;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    c.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    c.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    c.name = *name_result;

    auto authority_type_result = reader::read_string(data);
    if (!authority_type_result) return std::unexpected(authority_type_result.error());
    c.authority_type = *authority_type_result;

    auto subject_area_name_result = reader::read_string(data);
    if (!subject_area_name_result) return std::unexpected(subject_area_name_result.error());
    c.subject_area_name = *subject_area_name_result;

    auto domain_name_result = reader::read_string(data);
    if (!domain_name_result) return std::unexpected(domain_name_result.error());
    c.domain_name = *domain_name_result;

    auto has_uri_result = reader::read_bool(data);
    if (!has_uri_result) return std::unexpected(has_uri_result.error());
    if (*has_uri_result) {
        auto uri_result = reader::read_string(data);
        if (!uri_result) return std::unexpected(uri_result.error());
        c.uri = *uri_result;
    }

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    c.description = *description_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    c.recorded_by = *recorded_by_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    c.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        c.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return c;
}

// ============================================================================
// Coding Scheme Authority Type helpers
// ============================================================================

void write_coding_scheme_authority_type(std::vector<std::byte>& buffer,
    const domain::coding_scheme_authority_type& a) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(a.version));
    writer::write_string(buffer, a.code);
    writer::write_string(buffer, a.name);
    writer::write_string(buffer, a.description);
    writer::write_string(buffer, a.recorded_by);
    writer::write_string(buffer, a.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(a.recorded_at));
}

std::expected<domain::coding_scheme_authority_type, error_code>
read_coding_scheme_authority_type(std::span<const std::byte>& data) {
    domain::coding_scheme_authority_type a;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    a.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    a.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    a.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    a.description = *description_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    a.recorded_by = *recorded_by_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    a.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        a.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return a;
}

} // anonymous namespace

// ============================================================================
// Coding Scheme Messages Implementation
// ============================================================================

std::vector<std::byte> get_coding_schemes_request::serialize() const {
    return {};
}

std::expected<get_coding_schemes_request, error_code>
get_coding_schemes_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_coding_schemes_request{};
}

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_schemes_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(schemes.size()));
    for (const auto& c : schemes) {
        write_coding_scheme(buffer, c);
    }
    return buffer;
}

std::expected<get_coding_schemes_response, error_code>
get_coding_schemes_response::deserialize(std::span<const std::byte> data) {
    get_coding_schemes_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.schemes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto scheme_result = read_coding_scheme(data);
        if (!scheme_result) return std::unexpected(scheme_result.error());
        response.schemes.push_back(std::move(*scheme_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_schemes_by_authority_type_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, authority_type);
    return buffer;
}

std::expected<get_coding_schemes_by_authority_type_request, error_code>
get_coding_schemes_by_authority_type_request::deserialize(std::span<const std::byte> data) {
    get_coding_schemes_by_authority_type_request request;

    auto authority_type_result = reader::read_string(data);
    if (!authority_type_result) return std::unexpected(authority_type_result.error());
    request.authority_type = *authority_type_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_by_authority_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_schemes_by_authority_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(schemes.size()));
    for (const auto& c : schemes) {
        write_coding_scheme(buffer, c);
    }
    return buffer;
}

std::expected<get_coding_schemes_by_authority_type_response, error_code>
get_coding_schemes_by_authority_type_response::deserialize(std::span<const std::byte> data) {
    get_coding_schemes_by_authority_type_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.schemes.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto scheme_result = read_coding_scheme(data);
        if (!scheme_result) return std::unexpected(scheme_result.error());
        response.schemes.push_back(std::move(*scheme_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_coding_schemes_by_authority_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_coding_scheme_request::serialize() const {
    std::vector<std::byte> buffer;
    write_coding_scheme(buffer, scheme);
    return buffer;
}

std::expected<save_coding_scheme_request, error_code>
save_coding_scheme_request::deserialize(std::span<const std::byte> data) {
    save_coding_scheme_request request;

    auto scheme_result = read_coding_scheme(data);
    if (!scheme_result) return std::unexpected(scheme_result.error());
    request.scheme = std::move(*scheme_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_coding_scheme_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_coding_scheme_response, error_code>
save_coding_scheme_response::deserialize(std::span<const std::byte> data) {
    save_coding_scheme_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_coding_scheme_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_coding_scheme_request, error_code>
delete_coding_scheme_request::deserialize(std::span<const std::byte> data) {
    delete_coding_scheme_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_coding_scheme_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_coding_scheme_response, error_code>
delete_coding_scheme_response::deserialize(std::span<const std::byte> data) {
    delete_coding_scheme_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_coding_scheme_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_scheme_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_coding_scheme_history_request, error_code>
get_coding_scheme_history_request::deserialize(std::span<const std::byte> data) {
    get_coding_scheme_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_scheme_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_coding_scheme(buffer, v);
    }
    return buffer;
}

std::expected<get_coding_scheme_history_response, error_code>
get_coding_scheme_history_response::deserialize(std::span<const std::byte> data) {
    get_coding_scheme_history_response response;

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
        auto scheme_result = read_coding_scheme(data);
        if (!scheme_result) return std::unexpected(scheme_result.error());
        response.versions.push_back(std::move(*scheme_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Coding Scheme Authority Type Messages Implementation
// ============================================================================

std::vector<std::byte> get_coding_scheme_authority_types_request::serialize() const {
    return {};
}

std::expected<get_coding_scheme_authority_types_request, error_code>
get_coding_scheme_authority_types_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_coding_scheme_authority_types_request{};
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_types_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_scheme_authority_types_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(authority_types.size()));
    for (const auto& a : authority_types) {
        write_coding_scheme_authority_type(buffer, a);
    }
    return buffer;
}

std::expected<get_coding_scheme_authority_types_response, error_code>
get_coding_scheme_authority_types_response::deserialize(std::span<const std::byte> data) {
    get_coding_scheme_authority_types_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.authority_types.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto at_result = read_coding_scheme_authority_type(data);
        if (!at_result) return std::unexpected(at_result.error());
        response.authority_types.push_back(std::move(*at_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_types_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_coding_scheme_authority_type_request::serialize() const {
    std::vector<std::byte> buffer;
    write_coding_scheme_authority_type(buffer, authority_type);
    return buffer;
}

std::expected<save_coding_scheme_authority_type_request, error_code>
save_coding_scheme_authority_type_request::deserialize(std::span<const std::byte> data) {
    save_coding_scheme_authority_type_request request;

    auto at_result = read_coding_scheme_authority_type(data);
    if (!at_result) return std::unexpected(at_result.error());
    request.authority_type = std::move(*at_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_authority_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_coding_scheme_authority_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_coding_scheme_authority_type_response, error_code>
save_coding_scheme_authority_type_response::deserialize(std::span<const std::byte> data) {
    save_coding_scheme_authority_type_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_coding_scheme_authority_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_authority_type_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_coding_scheme_authority_type_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_coding_scheme_authority_type_request, error_code>
delete_coding_scheme_authority_type_request::deserialize(std::span<const std::byte> data) {
    delete_coding_scheme_authority_type_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_authority_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_coding_scheme_authority_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.code);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_coding_scheme_authority_type_response, error_code>
delete_coding_scheme_authority_type_response::deserialize(std::span<const std::byte> data) {
    delete_coding_scheme_authority_type_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_coding_scheme_authority_type_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_coding_scheme_authority_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_scheme_authority_type_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_coding_scheme_authority_type_history_request, error_code>
get_coding_scheme_authority_type_history_request::deserialize(std::span<const std::byte> data) {
    get_coding_scheme_authority_type_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_type_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_coding_scheme_authority_type_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_coding_scheme_authority_type(buffer, v);
    }
    return buffer;
}

std::expected<get_coding_scheme_authority_type_history_response, error_code>
get_coding_scheme_authority_type_history_response::deserialize(std::span<const std::byte> data) {
    get_coding_scheme_authority_type_history_response response;

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
        auto at_result = read_coding_scheme_authority_type(data);
        if (!at_result) return std::unexpected(at_result.error());
        response.versions.push_back(std::move(*at_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_coding_scheme_authority_type_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
