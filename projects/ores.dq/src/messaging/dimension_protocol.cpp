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
#include "ores.dq/messaging/dimension_protocol.hpp"

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
// Nature Dimension helpers
// ============================================================================

void write_nature_dimension(std::vector<std::byte>& buffer, const domain::nature_dimension& n) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(n.version));
    writer::write_string(buffer, n.code);
    writer::write_string(buffer, n.name);
    writer::write_string(buffer, n.description);
    writer::write_string(buffer, n.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(n.recorded_at));
}

std::expected<domain::nature_dimension, error_code>
read_nature_dimension(std::span<const std::byte>& data) {
    domain::nature_dimension n;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    n.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    n.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    n.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    n.description = *description_result;


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    n.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        n.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return n;
}

// ============================================================================
// Origin Dimension helpers
// ============================================================================

void write_origin_dimension(std::vector<std::byte>& buffer, const domain::origin_dimension& o) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(o.version));
    writer::write_string(buffer, o.code);
    writer::write_string(buffer, o.name);
    writer::write_string(buffer, o.description);
    writer::write_string(buffer, o.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(o.recorded_at));
}

std::expected<domain::origin_dimension, error_code>
read_origin_dimension(std::span<const std::byte>& data) {
    domain::origin_dimension o;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    o.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    o.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    o.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    o.description = *description_result;


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    o.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        o.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return o;
}

// ============================================================================
// Treatment Dimension helpers
// ============================================================================

void write_treatment_dimension(std::vector<std::byte>& buffer, const domain::treatment_dimension& t) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(t.version));
    writer::write_string(buffer, t.code);
    writer::write_string(buffer, t.name);
    writer::write_string(buffer, t.description);
    writer::write_string(buffer, t.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(t.recorded_at));
}

std::expected<domain::treatment_dimension, error_code>
read_treatment_dimension(std::span<const std::byte>& data) {
    domain::treatment_dimension t;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    t.version = static_cast<int>(*version_result);

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    t.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    t.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    t.description = *description_result;


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    t.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        t.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return t;
}

} // anonymous namespace

// ============================================================================
// Nature Dimension Messages Implementation
// ============================================================================

std::vector<std::byte> get_nature_dimensions_request::serialize() const {
    return {};
}

std::expected<get_nature_dimensions_request, error_code>
get_nature_dimensions_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_nature_dimensions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_nature_dimensions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_nature_dimensions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& n : dimensions) {
        write_nature_dimension(buffer, n);
    }
    return buffer;
}

std::expected<get_nature_dimensions_response, error_code>
get_nature_dimensions_response::deserialize(std::span<const std::byte> data) {
    get_nature_dimensions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.dimensions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto dim_result = read_nature_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.dimensions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_nature_dimensions_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_nature_dimension_request
save_nature_dimension_request::from(domain::nature_dimension dimension) {
    return save_nature_dimension_request{
        std::vector<domain::nature_dimension>{std::move(dimension)}};
}

save_nature_dimension_request
save_nature_dimension_request::from(std::vector<domain::nature_dimension> dimensions) {
    return save_nature_dimension_request{std::move(dimensions)};
}

std::vector<std::byte> save_nature_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& e : dimensions)
        write_nature_dimension(buffer, e);
    return buffer;
}

std::expected<save_nature_dimension_request, error_code>
save_nature_dimension_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    save_nature_dimension_request request;
    request.dimensions.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_nature_dimension(data);
        if (!e) return std::unexpected(e.error());
        request.dimensions.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_nature_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_nature_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_nature_dimension_response, error_code>
save_nature_dimension_response::deserialize(std::span<const std::byte> data) {
    save_nature_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_nature_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_nature_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_nature_dimension_request, error_code>
delete_nature_dimension_request::deserialize(std::span<const std::byte> data) {
    delete_nature_dimension_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_nature_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_nature_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_nature_dimension_response, error_code>
delete_nature_dimension_response::deserialize(std::span<const std::byte> data) {
    delete_nature_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_nature_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_nature_dimension_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_nature_dimension_history_request, error_code>
get_nature_dimension_history_request::deserialize(std::span<const std::byte> data) {
    get_nature_dimension_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_nature_dimension_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_nature_dimension_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_nature_dimension(buffer, v);
    }
    return buffer;
}

std::expected<get_nature_dimension_history_response, error_code>
get_nature_dimension_history_response::deserialize(std::span<const std::byte> data) {
    get_nature_dimension_history_response response;

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
        auto dim_result = read_nature_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.versions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_nature_dimension_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Origin Dimension Messages Implementation
// ============================================================================

std::vector<std::byte> get_origin_dimensions_request::serialize() const {
    return {};
}

std::expected<get_origin_dimensions_request, error_code>
get_origin_dimensions_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_origin_dimensions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_origin_dimensions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_origin_dimensions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& o : dimensions) {
        write_origin_dimension(buffer, o);
    }
    return buffer;
}

std::expected<get_origin_dimensions_response, error_code>
get_origin_dimensions_response::deserialize(std::span<const std::byte> data) {
    get_origin_dimensions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.dimensions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto dim_result = read_origin_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.dimensions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_origin_dimensions_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_origin_dimension_request
save_origin_dimension_request::from(domain::origin_dimension dimension) {
    return save_origin_dimension_request{
        std::vector<domain::origin_dimension>{std::move(dimension)}};
}

save_origin_dimension_request
save_origin_dimension_request::from(std::vector<domain::origin_dimension> dimensions) {
    return save_origin_dimension_request{std::move(dimensions)};
}

std::vector<std::byte> save_origin_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& e : dimensions)
        write_origin_dimension(buffer, e);
    return buffer;
}

std::expected<save_origin_dimension_request, error_code>
save_origin_dimension_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    save_origin_dimension_request request;
    request.dimensions.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_origin_dimension(data);
        if (!e) return std::unexpected(e.error());
        request.dimensions.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_origin_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_origin_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_origin_dimension_response, error_code>
save_origin_dimension_response::deserialize(std::span<const std::byte> data) {
    save_origin_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_origin_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_origin_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_origin_dimension_request, error_code>
delete_origin_dimension_request::deserialize(std::span<const std::byte> data) {
    delete_origin_dimension_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_origin_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_origin_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_origin_dimension_response, error_code>
delete_origin_dimension_response::deserialize(std::span<const std::byte> data) {
    delete_origin_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_origin_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_origin_dimension_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_origin_dimension_history_request, error_code>
get_origin_dimension_history_request::deserialize(std::span<const std::byte> data) {
    get_origin_dimension_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_origin_dimension_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_origin_dimension_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_origin_dimension(buffer, v);
    }
    return buffer;
}

std::expected<get_origin_dimension_history_response, error_code>
get_origin_dimension_history_response::deserialize(std::span<const std::byte> data) {
    get_origin_dimension_history_response response;

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
        auto dim_result = read_origin_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.versions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_origin_dimension_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Treatment Dimension Messages Implementation
// ============================================================================

std::vector<std::byte> get_treatment_dimensions_request::serialize() const {
    return {};
}

std::expected<get_treatment_dimensions_request, error_code>
get_treatment_dimensions_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_treatment_dimensions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_treatment_dimensions_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_treatment_dimensions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& t : dimensions) {
        write_treatment_dimension(buffer, t);
    }
    return buffer;
}

std::expected<get_treatment_dimensions_response, error_code>
get_treatment_dimensions_response::deserialize(std::span<const std::byte> data) {
    get_treatment_dimensions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.dimensions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto dim_result = read_treatment_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.dimensions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_treatment_dimensions_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_treatment_dimension_request
save_treatment_dimension_request::from(domain::treatment_dimension dimension) {
    return save_treatment_dimension_request{
        std::vector<domain::treatment_dimension>{std::move(dimension)}};
}

save_treatment_dimension_request
save_treatment_dimension_request::from(std::vector<domain::treatment_dimension> dimensions) {
    return save_treatment_dimension_request{std::move(dimensions)};
}

std::vector<std::byte> save_treatment_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dimensions.size()));
    for (const auto& e : dimensions)
        write_treatment_dimension(buffer, e);
    return buffer;
}

std::expected<save_treatment_dimension_request, error_code>
save_treatment_dimension_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    save_treatment_dimension_request request;
    request.dimensions.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_treatment_dimension(data);
        if (!e) return std::unexpected(e.error());
        request.dimensions.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_treatment_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_treatment_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_treatment_dimension_response, error_code>
save_treatment_dimension_response::deserialize(std::span<const std::byte> data) {
    save_treatment_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_treatment_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_treatment_dimension_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(codes.size()));
    for (const auto& code : codes) {
        writer::write_string(buffer, code);
    }
    return buffer;
}

std::expected<delete_treatment_dimension_request, error_code>
delete_treatment_dimension_request::deserialize(std::span<const std::byte> data) {
    delete_treatment_dimension_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_treatment_dimension_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_treatment_dimension_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_treatment_dimension_response, error_code>
delete_treatment_dimension_response::deserialize(std::span<const std::byte> data) {
    delete_treatment_dimension_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_treatment_dimension_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_treatment_dimension_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, code);
    return buffer;
}

std::expected<get_treatment_dimension_history_request, error_code>
get_treatment_dimension_history_request::deserialize(std::span<const std::byte> data) {
    get_treatment_dimension_history_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_treatment_dimension_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_treatment_dimension_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_treatment_dimension(buffer, v);
    }
    return buffer;
}

std::expected<get_treatment_dimension_history_response, error_code>
get_treatment_dimension_history_response::deserialize(std::span<const std::byte> data) {
    get_treatment_dimension_history_response response;

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
        auto dim_result = read_treatment_dimension(data);
        if (!dim_result) return std::unexpected(dim_result.error());
        response.versions.push_back(std::move(*dim_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_treatment_dimension_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
