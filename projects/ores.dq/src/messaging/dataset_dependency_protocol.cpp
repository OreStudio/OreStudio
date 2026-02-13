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
#include "ores.dq/messaging/dataset_dependency_protocol.hpp"

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

void write_dataset_dependency(std::vector<std::byte>& buffer,
    const domain::dataset_dependency& d) {
    writer::write_string(buffer, d.dataset_code);
    writer::write_string(buffer, d.dependency_code);
    writer::write_string(buffer, d.role);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.recorded_at));
}

std::expected<domain::dataset_dependency, error_code>
read_dataset_dependency(std::span<const std::byte>& data) {
    domain::dataset_dependency d;

    auto dataset_code_result = reader::read_string(data);
    if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
    d.dataset_code = *dataset_code_result;

    auto dependency_code_result = reader::read_string(data);
    if (!dependency_code_result) return std::unexpected(dependency_code_result.error());
    d.dependency_code = *dependency_code_result;

    auto role_result = reader::read_string(data);
    if (!role_result) return std::unexpected(role_result.error());
    d.role = *role_result;


    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        d.recorded_at = ores::platform::time::datetime::parse_time_point(
            *recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return d;
}

}

// ============================================================================
// get_dataset_dependencies_request
// ============================================================================

std::vector<std::byte> get_dataset_dependencies_request::serialize() const {
    return {};
}

std::expected<get_dataset_dependencies_request, error_code>
get_dataset_dependencies_request::deserialize(std::span<const std::byte> /*data*/) {
    return get_dataset_dependencies_request{};
}

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_dataset_dependencies_response
// ============================================================================

std::vector<std::byte> get_dataset_dependencies_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dependencies.size()));
    for (const auto& d : dependencies) {
        write_dataset_dependency(buffer, d);
    }
    return buffer;
}

std::expected<get_dataset_dependencies_response, error_code>
get_dataset_dependencies_response::deserialize(std::span<const std::byte> data) {
    get_dataset_dependencies_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    response.dependencies.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto dep_result = read_dataset_dependency(data);
        if (!dep_result) return std::unexpected(dep_result.error());
        response.dependencies.push_back(std::move(*dep_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_dataset_dependencies_by_dataset_request
// ============================================================================

std::vector<std::byte> get_dataset_dependencies_by_dataset_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, dataset_code);
    return buffer;
}

std::expected<get_dataset_dependencies_by_dataset_request, error_code>
get_dataset_dependencies_by_dataset_request::deserialize(std::span<const std::byte> data) {
    get_dataset_dependencies_by_dataset_request request;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.dataset_code = *code_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_by_dataset_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_dataset_dependencies_by_dataset_response
// ============================================================================

std::vector<std::byte> get_dataset_dependencies_by_dataset_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dependencies.size()));
    for (const auto& d : dependencies) {
        write_dataset_dependency(buffer, d);
    }
    return buffer;
}

std::expected<get_dataset_dependencies_by_dataset_response, error_code>
get_dataset_dependencies_by_dataset_response::deserialize(std::span<const std::byte> data) {
    get_dataset_dependencies_by_dataset_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    response.dependencies.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto dep_result = read_dataset_dependency(data);
        if (!dep_result) return std::unexpected(dep_result.error());
        response.dependencies.push_back(std::move(*dep_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_dependencies_by_dataset_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
