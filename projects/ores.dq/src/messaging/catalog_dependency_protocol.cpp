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
#include "ores.dq/messaging/catalog_dependency_protocol.hpp"

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

void write_catalog_dependency(std::vector<std::byte>& buffer,
    const domain::catalog_dependency& d) {
    writer::write_string(buffer, d.catalog_name);
    writer::write_string(buffer, d.dependency_name);
    writer::write_string(buffer, d.recorded_by);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.recorded_at));
}

std::expected<domain::catalog_dependency, error_code>
read_catalog_dependency(std::span<const std::byte>& data) {
    domain::catalog_dependency d;

    auto catalog_name_result = reader::read_string(data);
    if (!catalog_name_result) return std::unexpected(catalog_name_result.error());
    d.catalog_name = *catalog_name_result;

    auto dependency_name_result = reader::read_string(data);
    if (!dependency_name_result) return std::unexpected(dependency_name_result.error());
    d.dependency_name = *dependency_name_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    d.recorded_by = *recorded_by_result;

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
// get_catalog_dependencies_request
// ============================================================================

std::vector<std::byte> get_catalog_dependencies_request::serialize() const {
    return {};
}

std::expected<get_catalog_dependencies_request, error_code>
get_catalog_dependencies_request::deserialize(std::span<const std::byte> /*data*/) {
    return get_catalog_dependencies_request{};
}

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_catalog_dependencies_response
// ============================================================================

std::vector<std::byte> get_catalog_dependencies_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dependencies.size()));
    for (const auto& d : dependencies) {
        write_catalog_dependency(buffer, d);
    }
    return buffer;
}

std::expected<get_catalog_dependencies_response, error_code>
get_catalog_dependencies_response::deserialize(std::span<const std::byte> data) {
    get_catalog_dependencies_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    response.dependencies.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto dep_result = read_catalog_dependency(data);
        if (!dep_result) return std::unexpected(dep_result.error());
        response.dependencies.push_back(std::move(*dep_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_catalog_dependencies_by_catalog_request
// ============================================================================

std::vector<std::byte> get_catalog_dependencies_by_catalog_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, catalog_name);
    return buffer;
}

std::expected<get_catalog_dependencies_by_catalog_request, error_code>
get_catalog_dependencies_by_catalog_request::deserialize(std::span<const std::byte> data) {
    get_catalog_dependencies_by_catalog_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.catalog_name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_by_catalog_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_catalog_dependencies_by_catalog_response
// ============================================================================

std::vector<std::byte> get_catalog_dependencies_by_catalog_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dependencies.size()));
    for (const auto& d : dependencies) {
        write_catalog_dependency(buffer, d);
    }
    return buffer;
}

std::expected<get_catalog_dependencies_by_catalog_response, error_code>
get_catalog_dependencies_by_catalog_response::deserialize(std::span<const std::byte> data) {
    get_catalog_dependencies_by_catalog_response response;

    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    response.dependencies.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto dep_result = read_catalog_dependency(data);
        if (!dep_result) return std::unexpected(dep_result.error());
        response.dependencies.push_back(std::move(*dep_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_catalog_dependencies_by_catalog_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
