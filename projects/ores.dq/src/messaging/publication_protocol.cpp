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
#include "ores.dq/messaging/publication_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::dq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Publication result helpers
// ============================================================================

void write_publication_result(std::vector<std::byte>& buffer,
    const domain::publication_result& r) {
    writer::write_uuid(buffer, r.dataset_id);
    writer::write_string(buffer, r.dataset_code);
    writer::write_string(buffer, r.dataset_name);
    writer::write_string(buffer, r.target_table);
    writer::write_uint64(buffer, r.records_inserted);
    writer::write_uint64(buffer, r.records_skipped);
    writer::write_uint64(buffer, r.records_deleted);
    writer::write_bool(buffer, r.success);
    writer::write_string(buffer, r.error_message);
}

std::expected<domain::publication_result, error_code>
read_publication_result(std::span<const std::byte>& data) {
    domain::publication_result r;

    auto dataset_id_result = reader::read_uuid(data);
    if (!dataset_id_result) return std::unexpected(dataset_id_result.error());
    r.dataset_id = *dataset_id_result;

    auto dataset_code_result = reader::read_string(data);
    if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
    r.dataset_code = *dataset_code_result;

    auto dataset_name_result = reader::read_string(data);
    if (!dataset_name_result) return std::unexpected(dataset_name_result.error());
    r.dataset_name = *dataset_name_result;

    auto target_table_result = reader::read_string(data);
    if (!target_table_result) return std::unexpected(target_table_result.error());
    r.target_table = *target_table_result;

    auto records_inserted_result = reader::read_uint64(data);
    if (!records_inserted_result) return std::unexpected(records_inserted_result.error());
    r.records_inserted = *records_inserted_result;

    auto records_skipped_result = reader::read_uint64(data);
    if (!records_skipped_result) return std::unexpected(records_skipped_result.error());
    r.records_skipped = *records_skipped_result;

    auto records_deleted_result = reader::read_uint64(data);
    if (!records_deleted_result) return std::unexpected(records_deleted_result.error());
    r.records_deleted = *records_deleted_result;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    r.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    r.error_message = *error_message_result;

    return r;
}

} // anonymous namespace

// ============================================================================
// publish_datasets_request
// ============================================================================

std::vector<std::byte> publish_datasets_request::serialize() const {
    std::vector<std::byte> buffer;

    // Write dataset IDs
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dataset_ids.size()));
    for (const auto& id : dataset_ids) {
        writer::write_uuid(buffer, id);
    }

    // Write mode as uint8
    writer::write_uint8(buffer, static_cast<std::uint8_t>(mode));

    // Write published_by
    writer::write_string(buffer, published_by);

    // Write resolve_dependencies
    writer::write_bool(buffer, resolve_dependencies);

    return buffer;
}

std::expected<publish_datasets_request, error_code>
publish_datasets_request::deserialize(std::span<const std::byte> data) {
    publish_datasets_request request;

    // Read dataset IDs count
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    // Read dataset IDs
    request.dataset_ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.dataset_ids.push_back(*id_result);
    }

    // Read mode
    auto mode_result = reader::read_uint8(data);
    if (!mode_result) return std::unexpected(mode_result.error());
    request.mode = static_cast<domain::publication_mode>(*mode_result);

    // Read published_by
    auto published_by_result = reader::read_string(data);
    if (!published_by_result) return std::unexpected(published_by_result.error());
    request.published_by = *published_by_result;

    // Read resolve_dependencies
    auto resolve_deps_result = reader::read_bool(data);
    if (!resolve_deps_result) return std::unexpected(resolve_deps_result.error());
    request.resolve_dependencies = *resolve_deps_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const publish_datasets_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// publish_datasets_response
// ============================================================================

std::vector<std::byte> publish_datasets_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write results count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));

    // Write each result
    for (const auto& result : results) {
        write_publication_result(buffer, result);
    }

    return buffer;
}

std::expected<publish_datasets_response, error_code>
publish_datasets_response::deserialize(std::span<const std::byte> data) {
    publish_datasets_response response;

    // Read results count
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    // Read each result
    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_publication_result(data);
        if (!result) return std::unexpected(result.error());
        response.results.push_back(*result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const publish_datasets_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
