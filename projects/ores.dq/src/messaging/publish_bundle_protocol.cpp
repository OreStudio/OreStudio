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
#include "ores.dq/messaging/publish_bundle_protocol.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::dq::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

// ============================================================================
// publish_bundle_params
// ============================================================================

std::string build_params_json(const publish_bundle_params& params) {
    return rfl::json::write(params);
}

// ============================================================================
// bundle_dataset_result
// ============================================================================

std::vector<std::byte> bundle_dataset_result::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, dataset_code);
    writer::write_string(buffer, dataset_name);
    writer::write_string(buffer, status);
    writer::write_uint64(buffer, records_inserted);
    writer::write_uint64(buffer, records_updated);
    writer::write_uint64(buffer, records_skipped);
    writer::write_uint64(buffer, records_deleted);
    writer::write_string(buffer, error_message);
    return buffer;
}

std::expected<bundle_dataset_result, error_code>
bundle_dataset_result::deserialize(std::span<const std::byte>& data) {
    bundle_dataset_result result;

    auto dataset_code_result = reader::read_string(data);
    if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
    result.dataset_code = *dataset_code_result;

    auto dataset_name_result = reader::read_string(data);
    if (!dataset_name_result) return std::unexpected(dataset_name_result.error());
    result.dataset_name = *dataset_name_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    result.status = *status_result;

    auto records_inserted_result = reader::read_uint64(data);
    if (!records_inserted_result) return std::unexpected(records_inserted_result.error());
    result.records_inserted = *records_inserted_result;

    auto records_updated_result = reader::read_uint64(data);
    if (!records_updated_result) return std::unexpected(records_updated_result.error());
    result.records_updated = *records_updated_result;

    auto records_skipped_result = reader::read_uint64(data);
    if (!records_skipped_result) return std::unexpected(records_skipped_result.error());
    result.records_skipped = *records_skipped_result;

    auto records_deleted_result = reader::read_uint64(data);
    if (!records_deleted_result) return std::unexpected(records_deleted_result.error());
    result.records_deleted = *records_deleted_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    result.error_message = *error_message_result;

    return result;
}

std::ostream& operator<<(std::ostream& s, const bundle_dataset_result& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// publish_bundle_request
// ============================================================================

std::vector<std::byte> publish_bundle_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, bundle_code);
    writer::write_uint8(buffer, static_cast<std::uint8_t>(mode));
    writer::write_string(buffer, published_by);
    writer::write_bool(buffer, atomic);
    writer::write_string(buffer, params_json);
    writer::write_string(buffer, target_tenant_id);
    return buffer;
}

std::expected<publish_bundle_request, error_code>
publish_bundle_request::deserialize(std::span<const std::byte> data) {
    publish_bundle_request request;

    auto bundle_code_result = reader::read_string(data);
    if (!bundle_code_result) return std::unexpected(bundle_code_result.error());
    request.bundle_code = *bundle_code_result;

    auto mode_result = reader::read_uint8(data);
    if (!mode_result) return std::unexpected(mode_result.error());
    request.mode = static_cast<domain::publication_mode>(*mode_result);

    auto published_by_result = reader::read_string(data);
    if (!published_by_result) return std::unexpected(published_by_result.error());
    request.published_by = *published_by_result;

    auto atomic_result = reader::read_bool(data);
    if (!atomic_result) return std::unexpected(atomic_result.error());
    request.atomic = *atomic_result;

    auto params_json_result = reader::read_string(data);
    if (!params_json_result) return std::unexpected(params_json_result.error());
    request.params_json = *params_json_result;

    // Handle backward compatibility: target_tenant_id was added later
    if (!data.empty()) {
        auto target_tenant_id_result = reader::read_string(data);
        if (!target_tenant_id_result) return std::unexpected(target_tenant_id_result.error());
        request.target_tenant_id = *target_tenant_id_result;
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const publish_bundle_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// publish_bundle_response
// ============================================================================

std::vector<std::byte> publish_bundle_response::serialize() const {
    std::vector<std::byte> buffer;

    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    writer::write_uint32(buffer, datasets_processed);
    writer::write_uint32(buffer, datasets_succeeded);
    writer::write_uint32(buffer, datasets_failed);
    writer::write_uint32(buffer, datasets_skipped);
    writer::write_uint64(buffer, total_records_inserted);
    writer::write_uint64(buffer, total_records_updated);
    writer::write_uint64(buffer, total_records_skipped);
    writer::write_uint64(buffer, total_records_deleted);

    // Write dataset results
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dataset_results.size()));
    for (const auto& result : dataset_results) {
        auto result_bytes = result.serialize();
        buffer.insert(buffer.end(), result_bytes.begin(), result_bytes.end());
    }

    return buffer;
}

std::expected<publish_bundle_response, error_code>
publish_bundle_response::deserialize(std::span<const std::byte> data) {
    publish_bundle_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto datasets_processed_result = reader::read_uint32(data);
    if (!datasets_processed_result) return std::unexpected(datasets_processed_result.error());
    response.datasets_processed = *datasets_processed_result;

    auto datasets_succeeded_result = reader::read_uint32(data);
    if (!datasets_succeeded_result) return std::unexpected(datasets_succeeded_result.error());
    response.datasets_succeeded = *datasets_succeeded_result;

    auto datasets_failed_result = reader::read_uint32(data);
    if (!datasets_failed_result) return std::unexpected(datasets_failed_result.error());
    response.datasets_failed = *datasets_failed_result;

    auto datasets_skipped_result = reader::read_uint32(data);
    if (!datasets_skipped_result) return std::unexpected(datasets_skipped_result.error());
    response.datasets_skipped = *datasets_skipped_result;

    auto total_records_inserted_result = reader::read_uint64(data);
    if (!total_records_inserted_result) return std::unexpected(total_records_inserted_result.error());
    response.total_records_inserted = *total_records_inserted_result;

    auto total_records_updated_result = reader::read_uint64(data);
    if (!total_records_updated_result) return std::unexpected(total_records_updated_result.error());
    response.total_records_updated = *total_records_updated_result;

    auto total_records_skipped_result = reader::read_uint64(data);
    if (!total_records_skipped_result) return std::unexpected(total_records_skipped_result.error());
    response.total_records_skipped = *total_records_skipped_result;

    auto total_records_deleted_result = reader::read_uint64(data);
    if (!total_records_deleted_result) return std::unexpected(total_records_deleted_result.error());
    response.total_records_deleted = *total_records_deleted_result;

    // Read dataset results
    auto results_count_result = reader::read_uint32(data);
    if (!results_count_result) return std::unexpected(results_count_result.error());
    const auto results_count = *results_count_result;

    response.dataset_results.reserve(results_count);
    for (std::uint32_t i = 0; i < results_count; ++i) {
        auto result = bundle_dataset_result::deserialize(data);
        if (!result) return std::unexpected(result.error());
        response.dataset_results.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const publish_bundle_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
