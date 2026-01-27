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
// Publication result helpers
// ============================================================================

void write_publication_result(std::vector<std::byte>& buffer,
    const domain::publication_result& r) {
    writer::write_uuid(buffer, r.dataset_id);
    writer::write_string(buffer, r.dataset_code);
    writer::write_string(buffer, r.dataset_name);
    writer::write_string(buffer, r.target_table);
    writer::write_uint64(buffer, r.records_inserted);
    writer::write_uint64(buffer, r.records_updated);
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

    auto records_updated_result = reader::read_uint64(data);
    if (!records_updated_result) return std::unexpected(records_updated_result.error());
    r.records_updated = *records_updated_result;

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

    // Write atomic
    writer::write_bool(buffer, atomic);

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

    // Read atomic
    auto atomic_result = reader::read_bool(data);
    if (!atomic_result) return std::unexpected(atomic_result.error());
    request.atomic = *atomic_result;

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

// ============================================================================
// get_publications_request
// ============================================================================

std::vector<std::byte> get_publications_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, dataset_id);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_publications_request, error_code>
get_publications_request::deserialize(std::span<const std::byte> data) {
    get_publications_request request;

    auto dataset_id_result = reader::read_uuid(data);
    if (!dataset_id_result) return std::unexpected(dataset_id_result.error());
    request.dataset_id = *dataset_id_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_publications_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_publications_response
// ============================================================================

namespace {

void write_publication(std::vector<std::byte>& buffer,
    const domain::publication& p) {
    writer::write_uuid(buffer, p.id);
    writer::write_uuid(buffer, p.dataset_id);
    writer::write_string(buffer, p.dataset_code);
    writer::write_uint8(buffer, static_cast<std::uint8_t>(p.mode));
    writer::write_string(buffer, p.target_table);
    writer::write_uint64(buffer, p.records_inserted);
    writer::write_uint64(buffer, p.records_updated);
    writer::write_uint64(buffer, p.records_skipped);
    writer::write_uint64(buffer, p.records_deleted);
    writer::write_string(buffer, p.published_by);

    // Write published_at as milliseconds since epoch
    const auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        p.published_at.time_since_epoch()).count();
    writer::write_int64(buffer, ms);
}

std::expected<domain::publication, error_code>
read_publication(std::span<const std::byte>& data) {
    domain::publication p;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    p.id = *id_result;

    auto dataset_id_result = reader::read_uuid(data);
    if (!dataset_id_result) return std::unexpected(dataset_id_result.error());
    p.dataset_id = *dataset_id_result;

    auto dataset_code_result = reader::read_string(data);
    if (!dataset_code_result) return std::unexpected(dataset_code_result.error());
    p.dataset_code = *dataset_code_result;

    auto mode_result = reader::read_uint8(data);
    if (!mode_result) return std::unexpected(mode_result.error());
    p.mode = static_cast<domain::publication_mode>(*mode_result);

    auto target_table_result = reader::read_string(data);
    if (!target_table_result) return std::unexpected(target_table_result.error());
    p.target_table = *target_table_result;

    auto records_inserted_result = reader::read_uint64(data);
    if (!records_inserted_result) return std::unexpected(records_inserted_result.error());
    p.records_inserted = *records_inserted_result;

    auto records_updated_result = reader::read_uint64(data);
    if (!records_updated_result) return std::unexpected(records_updated_result.error());
    p.records_updated = *records_updated_result;

    auto records_skipped_result = reader::read_uint64(data);
    if (!records_skipped_result) return std::unexpected(records_skipped_result.error());
    p.records_skipped = *records_skipped_result;

    auto records_deleted_result = reader::read_uint64(data);
    if (!records_deleted_result) return std::unexpected(records_deleted_result.error());
    p.records_deleted = *records_deleted_result;

    auto published_by_result = reader::read_string(data);
    if (!published_by_result) return std::unexpected(published_by_result.error());
    p.published_by = *published_by_result;

    auto published_at_result = reader::read_int64(data);
    if (!published_at_result) return std::unexpected(published_at_result.error());
    p.published_at = std::chrono::system_clock::time_point{
        std::chrono::milliseconds{*published_at_result}};

    return p;
}

} // anonymous namespace

std::vector<std::byte> get_publications_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write publications count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(publications.size()));

    // Write each publication
    for (const auto& pub : publications) {
        write_publication(buffer, pub);
    }

    return buffer;
}

std::expected<get_publications_response, error_code>
get_publications_response::deserialize(std::span<const std::byte> data) {
    get_publications_response response;

    // Read publications count
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    const auto count = *count_result;

    // Read each publication
    response.publications.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_publication(data);
        if (!result) return std::unexpected(result.error());
        response.publications.push_back(*result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_publications_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// resolve_dependencies_request
// ============================================================================

std::vector<std::byte> resolve_dependencies_request::serialize() const {
    std::vector<std::byte> buffer;

    // Write dataset IDs
    writer::write_uint32(buffer, static_cast<std::uint32_t>(dataset_ids.size()));
    for (const auto& id : dataset_ids) {
        writer::write_uuid(buffer, id);
    }

    return buffer;
}

std::expected<resolve_dependencies_request, error_code>
resolve_dependencies_request::deserialize(std::span<const std::byte> data) {
    resolve_dependencies_request request;

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

    return request;
}

std::ostream& operator<<(std::ostream& s, const resolve_dependencies_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// resolve_dependencies_response
// ============================================================================

namespace {

// Dataset serialization helpers - matches dataset_protocol.cpp format
void write_dataset(std::vector<std::byte>& buffer, const domain::dataset& d) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(d.version));
    writer::write_uuid(buffer, d.id);
    writer::write_string(buffer, d.code);

    writer::write_bool(buffer, d.catalog_name.has_value());
    if (d.catalog_name.has_value()) {
        writer::write_string(buffer, *d.catalog_name);
    }

    writer::write_string(buffer, d.subject_area_name);
    writer::write_string(buffer, d.domain_name);

    writer::write_bool(buffer, d.coding_scheme_code.has_value());
    if (d.coding_scheme_code.has_value()) {
        writer::write_string(buffer, *d.coding_scheme_code);
    }

    writer::write_string(buffer, d.origin_code);
    writer::write_string(buffer, d.nature_code);
    writer::write_string(buffer, d.treatment_code);

    writer::write_bool(buffer, d.methodology_id.has_value());
    if (d.methodology_id.has_value()) {
        writer::write_uuid(buffer, *d.methodology_id);
    }

    writer::write_string(buffer, d.name);
    writer::write_string(buffer, d.description);
    writer::write_string(buffer, d.source_system_id);
    writer::write_string(buffer, d.business_context);

    writer::write_bool(buffer, d.upstream_derivation_id.has_value());
    if (d.upstream_derivation_id.has_value()) {
        writer::write_uuid(buffer, *d.upstream_derivation_id);
    }

    writer::write_uint32(buffer, static_cast<std::uint32_t>(d.lineage_depth));
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.as_of_date));
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.ingestion_timestamp));

    writer::write_bool(buffer, d.license_info.has_value());
    if (d.license_info.has_value()) {
        writer::write_string(buffer, *d.license_info);
    }

    writer::write_bool(buffer, d.artefact_type.has_value());
    if (d.artefact_type.has_value()) {
        writer::write_string(buffer, *d.artefact_type);
    }

    writer::write_string(buffer, d.recorded_by);
    writer::write_string(buffer, d.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.recorded_at));
}

std::expected<domain::dataset, error_code>
read_dataset(std::span<const std::byte>& data) {
    domain::dataset d;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    d.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    d.id = *id_result;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    d.code = *code_result;

    auto has_catalog_name_result = reader::read_bool(data);
    if (!has_catalog_name_result) return std::unexpected(has_catalog_name_result.error());
    if (*has_catalog_name_result) {
        auto catalog_name_result = reader::read_string(data);
        if (!catalog_name_result) return std::unexpected(catalog_name_result.error());
        d.catalog_name = *catalog_name_result;
    }

    auto subject_area_name_result = reader::read_string(data);
    if (!subject_area_name_result) return std::unexpected(subject_area_name_result.error());
    d.subject_area_name = *subject_area_name_result;

    auto domain_name_result = reader::read_string(data);
    if (!domain_name_result) return std::unexpected(domain_name_result.error());
    d.domain_name = *domain_name_result;

    auto has_coding_scheme_code_result = reader::read_bool(data);
    if (!has_coding_scheme_code_result) return std::unexpected(has_coding_scheme_code_result.error());
    if (*has_coding_scheme_code_result) {
        auto coding_scheme_code_result = reader::read_string(data);
        if (!coding_scheme_code_result) return std::unexpected(coding_scheme_code_result.error());
        d.coding_scheme_code = *coding_scheme_code_result;
    }

    auto origin_code_result = reader::read_string(data);
    if (!origin_code_result) return std::unexpected(origin_code_result.error());
    d.origin_code = *origin_code_result;

    auto nature_code_result = reader::read_string(data);
    if (!nature_code_result) return std::unexpected(nature_code_result.error());
    d.nature_code = *nature_code_result;

    auto treatment_code_result = reader::read_string(data);
    if (!treatment_code_result) return std::unexpected(treatment_code_result.error());
    d.treatment_code = *treatment_code_result;

    auto has_methodology_id_result = reader::read_bool(data);
    if (!has_methodology_id_result) return std::unexpected(has_methodology_id_result.error());
    if (*has_methodology_id_result) {
        auto methodology_id_result = reader::read_uuid(data);
        if (!methodology_id_result) return std::unexpected(methodology_id_result.error());
        d.methodology_id = *methodology_id_result;
    }

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    d.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    d.description = *description_result;

    auto source_system_id_result = reader::read_string(data);
    if (!source_system_id_result) return std::unexpected(source_system_id_result.error());
    d.source_system_id = *source_system_id_result;

    auto business_context_result = reader::read_string(data);
    if (!business_context_result) return std::unexpected(business_context_result.error());
    d.business_context = *business_context_result;

    auto has_upstream_derivation_id_result = reader::read_bool(data);
    if (!has_upstream_derivation_id_result) return std::unexpected(has_upstream_derivation_id_result.error());
    if (*has_upstream_derivation_id_result) {
        auto upstream_derivation_id_result = reader::read_uuid(data);
        if (!upstream_derivation_id_result) return std::unexpected(upstream_derivation_id_result.error());
        d.upstream_derivation_id = *upstream_derivation_id_result;
    }

    auto lineage_depth_result = reader::read_uint32(data);
    if (!lineage_depth_result) return std::unexpected(lineage_depth_result.error());
    d.lineage_depth = static_cast<int>(*lineage_depth_result);

    auto as_of_date_result = reader::read_string(data);
    if (!as_of_date_result) return std::unexpected(as_of_date_result.error());
    try {
        d.as_of_date = ores::platform::time::datetime::parse_time_point(*as_of_date_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto ingestion_timestamp_result = reader::read_string(data);
    if (!ingestion_timestamp_result) return std::unexpected(ingestion_timestamp_result.error());
    try {
        d.ingestion_timestamp = ores::platform::time::datetime::parse_time_point(*ingestion_timestamp_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto has_license_info_result = reader::read_bool(data);
    if (!has_license_info_result) return std::unexpected(has_license_info_result.error());
    if (*has_license_info_result) {
        auto license_info_result = reader::read_string(data);
        if (!license_info_result) return std::unexpected(license_info_result.error());
        d.license_info = *license_info_result;
    }

    auto has_artefact_type_result = reader::read_bool(data);
    if (!has_artefact_type_result) return std::unexpected(has_artefact_type_result.error());
    if (*has_artefact_type_result) {
        auto artefact_type_result = reader::read_string(data);
        if (!artefact_type_result) return std::unexpected(artefact_type_result.error());
        d.artefact_type = *artefact_type_result;
    }

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    d.recorded_by = *recorded_by_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    d.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        d.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return d;
}

} // anonymous namespace

std::vector<std::byte> resolve_dependencies_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write datasets count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(datasets.size()));

    // Write each dataset
    for (const auto& dataset : datasets) {
        write_dataset(buffer, dataset);
    }

    // Write requested_ids count
    writer::write_uint32(buffer, static_cast<std::uint32_t>(requested_ids.size()));

    // Write each requested ID
    for (const auto& id : requested_ids) {
        writer::write_uuid(buffer, id);
    }

    return buffer;
}

std::expected<resolve_dependencies_response, error_code>
resolve_dependencies_response::deserialize(std::span<const std::byte> data) {
    resolve_dependencies_response response;

    // Read datasets count
    auto datasets_count_result = reader::read_uint32(data);
    if (!datasets_count_result) return std::unexpected(datasets_count_result.error());
    const auto datasets_count = *datasets_count_result;

    // Read each dataset
    response.datasets.reserve(datasets_count);
    for (std::uint32_t i = 0; i < datasets_count; ++i) {
        auto dataset_result = read_dataset(data);
        if (!dataset_result) return std::unexpected(dataset_result.error());
        response.datasets.push_back(std::move(*dataset_result));
    }

    // Read requested_ids count
    auto ids_count_result = reader::read_uint32(data);
    if (!ids_count_result) return std::unexpected(ids_count_result.error());
    const auto ids_count = *ids_count_result;

    // Read each requested ID
    response.requested_ids.reserve(ids_count);
    for (std::uint32_t i = 0; i < ids_count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        response.requested_ids.push_back(*id_result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const resolve_dependencies_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
