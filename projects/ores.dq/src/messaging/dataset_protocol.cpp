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
#include "ores.dq/messaging/dataset_protocol.hpp"

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

// ============================================================================
// Dataset Serialization Helpers
// ============================================================================

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

namespace {

// ============================================================================
// Methodology helpers
// ============================================================================

void write_methodology(std::vector<std::byte>& buffer, const domain::methodology& m) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(m.version));
    writer::write_uuid(buffer, m.id);
    writer::write_string(buffer, m.name);
    writer::write_string(buffer, m.description);

    writer::write_bool(buffer, m.logic_reference.has_value());
    if (m.logic_reference.has_value()) {
        writer::write_string(buffer, *m.logic_reference);
    }

    writer::write_bool(buffer, m.implementation_details.has_value());
    if (m.implementation_details.has_value()) {
        writer::write_string(buffer, *m.implementation_details);
    }

    writer::write_string(buffer, m.recorded_by);
    writer::write_string(buffer, m.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(m.recorded_at));
}

std::expected<domain::methodology, error_code>
read_methodology(std::span<const std::byte>& data) {
    domain::methodology m;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    m.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    m.id = *id_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    m.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    m.description = *description_result;

    auto has_logic_reference_result = reader::read_bool(data);
    if (!has_logic_reference_result) return std::unexpected(has_logic_reference_result.error());
    if (*has_logic_reference_result) {
        auto logic_reference_result = reader::read_string(data);
        if (!logic_reference_result) return std::unexpected(logic_reference_result.error());
        m.logic_reference = *logic_reference_result;
    }

    auto has_implementation_details_result = reader::read_bool(data);
    if (!has_implementation_details_result) return std::unexpected(has_implementation_details_result.error());
    if (*has_implementation_details_result) {
        auto implementation_details_result = reader::read_string(data);
        if (!implementation_details_result) return std::unexpected(implementation_details_result.error());
        m.implementation_details = *implementation_details_result;
    }

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    m.recorded_by = *recorded_by_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    m.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        m.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return m;
}

} // anonymous namespace

// ============================================================================
// Dataset Messages Implementation
// ============================================================================

std::vector<std::byte> get_datasets_request::serialize() const {
    return {};
}

std::expected<get_datasets_request, error_code>
get_datasets_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_datasets_request{};
}

std::ostream& operator<<(std::ostream& s, const get_datasets_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_datasets_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(datasets.size()));
    for (const auto& d : datasets) {
        write_dataset(buffer, d);
    }
    return buffer;
}

std::expected<get_datasets_response, error_code>
get_datasets_response::deserialize(std::span<const std::byte> data) {
    get_datasets_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.datasets.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto dataset_result = read_dataset(data);
        if (!dataset_result) return std::unexpected(dataset_result.error());
        response.datasets.push_back(std::move(*dataset_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_datasets_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_dataset_request::serialize() const {
    std::vector<std::byte> buffer;
    write_dataset(buffer, dataset);
    return buffer;
}

std::expected<save_dataset_request, error_code>
save_dataset_request::deserialize(std::span<const std::byte> data) {
    save_dataset_request request;

    auto dataset_result = read_dataset(data);
    if (!dataset_result) return std::unexpected(dataset_result.error());
    request.dataset = std::move(*dataset_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_dataset_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_dataset_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_dataset_response, error_code>
save_dataset_response::deserialize(std::span<const std::byte> data) {
    save_dataset_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_dataset_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_dataset_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_dataset_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_dataset_request, error_code>
delete_dataset_request::deserialize(std::span<const std::byte> data) {
    delete_dataset_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_dataset_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_dataset_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_dataset_response, error_code>
delete_dataset_response::deserialize(std::span<const std::byte> data) {
    delete_dataset_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_dataset_result r;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        r.id = *id_result;

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

std::ostream& operator<<(std::ostream& s, const delete_dataset_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_dataset_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_dataset_history_request, error_code>
get_dataset_history_request::deserialize(std::span<const std::byte> data) {
    get_dataset_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_dataset_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_dataset(buffer, v);
    }
    return buffer;
}

std::expected<get_dataset_history_response, error_code>
get_dataset_history_response::deserialize(std::span<const std::byte> data) {
    get_dataset_history_response response;

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
        auto dataset_result = read_dataset(data);
        if (!dataset_result) return std::unexpected(dataset_result.error());
        response.versions.push_back(std::move(*dataset_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_dataset_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Methodology Messages Implementation
// ============================================================================

std::vector<std::byte> get_methodologies_request::serialize() const {
    return {};
}

std::expected<get_methodologies_request, error_code>
get_methodologies_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_methodologies_request{};
}

std::ostream& operator<<(std::ostream& s, const get_methodologies_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_methodologies_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(methodologies.size()));
    for (const auto& m : methodologies) {
        write_methodology(buffer, m);
    }
    return buffer;
}

std::expected<get_methodologies_response, error_code>
get_methodologies_response::deserialize(std::span<const std::byte> data) {
    get_methodologies_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.methodologies.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto methodology_result = read_methodology(data);
        if (!methodology_result) return std::unexpected(methodology_result.error());
        response.methodologies.push_back(std::move(*methodology_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_methodologies_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_methodology_request::serialize() const {
    std::vector<std::byte> buffer;
    write_methodology(buffer, methodology);
    return buffer;
}

std::expected<save_methodology_request, error_code>
save_methodology_request::deserialize(std::span<const std::byte> data) {
    save_methodology_request request;

    auto methodology_result = read_methodology(data);
    if (!methodology_result) return std::unexpected(methodology_result.error());
    request.methodology = std::move(*methodology_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_methodology_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_methodology_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_methodology_response, error_code>
save_methodology_response::deserialize(std::span<const std::byte> data) {
    save_methodology_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_methodology_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_methodology_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_methodology_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_methodology_request, error_code>
delete_methodology_request::deserialize(std::span<const std::byte> data) {
    delete_methodology_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.ids.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        request.ids.push_back(*id_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_methodology_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_methodology_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_methodology_response, error_code>
delete_methodology_response::deserialize(std::span<const std::byte> data) {
    delete_methodology_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_methodology_result r;

        auto id_result = reader::read_uuid(data);
        if (!id_result) return std::unexpected(id_result.error());
        r.id = *id_result;

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

std::ostream& operator<<(std::ostream& s, const delete_methodology_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_methodology_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_methodology_history_request, error_code>
get_methodology_history_request::deserialize(std::span<const std::byte> data) {
    get_methodology_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_methodology_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_methodology_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_methodology(buffer, v);
    }
    return buffer;
}

std::expected<get_methodology_history_response, error_code>
get_methodology_history_response::deserialize(std::span<const std::byte> data) {
    get_methodology_history_response response;

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
        auto methodology_result = read_methodology(data);
        if (!methodology_result) return std::unexpected(methodology_result.error());
        response.versions.push_back(std::move(*methodology_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_methodology_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
