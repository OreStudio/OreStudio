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
#include "ores.dq/messaging/data_organization_protocol.hpp"

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
// Catalog helpers
// ============================================================================

void write_catalog(std::vector<std::byte>& buffer, const domain::catalog& c) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(c.version));
    writer::write_string(buffer, c.name);
    writer::write_string(buffer, c.description);
    writer::write_bool(buffer, c.owner.has_value());
    if (c.owner.has_value()) {
        writer::write_string(buffer, *c.owner);
    }
    writer::write_string(buffer, c.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(c.recorded_at));
}

std::expected<domain::catalog, error_code>
read_catalog(std::span<const std::byte>& data) {
    domain::catalog c;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    c.version = static_cast<int>(*version_result);

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    c.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    c.description = *description_result;

    auto has_owner_result = reader::read_bool(data);
    if (!has_owner_result) return std::unexpected(has_owner_result.error());
    if (*has_owner_result) {
        auto owner_result = reader::read_string(data);
        if (!owner_result) return std::unexpected(owner_result.error());
        c.owner = *owner_result;
    }


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    c.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        c.recorded_at = ores::platform::time::datetime::parse_time_point(
            *recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return c;
}

// ============================================================================
// Data Domain helpers
// ============================================================================

void write_data_domain(std::vector<std::byte>& buffer, const domain::data_domain& d) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(d.version));
    writer::write_string(buffer, d.name);
    writer::write_string(buffer, d.description);
    writer::write_string(buffer, d.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(d.recorded_at));
}

std::expected<domain::data_domain, error_code>
read_data_domain(std::span<const std::byte>& data) {
    domain::data_domain d;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    d.version = static_cast<int>(*version_result);

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    d.name = *name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    d.description = *description_result;


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    d.change_commentary = *change_commentary_result;

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

// ============================================================================
// Subject Area helpers
// ============================================================================

void write_subject_area(std::vector<std::byte>& buffer, const domain::subject_area& s) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(s.version));
    writer::write_string(buffer, s.name);
    writer::write_string(buffer, s.domain_name);
    writer::write_string(buffer, s.description);
    writer::write_string(buffer, s.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(s.recorded_at));
}

std::expected<domain::subject_area, error_code>
read_subject_area(std::span<const std::byte>& data) {
    domain::subject_area s;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    s.version = static_cast<int>(*version_result);

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    s.name = *name_result;

    auto domain_name_result = reader::read_string(data);
    if (!domain_name_result) return std::unexpected(domain_name_result.error());
    s.domain_name = *domain_name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    s.description = *description_result;


    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    s.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        s.recorded_at = ores::platform::time::datetime::parse_time_point(
            *recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return s;
}

void write_subject_area_key(std::vector<std::byte>& buffer, const subject_area_key& k) {
    writer::write_string(buffer, k.name);
    writer::write_string(buffer, k.domain_name);
}

std::expected<subject_area_key, error_code>
read_subject_area_key(std::span<const std::byte>& data) {
    subject_area_key k;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    k.name = *name_result;

    auto domain_name_result = reader::read_string(data);
    if (!domain_name_result) return std::unexpected(domain_name_result.error());
    k.domain_name = *domain_name_result;

    return k;
}

} // anonymous namespace

// ============================================================================
// Catalog Messages Implementation
// ============================================================================

std::vector<std::byte> get_catalogs_request::serialize() const {
    return {};
}

std::expected<get_catalogs_request, error_code>
get_catalogs_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_catalogs_request{};
}

std::ostream& operator<<(std::ostream& s, const get_catalogs_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_catalogs_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(catalogs.size()));
    for (const auto& c : catalogs) {
        write_catalog(buffer, c);
    }
    return buffer;
}

std::expected<get_catalogs_response, error_code>
get_catalogs_response::deserialize(std::span<const std::byte> data) {
    get_catalogs_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.catalogs.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto catalog_result = read_catalog(data);
        if (!catalog_result) return std::unexpected(catalog_result.error());
        response.catalogs.push_back(std::move(*catalog_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_catalogs_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_catalog_request::serialize() const {
    std::vector<std::byte> buffer;
    write_catalog(buffer, catalog);
    return buffer;
}

std::expected<save_catalog_request, error_code>
save_catalog_request::deserialize(std::span<const std::byte> data) {
    save_catalog_request request;

    auto catalog_result = read_catalog(data);
    if (!catalog_result) return std::unexpected(catalog_result.error());
    request.catalog = std::move(*catalog_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_catalog_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_catalog_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_catalog_response, error_code>
save_catalog_response::deserialize(std::span<const std::byte> data) {
    save_catalog_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_catalog_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_catalog_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_catalog_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(names.size()));
    for (const auto& name : names) {
        writer::write_string(buffer, name);
    }
    return buffer;
}

std::expected<delete_catalog_request, error_code>
delete_catalog_request::deserialize(std::span<const std::byte> data) {
    delete_catalog_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.names.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        request.names.push_back(*name_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_catalog_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_catalog_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.name);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_catalog_response, error_code>
delete_catalog_response::deserialize(std::span<const std::byte> data) {
    delete_catalog_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_catalog_result r;

        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        r.name = *name_result;

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

std::ostream& operator<<(std::ostream& s, const delete_catalog_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_catalog_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, name);
    return buffer;
}

std::expected<get_catalog_history_request, error_code>
get_catalog_history_request::deserialize(std::span<const std::byte> data) {
    get_catalog_history_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_catalog_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_catalog_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_catalog(buffer, v);
    }
    return buffer;
}

std::expected<get_catalog_history_response, error_code>
get_catalog_history_response::deserialize(std::span<const std::byte> data) {
    get_catalog_history_response response;

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
        auto catalog_result = read_catalog(data);
        if (!catalog_result) return std::unexpected(catalog_result.error());
        response.versions.push_back(std::move(*catalog_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_catalog_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Data Domain Messages Implementation
// ============================================================================

std::vector<std::byte> get_data_domains_request::serialize() const {
    return {};
}

std::expected<get_data_domains_request, error_code>
get_data_domains_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_data_domains_request{};
}

std::ostream& operator<<(std::ostream& s, const get_data_domains_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_data_domains_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(domains.size()));
    for (const auto& d : domains) {
        write_data_domain(buffer, d);
    }
    return buffer;
}

std::expected<get_data_domains_response, error_code>
get_data_domains_response::deserialize(std::span<const std::byte> data) {
    get_data_domains_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.domains.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto domain_result = read_data_domain(data);
        if (!domain_result) return std::unexpected(domain_result.error());
        response.domains.push_back(std::move(*domain_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_data_domains_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_data_domain_request::serialize() const {
    std::vector<std::byte> buffer;
    write_data_domain(buffer, domain);
    return buffer;
}

std::expected<save_data_domain_request, error_code>
save_data_domain_request::deserialize(std::span<const std::byte> data) {
    save_data_domain_request request;

    auto domain_result = read_data_domain(data);
    if (!domain_result) return std::unexpected(domain_result.error());
    request.domain = std::move(*domain_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_data_domain_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_data_domain_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_data_domain_response, error_code>
save_data_domain_response::deserialize(std::span<const std::byte> data) {
    save_data_domain_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_data_domain_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_data_domain_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_data_domain_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(names.size()));
    for (const auto& name : names) {
        writer::write_string(buffer, name);
    }
    return buffer;
}

std::expected<delete_data_domain_request, error_code>
delete_data_domain_request::deserialize(std::span<const std::byte> data) {
    delete_data_domain_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.names.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        request.names.push_back(*name_result);
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_data_domain_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_data_domain_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_string(buffer, r.name);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_data_domain_response, error_code>
delete_data_domain_response::deserialize(std::span<const std::byte> data) {
    delete_data_domain_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_data_domain_result r;

        auto name_result = reader::read_string(data);
        if (!name_result) return std::unexpected(name_result.error());
        r.name = *name_result;

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

std::ostream& operator<<(std::ostream& s, const delete_data_domain_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_data_domain_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, name);
    return buffer;
}

std::expected<get_data_domain_history_request, error_code>
get_data_domain_history_request::deserialize(std::span<const std::byte> data) {
    get_data_domain_history_request request;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.name = *name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_data_domain_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_data_domain_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_data_domain(buffer, v);
    }
    return buffer;
}

std::expected<get_data_domain_history_response, error_code>
get_data_domain_history_response::deserialize(std::span<const std::byte> data) {
    get_data_domain_history_response response;

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
        auto domain_result = read_data_domain(data);
        if (!domain_result) return std::unexpected(domain_result.error());
        response.versions.push_back(std::move(*domain_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_data_domain_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Subject Area Messages Implementation
// ============================================================================

std::vector<std::byte> get_subject_areas_request::serialize() const {
    return {};
}

std::expected<get_subject_areas_request, error_code>
get_subject_areas_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_subject_areas_request{};
}

std::ostream& operator<<(std::ostream& s, const get_subject_areas_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_subject_areas_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(subject_areas.size()));
    for (const auto& sa : subject_areas) {
        write_subject_area(buffer, sa);
    }
    return buffer;
}

std::expected<get_subject_areas_response, error_code>
get_subject_areas_response::deserialize(std::span<const std::byte> data) {
    get_subject_areas_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.subject_areas.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto sa_result = read_subject_area(data);
        if (!sa_result) return std::unexpected(sa_result.error());
        response.subject_areas.push_back(std::move(*sa_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_subject_areas_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_subject_areas_by_domain_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, domain_name);
    return buffer;
}

std::expected<get_subject_areas_by_domain_request, error_code>
get_subject_areas_by_domain_request::deserialize(std::span<const std::byte> data) {
    get_subject_areas_by_domain_request request;

    auto domain_name_result = reader::read_string(data);
    if (!domain_name_result) return std::unexpected(domain_name_result.error());
    request.domain_name = *domain_name_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_subject_areas_by_domain_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_subject_areas_by_domain_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(subject_areas.size()));
    for (const auto& sa : subject_areas) {
        write_subject_area(buffer, sa);
    }
    return buffer;
}

std::expected<get_subject_areas_by_domain_response, error_code>
get_subject_areas_by_domain_response::deserialize(std::span<const std::byte> data) {
    get_subject_areas_by_domain_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.subject_areas.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto sa_result = read_subject_area(data);
        if (!sa_result) return std::unexpected(sa_result.error());
        response.subject_areas.push_back(std::move(*sa_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_subject_areas_by_domain_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_subject_area_request::serialize() const {
    std::vector<std::byte> buffer;
    write_subject_area(buffer, subject_area);
    return buffer;
}

std::expected<save_subject_area_request, error_code>
save_subject_area_request::deserialize(std::span<const std::byte> data) {
    save_subject_area_request request;

    auto sa_result = read_subject_area(data);
    if (!sa_result) return std::unexpected(sa_result.error());
    request.subject_area = std::move(*sa_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_subject_area_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_subject_area_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_subject_area_response, error_code>
save_subject_area_response::deserialize(std::span<const std::byte> data) {
    save_subject_area_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_subject_area_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const subject_area_key& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_subject_area_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_subject_area_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(keys.size()));
    for (const auto& k : keys) {
        write_subject_area_key(buffer, k);
    }
    return buffer;
}

std::expected<delete_subject_area_request, error_code>
delete_subject_area_request::deserialize(std::span<const std::byte> data) {
    delete_subject_area_request request;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    request.keys.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto key_result = read_subject_area_key(data);
        if (!key_result) return std::unexpected(key_result.error());
        request.keys.push_back(std::move(*key_result));
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const delete_subject_area_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_subject_area_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        write_subject_area_key(buffer, r.key);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_subject_area_response, error_code>
delete_subject_area_response::deserialize(std::span<const std::byte> data) {
    delete_subject_area_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_subject_area_result r;

        auto key_result = read_subject_area_key(data);
        if (!key_result) return std::unexpected(key_result.error());
        r.key = std::move(*key_result);

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

std::ostream& operator<<(std::ostream& s, const delete_subject_area_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_subject_area_history_request::serialize() const {
    std::vector<std::byte> buffer;
    write_subject_area_key(buffer, key);
    return buffer;
}

std::expected<get_subject_area_history_request, error_code>
get_subject_area_history_request::deserialize(std::span<const std::byte> data) {
    get_subject_area_history_request request;

    auto key_result = read_subject_area_key(data);
    if (!key_result) return std::unexpected(key_result.error());
    request.key = std::move(*key_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_subject_area_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_subject_area_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_subject_area(buffer, v);
    }
    return buffer;
}

std::expected<get_subject_area_history_response, error_code>
get_subject_area_history_response::deserialize(std::span<const std::byte> data) {
    get_subject_area_history_response response;

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
        auto sa_result = read_subject_area(data);
        if (!sa_result) return std::unexpected(sa_result.error());
        response.versions.push_back(std::move(*sa_result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_subject_area_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
