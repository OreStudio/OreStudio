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
#include "ores.iam/messaging/tenant_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::iam::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Tenant helpers
// ============================================================================

void write_tenant(std::vector<std::byte>& buffer,
    const domain::tenant& t) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(t.version));
    writer::write_uuid(buffer, t.id);
    writer::write_string(buffer, t.code);
    writer::write_string(buffer, t.name);
    writer::write_string(buffer, t.type);
    writer::write_string(buffer, t.description);
    writer::write_string(buffer, t.hostname);
    writer::write_string(buffer, t.status);
    writer::write_string(buffer, t.recorded_by);
    writer::write_string(buffer, t.change_reason_code);
    writer::write_string(buffer, t.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(t.recorded_at));
}

std::expected<domain::tenant, error_code>
read_tenant(std::span<const std::byte>& data) {
    domain::tenant t;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    t.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    t.id = *id_result;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    t.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    t.name = *name_result;

    auto type_result = reader::read_string(data);
    if (!type_result) return std::unexpected(type_result.error());
    t.type = *type_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    t.description = *description_result;

    auto hostname_result = reader::read_string(data);
    if (!hostname_result) return std::unexpected(hostname_result.error());
    t.hostname = *hostname_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    t.status = *status_result;

    auto recorded_by_result = reader::read_string(data);
    if (!recorded_by_result) return std::unexpected(recorded_by_result.error());
    t.recorded_by = *recorded_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    t.change_reason_code = *change_reason_code_result;

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
// Tenant Messages Implementation
// ============================================================================

std::vector<std::byte> get_tenants_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, include_deleted);
    return buffer;
}

std::expected<get_tenants_request, error_code>
get_tenants_request::deserialize(std::span<const std::byte> data) {
    get_tenants_request request;

    // Handle both old (empty) and new (with flag) formats for backward compatibility
    if (!data.empty()) {
        auto include_deleted_result = reader::read_bool(data);
        if (!include_deleted_result) return std::unexpected(include_deleted_result.error());
        request.include_deleted = *include_deleted_result;
    }

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_tenants_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_tenants_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(tenants.size()));
    for (const auto& t : tenants) {
        write_tenant(buffer, t);
    }
    return buffer;
}

std::expected<get_tenants_response, error_code>
get_tenants_response::deserialize(std::span<const std::byte> data) {
    get_tenants_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.tenants.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_tenant(data);
        if (!result) return std::unexpected(result.error());
        response.tenants.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_tenants_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_tenant_request::serialize() const {
    std::vector<std::byte> buffer;
    write_tenant(buffer, tenant);
    return buffer;
}

std::expected<save_tenant_request, error_code>
save_tenant_request::deserialize(std::span<const std::byte> data) {
    save_tenant_request request;

    auto result = read_tenant(data);
    if (!result) return std::unexpected(result.error());
    request.tenant = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_tenant_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_tenant_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_tenant_response, error_code>
save_tenant_response::deserialize(std::span<const std::byte> data) {
    save_tenant_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_tenant_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_tenant_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_tenant_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_tenant_request, error_code>
delete_tenant_request::deserialize(std::span<const std::byte> data) {
    delete_tenant_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_tenant_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_tenant_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_tenant_response, error_code>
delete_tenant_response::deserialize(std::span<const std::byte> data) {
    delete_tenant_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_tenant_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_tenant_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_tenant_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_tenant_history_request, error_code>
get_tenant_history_request::deserialize(std::span<const std::byte> data) {
    get_tenant_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_tenant_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_tenant_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_tenant(buffer, v);
    }
    return buffer;
}

std::expected<get_tenant_history_response, error_code>
get_tenant_history_response::deserialize(std::span<const std::byte> data) {
    get_tenant_history_response response;

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
        auto result = read_tenant(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_tenant_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// Provision Tenant Messages Implementation
// ============================================================================

std::vector<std::byte> provision_tenant_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_string(buffer, type);
    writer::write_string(buffer, code);
    writer::write_string(buffer, name);
    writer::write_string(buffer, hostname);
    writer::write_string(buffer, description);
    return buffer;
}

std::expected<provision_tenant_request, error_code>
provision_tenant_request::deserialize(std::span<const std::byte> data) {
    provision_tenant_request request;

    auto type_result = reader::read_string(data);
    if (!type_result) return std::unexpected(type_result.error());
    request.type = *type_result;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    request.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    request.name = *name_result;

    auto hostname_result = reader::read_string(data);
    if (!hostname_result) return std::unexpected(hostname_result.error());
    request.hostname = *hostname_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    request.description = *description_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const provision_tenant_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> provision_tenant_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, error_message);
    writer::write_string(buffer, tenant_id);
    return buffer;
}

std::expected<provision_tenant_response, error_code>
provision_tenant_response::deserialize(std::span<const std::byte> data) {
    provision_tenant_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto error_message_result = reader::read_string(data);
    if (!error_message_result) return std::unexpected(error_message_result.error());
    response.error_message = *error_message_result;

    auto tenant_id_result = reader::read_string(data);
    if (!tenant_id_result) return std::unexpected(tenant_id_result.error());
    response.tenant_id = *tenant_id_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const provision_tenant_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
