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
#include "ores.refdata/messaging/business_unit_type_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::refdata::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Business Unit Type helpers
// ============================================================================

void write_business_unit_type(std::vector<std::byte>& buffer,
    const domain::business_unit_type& but) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(but.version));
    writer::write_uuid(buffer, but.id);
    writer::write_string(buffer, but.tenant_id);
    writer::write_string(buffer, but.coding_scheme_code);
    writer::write_string(buffer, but.code);
    writer::write_string(buffer, but.name);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(but.level));
    writer::write_string(buffer, but.description);
    writer::write_string(buffer, but.modified_by);
    writer::write_string(buffer, but.performed_by);
    writer::write_string(buffer, but.change_reason_code);
    writer::write_string(buffer, but.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(but.recorded_at));
}

std::expected<domain::business_unit_type, error_code>
read_business_unit_type(std::span<const std::byte>& data) {
    domain::business_unit_type but;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    but.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    but.id = *id_result;

    auto tenant_id_result = reader::read_string(data);
    if (!tenant_id_result) return std::unexpected(tenant_id_result.error());
    but.tenant_id = *tenant_id_result;

    auto coding_scheme_code_result = reader::read_string(data);
    if (!coding_scheme_code_result) return std::unexpected(coding_scheme_code_result.error());
    but.coding_scheme_code = *coding_scheme_code_result;

    auto code_result = reader::read_string(data);
    if (!code_result) return std::unexpected(code_result.error());
    but.code = *code_result;

    auto name_result = reader::read_string(data);
    if (!name_result) return std::unexpected(name_result.error());
    but.name = *name_result;

    auto level_result = reader::read_uint32(data);
    if (!level_result) return std::unexpected(level_result.error());
    but.level = static_cast<int>(*level_result);

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    but.description = *description_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    but.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    but.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    but.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    but.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        but.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return but;
}

} // anonymous namespace

// ============================================================================
// Business Unit Type Messages Implementation
// ============================================================================

std::vector<std::byte> get_business_unit_types_request::serialize() const {
    return {};
}

std::expected<get_business_unit_types_request, error_code>
get_business_unit_types_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_business_unit_types_request{};
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_types_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_unit_types_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(types.size()));
    for (const auto& but : types) {
        write_business_unit_type(buffer, but);
    }
    return buffer;
}

std::expected<get_business_unit_types_response, error_code>
get_business_unit_types_response::deserialize(std::span<const std::byte> data) {
    get_business_unit_types_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.types.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_business_unit_type(data);
        if (!result) return std::unexpected(result.error());
        response.types.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_types_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_business_unit_type_request
save_business_unit_type_request::from(domain::business_unit_type type) {
    return save_business_unit_type_request{std::vector<domain::business_unit_type>{std::move(type)}};
}

save_business_unit_type_request
save_business_unit_type_request::from(std::vector<domain::business_unit_type> types) {
    return save_business_unit_type_request{std::move(types)};
}

std::vector<std::byte> save_business_unit_type_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(types.size()));
    for (const auto& e : types)
        write_business_unit_type(buffer, e);
    return buffer;
}

std::expected<save_business_unit_type_request, error_code>
save_business_unit_type_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result)
        return std::unexpected(count_result.error());

    save_business_unit_type_request request;
    request.types.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_business_unit_type(data);
        if (!e)
            return std::unexpected(e.error());
        request.types.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_business_unit_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_business_unit_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_business_unit_type_response, error_code>
save_business_unit_type_response::deserialize(std::span<const std::byte> data) {
    save_business_unit_type_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_business_unit_type_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_business_unit_type_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_business_unit_type_request, error_code>
delete_business_unit_type_request::deserialize(std::span<const std::byte> data) {
    delete_business_unit_type_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_business_unit_type_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_business_unit_type_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_business_unit_type_response, error_code>
delete_business_unit_type_response::deserialize(std::span<const std::byte> data) {
    delete_business_unit_type_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_business_unit_type_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_unit_type_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_business_unit_type_history_request, error_code>
get_business_unit_type_history_request::deserialize(std::span<const std::byte> data) {
    get_business_unit_type_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_type_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_unit_type_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_business_unit_type(buffer, v);
    }
    return buffer;
}

std::expected<get_business_unit_type_history_response, error_code>
get_business_unit_type_history_response::deserialize(std::span<const std::byte> data) {
    get_business_unit_type_history_response response;

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
        auto result = read_business_unit_type(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_type_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
