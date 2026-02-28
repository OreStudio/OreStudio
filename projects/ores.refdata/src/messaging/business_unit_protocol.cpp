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
#include "ores.refdata/messaging/business_unit_protocol.hpp"

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
// Business Unit helpers
// ============================================================================

void write_business_unit(std::vector<std::byte>& buffer,
    const domain::business_unit& bu) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(bu.version));
    writer::write_uuid(buffer, bu.id);
    writer::write_uuid(buffer, bu.party_id);
    writer::write_string(buffer, bu.unit_name);
    writer::write_bool(buffer, bu.parent_business_unit_id.has_value());
    if (bu.parent_business_unit_id.has_value()) {
        writer::write_uuid(buffer, *bu.parent_business_unit_id);
    }
    writer::write_bool(buffer, bu.unit_type_id.has_value());
    if (bu.unit_type_id.has_value()) {
        writer::write_uuid(buffer, *bu.unit_type_id);
    }
    writer::write_string(buffer, bu.unit_code);
    writer::write_string(buffer, bu.business_centre_code);
    writer::write_string(buffer, bu.status);
    writer::write_string(buffer, bu.modified_by);
    writer::write_string(buffer, bu.performed_by);
    writer::write_string(buffer, bu.change_reason_code);
    writer::write_string(buffer, bu.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(bu.recorded_at));
}

std::expected<domain::business_unit, error_code>
read_business_unit(std::span<const std::byte>& data) {
    domain::business_unit bu;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    bu.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    bu.id = *id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    bu.party_id = *party_id_result;

    auto unit_name_result = reader::read_string(data);
    if (!unit_name_result) return std::unexpected(unit_name_result.error());
    bu.unit_name = *unit_name_result;

    auto parent_business_unit_id_present_result = reader::read_bool(data);
    if (!parent_business_unit_id_present_result) return std::unexpected(parent_business_unit_id_present_result.error());
    if (*parent_business_unit_id_present_result) {
        auto parent_business_unit_id_result = reader::read_uuid(data);
        if (!parent_business_unit_id_result) return std::unexpected(parent_business_unit_id_result.error());
        bu.parent_business_unit_id = *parent_business_unit_id_result;
    }

    auto unit_type_id_present_result = reader::read_bool(data);
    if (!unit_type_id_present_result) return std::unexpected(unit_type_id_present_result.error());
    if (*unit_type_id_present_result) {
        auto unit_type_id_result = reader::read_uuid(data);
        if (!unit_type_id_result) return std::unexpected(unit_type_id_result.error());
        bu.unit_type_id = *unit_type_id_result;
    }

    auto unit_code_result = reader::read_string(data);
    if (!unit_code_result) return std::unexpected(unit_code_result.error());
    bu.unit_code = *unit_code_result;

    auto business_centre_code_result = reader::read_string(data);
    if (!business_centre_code_result) return std::unexpected(business_centre_code_result.error());
    bu.business_centre_code = *business_centre_code_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    bu.status = *status_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    bu.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    bu.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    bu.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    bu.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        bu.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return bu;
}

} // anonymous namespace

// ============================================================================
// Business Unit Messages Implementation
// ============================================================================

std::vector<std::byte> get_business_units_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_business_units_request, error_code>
get_business_units_request::deserialize(std::span<const std::byte> data) {
    get_business_units_request request;

    // Backward compatibility: empty payload from old clients uses defaults
    if (data.empty()) {
        return request;
    }

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_business_units_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_units_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write business unit count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(business_units.size()));
    for (const auto& bu : business_units) {
        write_business_unit(buffer, bu);
    }
    return buffer;
}

std::expected<get_business_units_response, error_code>
get_business_units_response::deserialize(std::span<const std::byte> data) {
    get_business_units_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_available_count = *total_result;

    // Read business unit count in this response
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.business_units.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_business_unit(data);
        if (!result) return std::unexpected(result.error());
        response.business_units.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_units_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_business_unit_request
save_business_unit_request::from(domain::business_unit business_unit) {
    return save_business_unit_request{std::vector<domain::business_unit>{std::move(business_unit)}};
}

save_business_unit_request
save_business_unit_request::from(std::vector<domain::business_unit> business_units) {
    return save_business_unit_request{std::move(business_units)};
}

std::vector<std::byte> save_business_unit_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(business_units.size()));
    for (const auto& e : business_units)
        write_business_unit(buffer, e);
    return buffer;
}

std::expected<save_business_unit_request, error_code>
save_business_unit_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result)
        return std::unexpected(count_result.error());

    save_business_unit_request request;
    request.business_units.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_business_unit(data);
        if (!e)
            return std::unexpected(e.error());
        request.business_units.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_business_unit_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_business_unit_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_business_unit_response, error_code>
save_business_unit_response::deserialize(std::span<const std::byte> data) {
    save_business_unit_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_business_unit_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_business_unit_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_business_unit_request, error_code>
delete_business_unit_request::deserialize(std::span<const std::byte> data) {
    delete_business_unit_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_business_unit_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_business_unit_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_business_unit_response, error_code>
delete_business_unit_response::deserialize(std::span<const std::byte> data) {
    delete_business_unit_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_business_unit_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_unit_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_business_unit_history_request, error_code>
get_business_unit_history_request::deserialize(std::span<const std::byte> data) {
    get_business_unit_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_business_unit_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_business_unit(buffer, v);
    }
    return buffer;
}

std::expected<get_business_unit_history_response, error_code>
get_business_unit_history_response::deserialize(std::span<const std::byte> data) {
    get_business_unit_history_response response;

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
        auto result = read_business_unit(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_business_unit_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
