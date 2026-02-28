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
#include "ores.refdata/messaging/party_protocol.hpp"

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
// Party helpers
// ============================================================================

void write_party(std::vector<std::byte>& buffer,
    const domain::party& p) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(p.version));
    writer::write_uuid(buffer, p.id);
    writer::write_string(buffer, p.full_name);
    writer::write_string(buffer, p.short_code);
    writer::write_string(buffer, p.party_category);
    writer::write_string(buffer, p.party_type);
    writer::write_bool(buffer, p.parent_party_id.has_value());
    if (p.parent_party_id.has_value()) {
        writer::write_uuid(buffer, *p.parent_party_id);
    }
    writer::write_string(buffer, p.business_center_code);
    writer::write_string(buffer, p.status);
    writer::write_string(buffer, p.modified_by);
    writer::write_string(buffer, p.performed_by);
    writer::write_string(buffer, p.change_reason_code);
    writer::write_string(buffer, p.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(p.recorded_at));
    writer::write_bool(buffer, p.transliterated_name.has_value());
    if (p.transliterated_name.has_value()) {
        writer::write_string(buffer, *p.transliterated_name);
    }
}

std::expected<domain::party, error_code>
read_party(std::span<const std::byte>& data) {
    domain::party p;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    p.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    p.id = *id_result;

    auto full_name_result = reader::read_string(data);
    if (!full_name_result) return std::unexpected(full_name_result.error());
    p.full_name = *full_name_result;

    auto short_code_result = reader::read_string(data);
    if (!short_code_result) return std::unexpected(short_code_result.error());
    p.short_code = *short_code_result;

    auto party_category_result = reader::read_string(data);
    if (!party_category_result) return std::unexpected(party_category_result.error());
    p.party_category = *party_category_result;

    auto party_type_result = reader::read_string(data);
    if (!party_type_result) return std::unexpected(party_type_result.error());
    p.party_type = *party_type_result;

    auto has_parent_party_id_result = reader::read_bool(data);
    if (!has_parent_party_id_result) return std::unexpected(has_parent_party_id_result.error());
    if (*has_parent_party_id_result) {
        auto parent_party_id_result = reader::read_uuid(data);
        if (!parent_party_id_result) return std::unexpected(parent_party_id_result.error());
        p.parent_party_id = *parent_party_id_result;
    }

    auto business_center_code_result = reader::read_string(data);
    if (!business_center_code_result) return std::unexpected(business_center_code_result.error());
    p.business_center_code = *business_center_code_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    p.status = *status_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    p.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    p.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    p.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    p.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        p.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto has_transliterated_name_result = reader::read_bool(data);
    if (!has_transliterated_name_result) return std::unexpected(has_transliterated_name_result.error());
    if (*has_transliterated_name_result) {
        auto transliterated_name_result = reader::read_string(data);
        if (!transliterated_name_result) return std::unexpected(transliterated_name_result.error());
        p.transliterated_name = *transliterated_name_result;
    }

    return p;
}

} // anonymous namespace

// ============================================================================
// Party Messages Implementation
// ============================================================================

std::vector<std::byte> get_parties_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_parties_request, error_code>
get_parties_request::deserialize(std::span<const std::byte> data) {
    get_parties_request request;

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

std::ostream& operator<<(std::ostream& s, const get_parties_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_parties_response::serialize() const {
    std::vector<std::byte> buffer;

    // Write total available count
    writer::write_uint32(buffer, total_available_count);

    // Write party count in this response
    writer::write_uint32(buffer, static_cast<std::uint32_t>(parties.size()));
    for (const auto& p : parties) {
        write_party(buffer, p);
    }
    return buffer;
}

std::expected<get_parties_response, error_code>
get_parties_response::deserialize(std::span<const std::byte> data) {
    get_parties_response response;

    // Read total available count
    auto total_result = reader::read_uint32(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_available_count = *total_result;

    // Read party count in this response
    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.parties.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_party(data);
        if (!result) return std::unexpected(result.error());
        response.parties.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_parties_response& v) {
    rfl::json::write(v, s);
    return s;
}

save_party_request
save_party_request::from(domain::party party) {
    return save_party_request{std::vector<domain::party>{std::move(party)}};
}

save_party_request
save_party_request::from(std::vector<domain::party> parties) {
    return save_party_request{std::move(parties)};
}

std::vector<std::byte> save_party_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(parties.size()));
    for (const auto& e : parties)
        write_party(buffer, e);
    return buffer;
}

std::expected<save_party_request, error_code>
save_party_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());

    save_party_request request;
    request.parties.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_party(data);
        if (!e) return std::unexpected(e.error());
        request.parties.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_party_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_party_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_party_response, error_code>
save_party_response::deserialize(std::span<const std::byte> data) {
    save_party_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_party_response& v) {
    rfl::json::write(v, s);
    return s;
}


std::vector<std::byte> delete_party_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_party_request, error_code>
delete_party_request::deserialize(std::span<const std::byte> data) {
    delete_party_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_party_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_party_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_party_response, error_code>
delete_party_response::deserialize(std::span<const std::byte> data) {
    delete_party_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_party_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_party_history_request, error_code>
get_party_history_request::deserialize(std::span<const std::byte> data) {
    get_party_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_party_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_party_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_party(buffer, v);
    }
    return buffer;
}

std::expected<get_party_history_response, error_code>
get_party_history_response::deserialize(std::span<const std::byte> data) {
    get_party_history_response response;

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
        auto result = read_party(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_party_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
