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
#include "ores.refdata/messaging/counterparty_protocol.hpp"

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
// Counterparty helpers
// ============================================================================

void write_counterparty(std::vector<std::byte>& buffer,
    const domain::counterparty& cp) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(cp.version));
    writer::write_uuid(buffer, cp.id);
    writer::write_string(buffer, cp.full_name);
    writer::write_string(buffer, cp.short_code);
    writer::write_string(buffer, cp.party_type);
    writer::write_bool(buffer, cp.parent_counterparty_id.has_value());
    if (cp.parent_counterparty_id.has_value()) {
        writer::write_uuid(buffer, *cp.parent_counterparty_id);
    }
    writer::write_string(buffer, cp.business_center_code);
    writer::write_string(buffer, cp.status);
    writer::write_string(buffer, cp.modified_by);
    writer::write_string(buffer, cp.performed_by);
    writer::write_string(buffer, cp.change_reason_code);
    writer::write_string(buffer, cp.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(cp.recorded_at));
    writer::write_bool(buffer, cp.transliterated_name.has_value());
    if (cp.transliterated_name.has_value()) {
        writer::write_string(buffer, *cp.transliterated_name);
    }
}

std::expected<domain::counterparty, error_code>
read_counterparty(std::span<const std::byte>& data) {
    domain::counterparty cp;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    cp.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    cp.id = *id_result;

    auto full_name_result = reader::read_string(data);
    if (!full_name_result) return std::unexpected(full_name_result.error());
    cp.full_name = *full_name_result;

    auto short_code_result = reader::read_string(data);
    if (!short_code_result) return std::unexpected(short_code_result.error());
    cp.short_code = *short_code_result;

    auto party_type_result = reader::read_string(data);
    if (!party_type_result) return std::unexpected(party_type_result.error());
    cp.party_type = *party_type_result;

    auto has_parent_counterparty_id_result = reader::read_bool(data);
    if (!has_parent_counterparty_id_result) return std::unexpected(has_parent_counterparty_id_result.error());
    if (*has_parent_counterparty_id_result) {
        auto parent_counterparty_id_result = reader::read_uuid(data);
        if (!parent_counterparty_id_result) return std::unexpected(parent_counterparty_id_result.error());
        cp.parent_counterparty_id = *parent_counterparty_id_result;
    }

    auto business_center_code_result = reader::read_string(data);
    if (!business_center_code_result) return std::unexpected(business_center_code_result.error());
    cp.business_center_code = *business_center_code_result;

    auto status_result = reader::read_string(data);
    if (!status_result) return std::unexpected(status_result.error());
    cp.status = *status_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    cp.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    cp.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    cp.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    cp.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        cp.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto has_transliterated_name_result = reader::read_bool(data);
    if (!has_transliterated_name_result) return std::unexpected(has_transliterated_name_result.error());
    if (*has_transliterated_name_result) {
        auto transliterated_name_result = reader::read_string(data);
        if (!transliterated_name_result) return std::unexpected(transliterated_name_result.error());
        cp.transliterated_name = *transliterated_name_result;
    }

    return cp;
}

} // anonymous namespace

// ============================================================================
// Counterparty Messages Implementation
// ============================================================================

std::vector<std::byte> get_counterparties_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, offset);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_counterparties_request, error_code>
get_counterparties_request::deserialize(std::span<const std::byte> data) {
    get_counterparties_request request;

    auto offset_result = reader::read_uint32(data);
    if (!offset_result) return std::unexpected(offset_result.error());
    request.offset = *offset_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_counterparties_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparties_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, total_available_count);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(counterparties.size()));
    for (const auto& cp : counterparties) {
        write_counterparty(buffer, cp);
    }
    return buffer;
}

std::expected<get_counterparties_response, error_code>
get_counterparties_response::deserialize(std::span<const std::byte> data) {
    get_counterparties_response response;

    auto total_result = reader::read_uint32(data);
    if (!total_result) return std::unexpected(total_result.error());
    response.total_available_count = *total_result;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.counterparties.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_counterparty(data);
        if (!result) return std::unexpected(result.error());
        response.counterparties.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_counterparties_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_counterparty_request::serialize() const {
    std::vector<std::byte> buffer;
    write_counterparty(buffer, counterparty);
    return buffer;
}

std::expected<save_counterparty_request, error_code>
save_counterparty_request::deserialize(std::span<const std::byte> data) {
    save_counterparty_request request;

    auto result = read_counterparty(data);
    if (!result) return std::unexpected(result.error());
    request.counterparty = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_counterparty_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_counterparty_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_counterparty_response, error_code>
save_counterparty_response::deserialize(std::span<const std::byte> data) {
    save_counterparty_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_counterparty_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_counterparty_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_counterparty_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_counterparty_request, error_code>
delete_counterparty_request::deserialize(std::span<const std::byte> data) {
    delete_counterparty_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_counterparty_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_counterparty_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_counterparty_response, error_code>
delete_counterparty_response::deserialize(std::span<const std::byte> data) {
    delete_counterparty_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_counterparty_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_counterparty_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparty_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_counterparty_history_request, error_code>
get_counterparty_history_request::deserialize(std::span<const std::byte> data) {
    get_counterparty_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_counterparty_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_counterparty(buffer, v);
    }
    return buffer;
}

std::expected<get_counterparty_history_response, error_code>
get_counterparty_history_response::deserialize(std::span<const std::byte> data) {
    get_counterparty_history_response response;

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
        auto result = read_counterparty(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_counterparty_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
