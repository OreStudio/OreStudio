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
#include "ores.reporting/messaging/report_instance_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::reporting::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Report Instance helpers
// ============================================================================

void write_report_instance(std::vector<std::byte>& buffer,
    const domain::report_instance& ri) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ri.version));
    writer::write_uuid(buffer, ri.id);
    writer::write_uuid(buffer, ri.party_id);
    writer::write_uuid(buffer, ri.definition_id);
    writer::write_bool(buffer, ri.fsm_state_id.has_value());
    if (ri.fsm_state_id.has_value()) {
        writer::write_uuid(buffer, *ri.fsm_state_id);
    }
    writer::write_int64(buffer, ri.trigger_run_id);
    writer::write_string(buffer, ri.output_message);
    writer::write_bool(buffer, ri.started_at.has_value());
    if (ri.started_at.has_value()) {
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(*ri.started_at));
    }
    writer::write_bool(buffer, ri.completed_at.has_value());
    if (ri.completed_at.has_value()) {
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(*ri.completed_at));
    }
    writer::write_string(buffer, ri.modified_by);
    writer::write_string(buffer, ri.performed_by);
    writer::write_string(buffer, ri.change_reason_code);
    writer::write_string(buffer, ri.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(ri.recorded_at));
}

std::expected<domain::report_instance, error_code>
read_report_instance(std::span<const std::byte>& data) {
    domain::report_instance ri;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    ri.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    ri.id = *id_result;

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());
    ri.party_id = *party_id_result;

    auto definition_id_result = reader::read_uuid(data);
    if (!definition_id_result) return std::unexpected(definition_id_result.error());
    ri.definition_id = *definition_id_result;

    auto fsm_state_id_present_result = reader::read_bool(data);
    if (!fsm_state_id_present_result) return std::unexpected(fsm_state_id_present_result.error());
    if (*fsm_state_id_present_result) {
        auto fsm_state_id_result = reader::read_uuid(data);
        if (!fsm_state_id_result) return std::unexpected(fsm_state_id_result.error());
        ri.fsm_state_id = *fsm_state_id_result;
    }

    auto trigger_run_id_result = reader::read_int64(data);
    if (!trigger_run_id_result) return std::unexpected(trigger_run_id_result.error());
    ri.trigger_run_id = *trigger_run_id_result;

    auto output_message_result = reader::read_string(data);
    if (!output_message_result) return std::unexpected(output_message_result.error());
    ri.output_message = *output_message_result;

    auto started_at_present_result = reader::read_bool(data);
    if (!started_at_present_result) return std::unexpected(started_at_present_result.error());
    if (*started_at_present_result) {
        auto started_at_result = reader::read_string(data);
        if (!started_at_result) return std::unexpected(started_at_result.error());
        try {
            ri.started_at = ores::platform::time::datetime::parse_time_point(*started_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto completed_at_present_result = reader::read_bool(data);
    if (!completed_at_present_result) return std::unexpected(completed_at_present_result.error());
    if (*completed_at_present_result) {
        auto completed_at_result = reader::read_string(data);
        if (!completed_at_result) return std::unexpected(completed_at_result.error());
        try {
            ri.completed_at = ores::platform::time::datetime::parse_time_point(*completed_at_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    ri.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    ri.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    ri.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    ri.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        ri.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return ri;
}

} // anonymous namespace

// ============================================================================
// Report Instance Messages Implementation
// ============================================================================

std::vector<std::byte> get_report_instances_request::serialize() const {
    return {};
}

std::expected<get_report_instances_request, error_code>
get_report_instances_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_report_instances_request{};
}

std::ostream& operator<<(std::ostream& s, const get_report_instances_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_report_instances_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(instances.size()));
    for (const auto& ri : instances) {
        write_report_instance(buffer, ri);
    }
    return buffer;
}

std::expected<get_report_instances_response, error_code>
get_report_instances_response::deserialize(std::span<const std::byte> data) {
    get_report_instances_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.instances.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_report_instance(data);
        if (!result) return std::unexpected(result.error());
        response.instances.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_report_instances_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_report_instance_request::serialize() const {
    std::vector<std::byte> buffer;
    write_report_instance(buffer, instance);
    return buffer;
}

std::expected<save_report_instance_request, error_code>
save_report_instance_request::deserialize(std::span<const std::byte> data) {
    save_report_instance_request request;

    auto result = read_report_instance(data);
    if (!result) return std::unexpected(result.error());
    request.instance = std::move(*result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const save_report_instance_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_report_instance_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_report_instance_response, error_code>
save_report_instance_response::deserialize(std::span<const std::byte> data) {
    save_report_instance_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_report_instance_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::ostream& operator<<(std::ostream& s, const delete_report_instance_result& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_report_instance_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_report_instance_request, error_code>
delete_report_instance_request::deserialize(std::span<const std::byte> data) {
    delete_report_instance_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_report_instance_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_report_instance_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(results.size()));
    for (const auto& r : results) {
        writer::write_uuid(buffer, r.id);
        writer::write_bool(buffer, r.success);
        writer::write_string(buffer, r.message);
    }
    return buffer;
}

std::expected<delete_report_instance_response, error_code>
delete_report_instance_response::deserialize(std::span<const std::byte> data) {
    delete_report_instance_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.results.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        delete_report_instance_result r;

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

std::ostream& operator<<(std::ostream& s, const delete_report_instance_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_report_instance_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_report_instance_history_request, error_code>
get_report_instance_history_request::deserialize(std::span<const std::byte> data) {
    get_report_instance_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_report_instance_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_report_instance_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_report_instance(buffer, v);
    }
    return buffer;
}

std::expected<get_report_instance_history_response, error_code>
get_report_instance_history_response::deserialize(std::span<const std::byte> data) {
    get_report_instance_history_response response;

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
        auto result = read_report_instance(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_report_instance_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
