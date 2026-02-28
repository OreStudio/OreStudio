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
#include "ores.scheduler/messaging/job_definition_protocol.hpp"

#include <expected>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.scheduler/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::scheduler::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// Job Definition helpers
// ============================================================================

void write_job_definition(std::vector<std::byte>& buffer,
    const domain::job_definition& jd) {
    writer::write_uint32(buffer, static_cast<std::uint32_t>(jd.version));
    writer::write_uuid(buffer, jd.id);
    writer::write_string(buffer, jd.job_name);
    writer::write_string(buffer, jd.description);
    writer::write_string(buffer, jd.command);
    writer::write_string(buffer, jd.schedule_expression.to_string());
    writer::write_string(buffer, jd.database_name);
    writer::write_bool(buffer, jd.is_active);
    writer::write_string(buffer, jd.modified_by);
    writer::write_string(buffer, jd.performed_by);
    writer::write_string(buffer, jd.change_reason_code);
    writer::write_string(buffer, jd.change_commentary);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(jd.recorded_at));
}

std::expected<domain::job_definition, error_code>
read_job_definition(std::span<const std::byte>& data) {
    domain::job_definition jd;

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());
    jd.version = static_cast<int>(*version_result);

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    jd.id = *id_result;

    auto job_name_result = reader::read_string(data);
    if (!job_name_result) return std::unexpected(job_name_result.error());
    jd.job_name = *job_name_result;

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());
    jd.description = *description_result;

    auto command_result = reader::read_string(data);
    if (!command_result) return std::unexpected(command_result.error());
    jd.command = *command_result;

    auto schedule_expression_result = reader::read_string(data);
    if (!schedule_expression_result) return std::unexpected(schedule_expression_result.error());
    auto cron_expr = domain::cron_expression::from_string(*schedule_expression_result);
    if (!cron_expr) return std::unexpected(error_code::invalid_request);
    jd.schedule_expression = std::move(*cron_expr);

    auto database_name_result = reader::read_string(data);
    if (!database_name_result) return std::unexpected(database_name_result.error());
    jd.database_name = *database_name_result;

    auto is_active_result = reader::read_bool(data);
    if (!is_active_result) return std::unexpected(is_active_result.error());
    jd.is_active = *is_active_result;

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());
    jd.modified_by = *modified_by_result;

    auto performed_by_result = reader::read_string(data);
    if (!performed_by_result) return std::unexpected(performed_by_result.error());
    jd.performed_by = *performed_by_result;

    auto change_reason_code_result = reader::read_string(data);
    if (!change_reason_code_result) return std::unexpected(change_reason_code_result.error());
    jd.change_reason_code = *change_reason_code_result;

    auto change_commentary_result = reader::read_string(data);
    if (!change_commentary_result) return std::unexpected(change_commentary_result.error());
    jd.change_commentary = *change_commentary_result;

    auto recorded_at_result = reader::read_string(data);
    if (!recorded_at_result) return std::unexpected(recorded_at_result.error());
    try {
        jd.recorded_at = ores::platform::time::datetime::parse_time_point(*recorded_at_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    return jd;
}

} // anonymous namespace

// ============================================================================
// Job Definition Messages Implementation
// ============================================================================

save_job_definition_request
save_job_definition_request::from(domain::job_definition definition) {
    return save_job_definition_request{
        std::vector<domain::job_definition>{std::move(definition)}};
}

save_job_definition_request
save_job_definition_request::from(std::vector<domain::job_definition> definitions) {
    return save_job_definition_request{std::move(definitions)};
}

std::vector<std::byte> save_job_definition_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(definitions.size()));
    for (const auto& e : definitions)
        write_job_definition(buffer, e);
    return buffer;
}

std::expected<save_job_definition_request, error_code>
save_job_definition_request::deserialize(std::span<const std::byte> data) {
    auto count_result = reader::read_uint32(data);
    if (!count_result) return std::unexpected(count_result.error());
    save_job_definition_request request;
    request.definitions.reserve(*count_result);
    for (std::uint32_t i = 0; i < *count_result; ++i) {
        auto e = read_job_definition(data);
        if (!e) return std::unexpected(e.error());
        request.definitions.push_back(std::move(*e));
    }
    return request;
}

std::ostream& operator<<(std::ostream& s, const save_job_definition_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> save_job_definition_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<save_job_definition_response, error_code>
save_job_definition_response::deserialize(std::span<const std::byte> data) {
    save_job_definition_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const save_job_definition_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_job_definition_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(ids.size()));
    for (const auto& id : ids) {
        writer::write_uuid(buffer, id);
    }
    return buffer;
}

std::expected<delete_job_definition_request, error_code>
delete_job_definition_request::deserialize(std::span<const std::byte> data) {
    delete_job_definition_request request;

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

std::ostream& operator<<(std::ostream& s, const delete_job_definition_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> delete_job_definition_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<delete_job_definition_response, error_code>
delete_job_definition_response::deserialize(std::span<const std::byte> data) {
    delete_job_definition_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = *message_result;

    return response;
}

std::ostream& operator<<(std::ostream& s, const delete_job_definition_response& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_job_definition_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, id);
    return buffer;
}

std::expected<get_job_definition_history_request, error_code>
get_job_definition_history_request::deserialize(std::span<const std::byte> data) {
    get_job_definition_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.id = *id_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_job_definition_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

std::vector<std::byte> get_job_definition_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(versions.size()));
    for (const auto& v : versions) {
        write_job_definition(buffer, v);
    }
    return buffer;
}

std::expected<get_job_definition_history_response, error_code>
get_job_definition_history_response::deserialize(std::span<const std::byte> data) {
    get_job_definition_history_response response;

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
        auto result = read_job_definition(data);
        if (!result) return std::unexpected(result.error());
        response.versions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_job_definition_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
