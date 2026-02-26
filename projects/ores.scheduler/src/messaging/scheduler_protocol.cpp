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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.scheduler/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.scheduler/messaging/scheduler_protocol.hpp"

#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/serialization/reader.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::scheduler::messaging {

using ores::utility::serialization::error_code;
using ores::utility::serialization::reader;
using ores::utility::serialization::writer;

namespace {

// ============================================================================
// job_status helpers
// ============================================================================

std::uint8_t job_status_to_uint8(domain::job_status s) {
    switch (s) {
    case domain::job_status::starting:   return 0;
    case domain::job_status::succeeded:  return 1;
    case domain::job_status::failed:     return 2;
    }
    return 0;
}

std::expected<domain::job_status, error_code>
job_status_from_uint8(std::uint8_t v) {
    switch (v) {
    case 0: return domain::job_status::starting;
    case 1: return domain::job_status::succeeded;
    case 2: return domain::job_status::failed;
    default: return std::unexpected(error_code::invalid_request);
    }
}

// ============================================================================
// job_definition wire helpers
// ============================================================================

void write_job_definition(std::vector<std::byte>& buffer,
    const domain::job_definition& def) {
    writer::write_uuid(buffer, def.id);
    writer::write_tenant_id(buffer, def.tenant_id);
    writer::write_uuid(buffer, def.party_id);
    writer::write_bool(buffer, def.cron_job_id.has_value());
    if (def.cron_job_id.has_value()) {
        writer::write_int64(buffer, *def.cron_job_id);
    }
    writer::write_string(buffer, def.job_name);
    writer::write_string(buffer, def.description);
    writer::write_string(buffer, def.command);
    writer::write_string(buffer, def.schedule_expression.to_string());
    writer::write_string(buffer, def.database_name);
    writer::write_bool(buffer, def.is_active);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(def.version));
    writer::write_string(buffer, def.modified_by);
}

std::expected<domain::job_definition, error_code>
read_job_definition(std::span<const std::byte>& data) {
    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());

    auto tenant_id_result = reader::read_tenant_id(data);
    if (!tenant_id_result) return std::unexpected(tenant_id_result.error());

    auto party_id_result = reader::read_uuid(data);
    if (!party_id_result) return std::unexpected(party_id_result.error());

    auto has_cron_job_id_result = reader::read_bool(data);
    if (!has_cron_job_id_result) return std::unexpected(has_cron_job_id_result.error());
    std::optional<std::int64_t> cron_job_id;
    if (*has_cron_job_id_result) {
        auto cron_job_id_result = reader::read_int64(data);
        if (!cron_job_id_result) return std::unexpected(cron_job_id_result.error());
        cron_job_id = *cron_job_id_result;
    }

    auto job_name_result = reader::read_string(data);
    if (!job_name_result) return std::unexpected(job_name_result.error());

    auto description_result = reader::read_string(data);
    if (!description_result) return std::unexpected(description_result.error());

    auto command_result = reader::read_string(data);
    if (!command_result) return std::unexpected(command_result.error());

    auto schedule_expr_result = reader::read_string(data);
    if (!schedule_expr_result) return std::unexpected(schedule_expr_result.error());

    auto expr_result = domain::cron_expression::from_string(*schedule_expr_result);
    if (!expr_result) return std::unexpected(error_code::invalid_request);

    auto database_name_result = reader::read_string(data);
    if (!database_name_result) return std::unexpected(database_name_result.error());

    auto is_active_result = reader::read_bool(data);
    if (!is_active_result) return std::unexpected(is_active_result.error());

    auto version_result = reader::read_uint32(data);
    if (!version_result) return std::unexpected(version_result.error());

    auto modified_by_result = reader::read_string(data);
    if (!modified_by_result) return std::unexpected(modified_by_result.error());

    return domain::job_definition{
        .id = *id_result,
        .tenant_id = *tenant_id_result,
        .party_id = *party_id_result,
        .cron_job_id = cron_job_id,
        .job_name = std::move(*job_name_result),
        .description = std::move(*description_result),
        .command = std::move(*command_result),
        .schedule_expression = std::move(*expr_result),
        .database_name = std::move(*database_name_result),
        .is_active = *is_active_result,
        .version = static_cast<int>(*version_result),
        .modified_by = std::move(*modified_by_result),
    };
}

// ============================================================================
// job_instance wire helpers
// ============================================================================

void write_job_instance(std::vector<std::byte>& buffer,
    const domain::job_instance& inst) {
    writer::write_int64(buffer, inst.instance_id);
    writer::write_int64(buffer, inst.cron_job_id);
    writer::write_uuid(buffer, inst.parent_job_id);
    writer::write_uint8(buffer, job_status_to_uint8(inst.status));
    writer::write_string(buffer, inst.return_message);
    writer::write_string(buffer,
        ores::platform::time::datetime::format_time_point(inst.start_time));
    writer::write_bool(buffer, inst.end_time.has_value());
    if (inst.end_time.has_value()) {
        writer::write_string(buffer,
            ores::platform::time::datetime::format_time_point(*inst.end_time));
    }
}

std::expected<domain::job_instance, error_code>
read_job_instance(std::span<const std::byte>& data) {
    domain::job_instance inst{
        .instance_id = 0,
        .cron_job_id = 0,
        .parent_job_id = {},
    };

    auto instance_id_result = reader::read_int64(data);
    if (!instance_id_result) return std::unexpected(instance_id_result.error());
    inst.instance_id = *instance_id_result;

    auto cron_job_id_result = reader::read_int64(data);
    if (!cron_job_id_result) return std::unexpected(cron_job_id_result.error());
    inst.cron_job_id = *cron_job_id_result;

    auto parent_job_id_result = reader::read_uuid(data);
    if (!parent_job_id_result) return std::unexpected(parent_job_id_result.error());
    inst.parent_job_id = *parent_job_id_result;

    auto status_result = reader::read_uint8(data);
    if (!status_result) return std::unexpected(status_result.error());
    auto job_status = job_status_from_uint8(*status_result);
    if (!job_status) return std::unexpected(job_status.error());
    inst.status = *job_status;

    auto return_message_result = reader::read_string(data);
    if (!return_message_result) return std::unexpected(return_message_result.error());
    inst.return_message = std::move(*return_message_result);

    auto start_time_result = reader::read_string(data);
    if (!start_time_result) return std::unexpected(start_time_result.error());
    try {
        inst.start_time =
            ores::platform::time::datetime::parse_time_point(*start_time_result);
    } catch (const std::invalid_argument&) {
        return std::unexpected(error_code::invalid_request);
    }

    auto has_end_time_result = reader::read_bool(data);
    if (!has_end_time_result) return std::unexpected(has_end_time_result.error());
    if (*has_end_time_result) {
        auto end_time_result = reader::read_string(data);
        if (!end_time_result) return std::unexpected(end_time_result.error());
        try {
            inst.end_time =
                ores::platform::time::datetime::parse_time_point(*end_time_result);
        } catch (const std::invalid_argument&) {
            return std::unexpected(error_code::invalid_request);
        }
    }

    return inst;
}

} // anonymous namespace

// ============================================================================
// get_job_definitions_request
// ============================================================================

std::vector<std::byte> get_job_definitions_request::serialize() const {
    return {};
}

std::expected<get_job_definitions_request, error_code>
get_job_definitions_request::deserialize(std::span<const std::byte> data) {
    if (!data.empty()) {
        return std::unexpected(error_code::payload_too_large);
    }
    return get_job_definitions_request{};
}

std::ostream& operator<<(std::ostream& s, const get_job_definitions_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_job_definitions_response
// ============================================================================

std::vector<std::byte> get_job_definitions_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uint32(buffer, static_cast<std::uint32_t>(definitions.size()));
    for (const auto& def : definitions) {
        write_job_definition(buffer, def);
    }
    return buffer;
}

std::expected<get_job_definitions_response, error_code>
get_job_definitions_response::deserialize(std::span<const std::byte> data) {
    get_job_definitions_response response;

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.definitions.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_job_definition(data);
        if (!result) return std::unexpected(result.error());
        response.definitions.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_job_definitions_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// schedule_job_request
// ============================================================================

std::vector<std::byte> schedule_job_request::serialize() const {
    std::vector<std::byte> buffer;
    write_job_definition(buffer, definition);
    writer::write_string(buffer, change_reason_code);
    writer::write_string(buffer, change_commentary);
    return buffer;
}

std::expected<schedule_job_request, error_code>
schedule_job_request::deserialize(std::span<const std::byte> data) {
    auto def_result = read_job_definition(data);
    if (!def_result) return std::unexpected(def_result.error());

    auto reason_result = reader::read_string(data);
    if (!reason_result) return std::unexpected(reason_result.error());

    auto commentary_result = reader::read_string(data);
    if (!commentary_result) return std::unexpected(commentary_result.error());

    return schedule_job_request{
        .definition = std::move(*def_result),
        .change_reason_code = std::move(*reason_result),
        .change_commentary = std::move(*commentary_result),
    };
}

std::ostream& operator<<(std::ostream& s, const schedule_job_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// schedule_job_response
// ============================================================================

std::vector<std::byte> schedule_job_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_bool(buffer, definition.has_value());
    if (definition.has_value()) {
        write_job_definition(buffer, *definition);
    }
    return buffer;
}

std::expected<schedule_job_response, error_code>
schedule_job_response::deserialize(std::span<const std::byte> data) {
    schedule_job_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = std::move(*message_result);

    auto has_def_result = reader::read_bool(data);
    if (!has_def_result) return std::unexpected(has_def_result.error());
    if (*has_def_result) {
        auto def_result = read_job_definition(data);
        if (!def_result) return std::unexpected(def_result.error());
        response.definition = std::move(*def_result);
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const schedule_job_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// unschedule_job_request
// ============================================================================

std::vector<std::byte> unschedule_job_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, job_definition_id);
    writer::write_string(buffer, change_reason_code);
    writer::write_string(buffer, change_commentary);
    return buffer;
}

std::expected<unschedule_job_request, error_code>
unschedule_job_request::deserialize(std::span<const std::byte> data) {
    unschedule_job_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.job_definition_id = *id_result;

    auto reason_result = reader::read_string(data);
    if (!reason_result) return std::unexpected(reason_result.error());
    request.change_reason_code = std::move(*reason_result);

    auto commentary_result = reader::read_string(data);
    if (!commentary_result) return std::unexpected(commentary_result.error());
    request.change_commentary = std::move(*commentary_result);

    return request;
}

std::ostream& operator<<(std::ostream& s, const unschedule_job_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// unschedule_job_response
// ============================================================================

std::vector<std::byte> unschedule_job_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    return buffer;
}

std::expected<unschedule_job_response, error_code>
unschedule_job_response::deserialize(std::span<const std::byte> data) {
    unschedule_job_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = std::move(*message_result);

    return response;
}

std::ostream& operator<<(std::ostream& s, const unschedule_job_response& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_job_history_request
// ============================================================================

std::vector<std::byte> get_job_history_request::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_uuid(buffer, job_definition_id);
    writer::write_uint32(buffer, limit);
    return buffer;
}

std::expected<get_job_history_request, error_code>
get_job_history_request::deserialize(std::span<const std::byte> data) {
    get_job_history_request request;

    auto id_result = reader::read_uuid(data);
    if (!id_result) return std::unexpected(id_result.error());
    request.job_definition_id = *id_result;

    auto limit_result = reader::read_uint32(data);
    if (!limit_result) return std::unexpected(limit_result.error());
    request.limit = *limit_result;

    return request;
}

std::ostream& operator<<(std::ostream& s, const get_job_history_request& v) {
    rfl::json::write(v, s);
    return s;
}

// ============================================================================
// get_job_history_response
// ============================================================================

std::vector<std::byte> get_job_history_response::serialize() const {
    std::vector<std::byte> buffer;
    writer::write_bool(buffer, success);
    writer::write_string(buffer, message);
    writer::write_uint32(buffer, static_cast<std::uint32_t>(instances.size()));
    for (const auto& inst : instances) {
        write_job_instance(buffer, inst);
    }
    return buffer;
}

std::expected<get_job_history_response, error_code>
get_job_history_response::deserialize(std::span<const std::byte> data) {
    get_job_history_response response;

    auto success_result = reader::read_bool(data);
    if (!success_result) return std::unexpected(success_result.error());
    response.success = *success_result;

    auto message_result = reader::read_string(data);
    if (!message_result) return std::unexpected(message_result.error());
    response.message = std::move(*message_result);

    auto count_result = reader::read_count(data);
    if (!count_result) return std::unexpected(count_result.error());
    auto count = *count_result;

    response.instances.reserve(count);
    for (std::uint32_t i = 0; i < count; ++i) {
        auto result = read_job_instance(data);
        if (!result) return std::unexpected(result.error());
        response.instances.push_back(std::move(*result));
    }

    return response;
}

std::ostream& operator<<(std::ostream& s, const get_job_history_response& v) {
    rfl::json::write(v, s);
    return s;
}

}
