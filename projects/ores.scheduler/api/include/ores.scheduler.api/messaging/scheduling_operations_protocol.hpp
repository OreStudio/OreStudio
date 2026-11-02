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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SCHEDULER_API_MESSAGING_SCHEDULING_OPERATIONS_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_SCHEDULING_OPERATIONS_PROTOCOL_HPP

#include "ores.scheduler.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::scheduler::messaging {

/**
 * @brief Asks for a page of job executions, newest first.
 *
 * Every job the caller's tenant owns is included, so the view answers
 * "what has the scheduler been running" without naming a job first.
 */
struct get_job_instances_request {
    using response_type = struct get_job_instances_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_instances.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

/**
 * @brief One execution, with its job's display fields folded in.
 *
 * The definition identifier is the link to the job; the name and action type
 * come from the definition row so the caller does not look it up.
 */
struct job_instance_summary {
    /**
     * @brief The execution's storage key.
     */
    std::int64_t id = 0;
    /**
     * @brief The job that was executed.
     */
    std::string job_definition_id;
    /**
     * @brief The job's name, read from the definition.
     */
    std::string job_name;
    /**
     * @brief How the job was executed: =execute_sql= or =nats_publish=.
     */
    std::string action_type;
    /**
     * @brief The lifecycle state: =starting=, =succeeded= or =failed=.
     */
    std::string status;
    /**
     * @brief When the cron expression fired, in ISO-8601 UTC.
     */
    std::string triggered_at;
    /**
     * @brief When execution began, in ISO-8601 UTC.
     */
    std::string started_at;
    /**
     * @brief When execution ended, if it has ended.
     */
    std::optional<std::string> completed_at;
    /**
     * @brief How long execution took in milliseconds, if it has ended.
     */
    std::optional<std::int64_t> duration_ms;
    /**
     * @brief Why execution failed, when it failed.
     */
    std::string error_message;
};

/**
 * @brief The page of executions.
 */
struct get_job_instances_response {
    bool success = true;
    std::string message;
    std::vector<job_instance_summary> instances;
    int total_available_count = 0;
};

/**
 * @brief Asks for the live status of every job the caller can see.
 *
 * It carries no fields, because the caller's session decides the scope and
 * the scheduler holds the rest.
 */
struct get_scheduler_status_request {
    using response_type = struct get_scheduler_status_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.ops.get_scheduler_status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief One job's schedule and its last run, as the monitor renders it.
 */
struct job_schedule_status {
    std::string job_definition_id;
    std::string job_name;
    std::string description;
    /**
     * @brief The cron expression, as the job stores it.
     */
    std::string schedule_expression;
    bool is_active = false;
    /**
     * @brief When the job last started, in ISO-8601 UTC, if it ever has.
     */
    std::optional<std::string> last_run_at;
    /**
     * @brief The last run's state: =starting=, =succeeded= or =failed=.
     */
    std::optional<std::string> last_run_status;
    /**
     * @brief When the expression next fires, in ISO-8601 UTC.
     *
     * Absent for a paused job, and absent for an expression the evaluator
     * cannot advance.
     */
    std::optional<std::string> next_fire_at;
    /**
     * @brief Executions of this job that are still starting.
     */
    int running_count = 0;
};

/**
 * @brief The status of every job, with the totals the monitor shows.
 */
struct get_scheduler_status_response {
    bool success = true;
    std::string message;
    std::vector<job_schedule_status> jobs;
    int total_running = 0;
    int total_active = 0;
};

/**
 * @brief The members of get_job_instances_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_job_instances_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"offset", &get_job_instances_request::offset},
        ::ores::utility::codec::field{"limit", &get_job_instances_request::limit},
    };
}

/**
 * @brief The members of job_instance_summary, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const job_instance_summary&) {
    return std::tuple{
        ::ores::utility::codec::field{"id", &job_instance_summary::id},
        ::ores::utility::codec::field{"job_definition_id",
                                      &job_instance_summary::job_definition_id},
        ::ores::utility::codec::field{"job_name", &job_instance_summary::job_name},
        ::ores::utility::codec::field{"action_type", &job_instance_summary::action_type},
        ::ores::utility::codec::field{"status", &job_instance_summary::status},
        ::ores::utility::codec::field{"triggered_at", &job_instance_summary::triggered_at},
        ::ores::utility::codec::field{"started_at", &job_instance_summary::started_at},
        ::ores::utility::codec::field{"completed_at", &job_instance_summary::completed_at},
        ::ores::utility::codec::field{"duration_ms", &job_instance_summary::duration_ms},
        ::ores::utility::codec::field{"error_message", &job_instance_summary::error_message},
    };
}

/**
 * @brief The members of get_job_instances_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_job_instances_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_job_instances_response::success},
        ::ores::utility::codec::field{"message", &get_job_instances_response::message},
        ::ores::utility::codec::field{"instances", &get_job_instances_response::instances},
        ::ores::utility::codec::field{"total_available_count",
                                      &get_job_instances_response::total_available_count},
    };
}

/**
 * @brief The members of get_scheduler_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_scheduler_status_request&) {
    return std::tuple{};
}

/**
 * @brief The members of job_schedule_status, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const job_schedule_status&) {
    return std::tuple{
        ::ores::utility::codec::field{"job_definition_id", &job_schedule_status::job_definition_id},
        ::ores::utility::codec::field{"job_name", &job_schedule_status::job_name},
        ::ores::utility::codec::field{"description", &job_schedule_status::description},
        ::ores::utility::codec::field{"schedule_expression",
                                      &job_schedule_status::schedule_expression},
        ::ores::utility::codec::field{"is_active", &job_schedule_status::is_active},
        ::ores::utility::codec::field{"last_run_at", &job_schedule_status::last_run_at},
        ::ores::utility::codec::field{"last_run_status", &job_schedule_status::last_run_status},
        ::ores::utility::codec::field{"next_fire_at", &job_schedule_status::next_fire_at},
        ::ores::utility::codec::field{"running_count", &job_schedule_status::running_count},
    };
}

/**
 * @brief The members of get_scheduler_status_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_scheduler_status_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_scheduler_status_response::success},
        ::ores::utility::codec::field{"message", &get_scheduler_status_response::message},
        ::ores::utility::codec::field{"jobs", &get_scheduler_status_response::jobs},
        ::ores::utility::codec::field{"total_running",
                                      &get_scheduler_status_response::total_running},
        ::ores::utility::codec::field{"total_active", &get_scheduler_status_response::total_active},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_job_instances_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_job_instances_request& v);
    static ores::scheduler::messaging::get_job_instances_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_job_instances_request& v);
    static ores::scheduler::messaging::get_job_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::job_instance_summary> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::job_instance_summary& v);
    static ores::scheduler::messaging::job_instance_summary read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::job_instance_summary& v);
    static ores::scheduler::messaging::job_instance_summary
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_job_instances_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::get_job_instances_response& v);
    static ores::scheduler::messaging::get_job_instances_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_job_instances_response& v);
    static ores::scheduler::messaging::get_job_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_scheduler_status_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_scheduler_status_request& v);
    static ores::scheduler::messaging::get_scheduler_status_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_scheduler_status_request& v);
    static ores::scheduler::messaging::get_scheduler_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT wire_traits<ores::scheduler::messaging::job_schedule_status> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::scheduler::messaging::job_schedule_status& v);
    static ores::scheduler::messaging::job_schedule_status read_json(std::string_view text);
    static std::string write_msgpack(const ores::scheduler::messaging::job_schedule_status& v);
    static ores::scheduler::messaging::job_schedule_status
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SCHEDULER_API_EXPORT
    wire_traits<ores::scheduler::messaging::get_scheduler_status_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::scheduler::messaging::get_scheduler_status_response& v);
    static ores::scheduler::messaging::get_scheduler_status_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::scheduler::messaging::get_scheduler_status_response& v);
    static ores::scheduler::messaging::get_scheduler_status_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
