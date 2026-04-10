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
#ifndef ORES_SCHEDULER_MESSAGING_SCHEDULER_PROTOCOL_HPP
#define ORES_SCHEDULER_MESSAGING_SCHEDULER_PROTOCOL_HPP

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.api/domain/job_instance.hpp"

namespace ores::scheduler::messaging {

struct get_job_definitions_request {
    using response_type = struct get_job_definitions_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-definitions.list";
    int offset = 0;
    int limit = 100;
};

struct get_job_definitions_response {
    std::vector<ores::scheduler::domain::job_definition> definitions;
    int total_available_count = 0;
};

struct schedule_job_request {
    using response_type = struct schedule_job_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-definitions.schedule";
    ores::scheduler::domain::job_definition definition;
    std::string change_reason_code;
    std::string change_commentary;
};

struct schedule_job_response {
    bool success = false;
    std::string message;
};

struct unschedule_job_request {
    using response_type = struct unschedule_job_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-definitions.unschedule";
    std::string job_definition_id;
    std::string change_reason_code;
    std::string change_commentary;
};

struct unschedule_job_response {
    bool success = false;
    std::string message;
};

struct schedule_jobs_batch_request {
    using response_type = struct schedule_jobs_batch_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-definitions.schedule-batch";
    std::vector<ores::scheduler::domain::job_definition> definitions;
    std::string change_reason_code;
    std::string change_commentary;
};

struct schedule_jobs_batch_response {
    bool success = false;
    std::string message;
    int scheduled_count = 0;
    std::vector<std::string> failed_ids;
};

struct get_job_history_request {
    using response_type = struct get_job_history_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-definitions.history";
    std::string job_definition_id;
    int limit = 0;
};

struct get_job_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::scheduler::domain::job_instance> instances;
};

// ---------------------------------------------------------------------------
// Job instances — global execution history view
// ---------------------------------------------------------------------------

struct get_job_instances_request {
    using response_type = struct get_job_instances_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.job-instances.list";
    int offset = 0;
    int limit = 100;
};

/**
 * @brief A job_instance enriched with the parent job's display name.
 *
 * Returned by get_job_instances so the Qt layer can render the job name
 * without a separate look-up.
 */
struct job_instance_summary {
    std::int64_t id = 0;
    std::string job_definition_id;
    std::string job_name;
    std::string action_type;
    std::string status;          ///< "starting" | "succeeded" | "failed"
    std::string triggered_at;    ///< ISO-8601 UTC
    std::string started_at;      ///< ISO-8601 UTC
    std::optional<std::string> completed_at;
    std::optional<std::int64_t> duration_ms;
    std::string error_message;
};

struct get_job_instances_response {
    std::vector<job_instance_summary> instances;
    int total_available_count = 0;
};

// ---------------------------------------------------------------------------
// Scheduler status — live overview for the Monitor window
// ---------------------------------------------------------------------------

struct get_scheduler_status_request {
    using response_type = struct get_scheduler_status_response;
    static constexpr std::string_view nats_subject =
        "scheduler.v1.status";
};

/**
 * @brief Per-job status snapshot used by the Scheduler Monitor window.
 */
struct job_schedule_status {
    std::string job_definition_id;
    std::string job_name;
    std::string description;
    std::string schedule_expression;
    bool is_active = false;
    std::optional<std::string> last_run_at;     ///< ISO-8601 UTC, if ever run
    std::optional<std::string> last_run_status; ///< "succeeded" | "failed" | "starting"
    std::optional<std::string> next_fire_at;    ///< ISO-8601 UTC, null if inactive
    int running_count = 0;                      ///< currently running instances
};

struct get_scheduler_status_response {
    std::vector<job_schedule_status> jobs;
    int total_running = 0;
    int total_active = 0;
};

}

#endif
