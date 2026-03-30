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
    /**
     * @brief Original end-user on whose behalf this request is made.
     *
     * Set by the calling service from its validated JWT actor (via
     * delegated_actor()). The scheduler uses this as modified_by on the
     * job definition, attributing the write to the originating user rather
     * than the calling service account.
     *
     * Non-cryptographic: trusted only because the caller is an authenticated
     * internal service. See delegated_actor() for full security note.
     */
    std::string on_behalf_of;
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
    /** @brief Original end-user on whose behalf this request is made. See
     *  schedule_job_request::on_behalf_of for the full note. */
    std::string on_behalf_of;
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
    /** @brief Original end-user on whose behalf this request is made. See
     *  schedule_job_request::on_behalf_of for the full note. */
    std::string on_behalf_of;
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

}

#endif
