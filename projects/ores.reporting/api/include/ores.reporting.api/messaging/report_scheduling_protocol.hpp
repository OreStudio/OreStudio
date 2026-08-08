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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_SCHEDULING_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_SCHEDULING_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::messaging {

/**
 * @brief Request to schedule report definitions (create scheduler jobs).
 */
struct schedule_report_definitions_request {
    using response_type = struct schedule_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report-definitions.schedule";
    std::vector<std::string> ids;
};

struct schedule_report_definitions_response {
    bool success = false;
    std::string message;
    int scheduled_count = 0;
    std::vector<std::string> failed_ids;
};

/**
 * @brief Request to unschedule report definitions (remove scheduler jobs).
 */
struct unschedule_report_definitions_request {
    using response_type = struct unschedule_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report-definitions.unschedule";
    std::vector<std::string> ids;
};

struct unschedule_report_definitions_response {
    bool success = false;
    std::string message;
    int unscheduled_count = 0;
    std::vector<std::string> failed_ids;
};

/**
 * @brief Fire-and-forget message published by the scheduler when a report job fires.
 *
 * The scheduler's nats_publish_action_handler publishes this to
 * reporting.v1.report-instances.trigger on each job firing.
 * The reporting service handles it by creating a new report_instance.
 */
struct trigger_report_instance_message {
    static constexpr std::string_view nats_subject = "reporting.v1.report-instances.trigger";
    std::string report_definition_id;
    std::string tenant_id;
    std::int64_t job_instance_id = 0;
};

}

#endif
