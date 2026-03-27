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
#ifndef ORES_REPORTING_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.reporting.api/domain/report_instance.hpp"

namespace ores::reporting::messaging {

struct get_report_instances_request {
    using response_type = struct get_report_instances_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-instances.list";
    int offset = 0;
    int limit = 100;
};

struct get_report_instances_response {
    std::vector<ores::reporting::domain::report_instance> instances;
    int total_available_count = 0;
};

struct save_report_instance_request {
    using response_type = struct save_report_instance_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-instances.save";
    ores::reporting::domain::report_instance instance;
};

struct save_report_instance_response {
    bool success = false;
    std::string message;
};

struct delete_report_instance_request {
    using response_type = struct delete_report_instance_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-instances.delete";
    std::vector<std::string> ids;
};

struct delete_report_instance_response {
    bool success = false;
    std::string message;
};

struct get_report_instance_history_request {
    using response_type = struct get_report_instance_history_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-instances.history";
    std::string id;
};

struct get_report_instance_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::reporting::domain::report_instance> history;
};

/**
 * @brief Fire-and-forget message published by the scheduler when a report job fires.
 *
 * The scheduler's nats_publish_action_handler publishes this to
 * reporting.v1.report-instances.trigger on each job firing.
 * The reporting service handles it by creating a new report_instance.
 */
struct trigger_report_instance_message {
    static constexpr std::string_view nats_subject =
        "reporting.v1.report-instances.trigger";
    std::string report_definition_id;
    std::string tenant_id;
    std::int64_t job_instance_id = 0;
};

}

#endif
