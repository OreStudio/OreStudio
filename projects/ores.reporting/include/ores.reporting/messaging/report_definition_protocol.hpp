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
#ifndef ORES_REPORTING_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.reporting/domain/report_definition.hpp"

namespace ores::reporting::messaging {

struct get_report_definitions_request {
    using response_type = struct get_report_definitions_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.list";
    int offset = 0;
    int limit = 100;
};

struct get_report_definitions_response {
    std::vector<ores::reporting::domain::report_definition> definitions;
    int total_available_count = 0;
};

struct save_report_definition_request {
    using response_type = struct save_report_definition_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.save";
    ores::reporting::domain::report_definition definition;
};

struct save_report_definition_response {
    bool success = false;
    std::string message;
};

struct delete_report_definition_request {
    using response_type = struct delete_report_definition_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.delete";
    std::vector<std::string> ids;
};

struct delete_report_definition_response {
    bool success = false;
    std::string message;
};

struct get_report_definition_history_request {
    using response_type = struct get_report_definition_history_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.history";
    std::string id;
};

struct get_report_definition_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::reporting::domain::report_definition> history;
};

struct schedule_report_definitions_request {
    using response_type = struct schedule_report_definitions_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.schedule";
    std::vector<std::string> ids;
    std::string performed_by;
};

struct schedule_report_definitions_response {
    bool success = false;
    std::string message;
    int scheduled_count = 0;
};

struct unschedule_report_definitions_request {
    using response_type = struct unschedule_report_definitions_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-definitions.unschedule";
    std::vector<std::string> ids;
    std::string performed_by;
};

struct unschedule_report_definitions_response {
    bool success = false;
    std::string message;
    int unscheduled_count = 0;
};

}

#endif
