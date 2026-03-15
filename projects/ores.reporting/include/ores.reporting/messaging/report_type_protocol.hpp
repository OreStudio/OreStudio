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
#ifndef ORES_REPORTING_MESSAGING_REPORT_TYPE_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_REPORT_TYPE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.reporting/domain/report_type.hpp"

namespace ores::reporting::messaging {

struct get_report_types_request {
    using response_type = struct get_report_types_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-types.list";
    int offset = 0;
    int limit = 100;
};

struct get_report_types_response {
    std::vector<ores::reporting::domain::report_type> types;
    int total_available_count = 0;
};

struct save_report_type_request {
    using response_type = struct save_report_type_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-types.save";
    ores::reporting::domain::report_type type;
};

struct save_report_type_response {
    bool success = false;
    std::string message;
};

struct delete_report_type_request {
    using response_type = struct delete_report_type_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-types.delete";
    std::vector<std::string> codes;
};

struct delete_report_type_response {
    bool success = false;
    std::string message;
};

struct get_report_type_history_request {
    using response_type = struct get_report_type_history_response;
    static constexpr std::string_view nats_subject =
        "ores.reporting.v1.report-types.history";
    std::string code;
};

struct get_report_type_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::reporting::domain::report_type> history;
};

}

#endif
