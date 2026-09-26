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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_OPERATIONS_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_OPERATIONS_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct trigger_report_instance_request {
    using response_type = struct trigger_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.trigger_report_instance";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid report_definition_id;
    boost::uuids::uuid tenant_id;
    std::int64_t job_instance_id = 0;
};

struct trigger_report_instance_response {
    ores::utility::domain::result result;
};

struct schedule_report_definitions_request {
    using response_type = struct schedule_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report-definitions.schedule";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> ids;
};

struct schedule_report_definitions_response {
    bool success = false;
    std::string message;
    int scheduled_count = 0;
    std::vector<std::string> failed_ids;
};

struct unschedule_report_definitions_request {
    using response_type = struct unschedule_report_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report-definitions.unschedule";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> ids;
};

struct unschedule_report_definitions_response {
    bool success = false;
    std::string message;
    int unscheduled_count = 0;
    std::vector<std::string> failed_ids;
};

}

#endif
