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
#ifndef ORES_REFDATA_MESSAGING_CALENDAR_ADJUSTMENT_PROTOCOL_HPP
#define ORES_REFDATA_MESSAGING_CALENDAR_ADJUSTMENT_PROTOCOL_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Overrides applied to a named ORE calendar.
 *
 * A calendar adjustment patches a built-in calendar with institution-
 * or date-specific exceptions: additional holidays, and additional
 * business days. Dates are ISO-8601 strings ("YYYY-MM-DD") verbatim
 * from the source file, so a round trip preserves them exactly.
 */
struct calendar_adjustment {
    int version = 0;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    std::string calendar_name;
    std::optional<std::string> base_calendar;
    std::vector<std::string> additional_holidays;
    std::vector<std::string> additional_business_days;
    std::string modified_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::string performed_by;
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Requests the transient calendar_adjustment DTOs ORE needs for
 * source='user' calendar templates, assembled server-side from
 * calendar + calendar_exception rows -- never itself persisted. The
 * caller turns the response into a CalendarAdjustments XML file.
 */
struct get_calendar_adjustments_request {
    using response_type = struct get_calendar_adjustments_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_adjustments.export";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<std::string> calendar_codes;
};

struct get_calendar_adjustments_response {
    std::vector<calendar_adjustment> adjustments;
    bool success = false;
    std::string message;
};

}

#endif
