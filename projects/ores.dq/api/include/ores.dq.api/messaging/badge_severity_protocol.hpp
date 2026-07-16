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
#ifndef ORES_DQ_API_MESSAGING_BADGE_SEVERITY_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_BADGE_SEVERITY_PROTOCOL_HPP

#include "ores.dq.api/domain/badge_severity.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::dq::messaging {

struct get_badge_severities_request {
    using response_type = struct get_badge_severities_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_badge_severities_response {
    std::vector<ores::dq::domain::badge_severity> severities;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_badge_severity_request {
    using response_type = struct save_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.save";
    ores::dq::domain::badge_severity data;

    static save_badge_severity_request from(ores::dq::domain::badge_severity v) {
        return {.data = std::move(v)};
    }
};

struct save_badge_severity_response {
    bool success = false;
    std::string message;
};

struct delete_badge_severity_request {
    using response_type = struct delete_badge_severity_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.delete";
    std::vector<std::string> codes;
};

struct delete_badge_severity_response {
    bool success = false;
    std::string message;
};

struct get_badge_severity_history_request {
    using response_type = struct get_badge_severity_history_response;
    static constexpr std::string_view nats_subject = "dq.v1.badge_severities.history";
    std::string code;
};

struct get_badge_severity_history_response {
    std::vector<ores::dq::domain::badge_severity> history;
    bool success = false;
    std::string message;
};

}

#endif
