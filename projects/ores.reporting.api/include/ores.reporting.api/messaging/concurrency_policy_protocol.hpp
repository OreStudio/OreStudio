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
#ifndef ORES_REPORTING_MESSAGING_CONCURRENCY_POLICY_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_CONCURRENCY_POLICY_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.reporting.api/domain/concurrency_policy.hpp"

namespace ores::reporting::messaging {

struct get_concurrency_policies_request {
    using response_type = struct get_concurrency_policies_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency-policies.list";
    int offset = 0;
    int limit = 100;
};

struct get_concurrency_policies_response {
    std::vector<ores::reporting::domain::concurrency_policy> policies;
    int total_available_count = 0;
};

struct save_concurrency_policy_request {
    using response_type = struct save_concurrency_policy_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency-policies.save";
    ores::reporting::domain::concurrency_policy policy;
};

struct save_concurrency_policy_response {
    bool success = false;
    std::string message;
};

struct delete_concurrency_policy_request {
    using response_type = struct delete_concurrency_policy_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency-policies.delete";
    std::vector<std::string> codes;
};

struct delete_concurrency_policy_response {
    bool success = false;
    std::string message;
};

struct get_concurrency_policy_history_request {
    using response_type = struct get_concurrency_policy_history_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency-policies.history";
    std::string code;
};

struct get_concurrency_policy_history_response {
    bool success = false;
    std::string message;
    std::vector<ores::reporting::domain::concurrency_policy> history;
};

}

#endif
