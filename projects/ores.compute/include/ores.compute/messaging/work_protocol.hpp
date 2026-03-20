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
#ifndef ORES_COMPUTE_MESSAGING_WORK_PROTOCOL_HPP
#define ORES_COMPUTE_MESSAGING_WORK_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::compute::messaging {

struct pull_work_request {
    using response_type = struct pull_work_response;
    static constexpr std::string_view nats_subject =
        "compute.v1.work.pull";
    std::string host_id;
};

struct pull_work_response {
    bool success = false;
    std::string result_id;
    std::string workunit_id;
    std::string app_version_id;
    std::string input_uri;
    std::string config_uri;
    std::string message;
};

/**
 * @brief Payload published to the COMPUTE JetStream stream on workunit dispatch.
 *
 * Subject: compute.v1.work.assignments.{tenant_id}
 */
struct work_assignment_event {
    std::string result_id;
    std::string workunit_id;
};

struct heartbeat_message {
    static constexpr std::string_view nats_subject =
        "compute.v1.work.heartbeat";
    std::string host_id;
};

struct reap_work_message {
    static constexpr std::string_view nats_subject =
        "compute.v1.work.reap";
};

}

#endif
