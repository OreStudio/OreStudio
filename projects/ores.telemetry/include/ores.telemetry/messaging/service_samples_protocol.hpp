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
#ifndef ORES_TELEMETRY_MESSAGING_SERVICE_SAMPLES_PROTOCOL_HPP
#define ORES_TELEMETRY_MESSAGING_SERVICE_SAMPLES_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.telemetry/domain/service_sample.hpp"

namespace ores::telemetry::messaging {

/**
 * @brief Fire-and-forget heartbeat published by every domain service.
 *
 * Each service embeds a heartbeat_publisher that fires every 15 seconds
 * and publishes this message to telemetry.v1.services.heartbeat.
 * The telemetry service subscribes, timestamps the receipt, and persists
 * a service_sample row to ores_telemetry_service_samples_tbl.
 *
 * There is no response type — this is a one-way publish, not a request.
 */
struct service_heartbeat_message {
    static constexpr std::string_view nats_subject =
        "telemetry.v1.services.heartbeat";

    /** @brief Canonical service name (e.g. "ores.compute.service"). */
    std::string service_name;

    /**
     * @brief Per-process UUID, generated once at service startup.
     *
     * Allows distinguishing multiple instances of the same service.
     */
    std::string instance_id;

    /** @brief Service version string (e.g. "1.0"). */
    std::string version;
};

/**
 * @brief Request the latest heartbeat per (service_name, instance_id).
 *
 * Returns one service_sample per running instance, ordered arbitrarily.
 * The client determines RAG status from the sampled_at timestamp.
 */
struct get_service_samples_request {
    using response_type = struct get_service_samples_response;
    static constexpr std::string_view nats_subject =
        "telemetry.v1.services.list";
};

struct get_service_samples_response {
    bool success{false};
    std::string message;
    std::vector<domain::service_sample> samples;
};

}

#endif
