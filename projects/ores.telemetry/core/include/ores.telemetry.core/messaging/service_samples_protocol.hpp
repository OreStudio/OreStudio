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
#ifndef ORES_TELEMETRY_CORE_MESSAGING_SERVICE_SAMPLES_PROTOCOL_HPP
#define ORES_TELEMETRY_CORE_MESSAGING_SERVICE_SAMPLES_PROTOCOL_HPP

#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace ores::telemetry::messaging {

/**
 * @brief The latest heartbeat recorded for one running service instance.
 */
struct service_sample {
    /**
     * @brief When the service last reported.
     */
    std::chrono::system_clock::time_point sampled_at;
    /**
     * @brief Canonical service name, for example @c ores.compute.service.
     */
    std::string service_name;
    /**
     * @brief Per-process identifier, so two instances of one service are told
     * apart.
     */
    std::string instance_id;
    /**
     * @brief Version the instance reports.
     */
    std::string version;
};

/**
 * @brief One service instance reporting that it is alive.
 *
 * The service publishes it on a timer, and the telemetry service timestamps
 * the receipt and stores it. It carries no response, so the publisher does
 * not wait for one.
 */
struct service_heartbeat_message {
    static constexpr std::string_view nats_subject = "telemetry.v1.services.heartbeat";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /**
     * @brief Canonical service name, for example @c ores.compute.service.
     */
    std::string service_name;
    /**
     * @brief Per-process identifier, generated once at startup.
     */
    std::string instance_id;
    /**
     * @brief Version the instance reports.
     */
    std::string version;
};

/**
 * @brief Asks for the latest sample of every running instance.
 *
 * It carries no fields: the reply covers every service the caller may see.
 */
struct get_service_samples_request {
    using response_type = struct get_service_samples_response;
    static constexpr std::string_view nats_subject = "telemetry.v1.services.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The latest sample of every running instance.
 */
struct get_service_samples_response {
    /**
     * @brief Whether the samples were read.
     */
    bool success = false;
    /**
     * @brief Why they were not, when they were not.
     */
    std::string message;
    /**
     * @brief One sample per running instance, in no particular order.
     */
    std::vector<service_sample> samples;
};

}

#endif
