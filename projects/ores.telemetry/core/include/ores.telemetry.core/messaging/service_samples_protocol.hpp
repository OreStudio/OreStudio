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

#include "ores.telemetry.core/domain/service_state.hpp"
#include <chrono>
#include <optional>
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
     * @brief The host the instance runs on, empty when the publisher does not
     * know it.
     */
    std::string host_id;
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
    static constexpr std::string_view nats_subject = "telemetry.v1.ops.service_heartbeat";
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
     * @brief The host the instance runs on, empty when the publisher does not
     * know it.
     */
    std::string host_id;
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

/**
 * @brief One expected instance of one service, and its last report.
 *
 * A service expects as many slots as the registry gives it replicas. The
 * newest reporting instances fill the slots, newest first; a slot no
 * instance fills is missing, and its report fields are empty.
 */
struct service_roster_slot {
    /**
     * @brief Canonical service name, for example @c ores.iam.service.
     */
    std::string service_name;
    /**
     * @brief The name a person reads, for example @c Analytics @c Service.
     */
    std::string display_name;
    /**
     * @brief What the service does, in one sentence from the service registry.
     */
    std::string description;
    /**
     * @brief The IAM service account the process signs in as, empty for a process
     * that signs in as no account of its own.
     */
    std::optional<std::string> service_account;
    /**
     * @brief Which expected instance this is, from 1 to the service's replicas.
     */
    int slot = 0;
    /**
     * @brief Running, lost or missing, read from the heartbeats.
     */
    ores::telemetry::domain::service_state state = ores::telemetry::domain::service_state::missing;
    /**
     * @brief The instance that fills the slot, empty when the slot is missing.
     */
    std::optional<std::string> instance_id;
    /**
     * @brief The host the instance last reported from, empty when the slot is
     * missing.
     */
    std::optional<std::string> host_id;
    /**
     * @brief The version the instance last reported, empty when the slot is
     * missing.
     */
    std::optional<std::string> version;
    /**
     * @brief When the instance last reported, at any age; empty when the slot is
     * missing.
     */
    std::optional<std::chrono::system_clock::time_point> sampled_at;
};

/**
 * @brief Asks for the services roster: every expected instance and its state.
 *
 * It carries no fields: the roster covers the whole installation.
 */
struct get_service_roster_request {
    using response_type = struct get_service_roster_response;
    static constexpr std::string_view nats_subject = "telemetry.v1.ops.get_service_roster";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief Every expected instance of every enabled service, with its state.
 */
struct get_service_roster_response {
    /**
     * @brief Whether the roster was read.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
    /**
     * @brief One slot per expected instance, ordered by service name, then slot.
     */
    std::vector<service_roster_slot> slots;
};

}

#endif
