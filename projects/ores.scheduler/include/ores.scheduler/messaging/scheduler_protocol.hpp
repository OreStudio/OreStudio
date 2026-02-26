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
#ifndef ORES_SCHEDULER_MESSAGING_SCHEDULER_PROTOCOL_HPP
#define ORES_SCHEDULER_MESSAGING_SCHEDULER_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.scheduler/domain/job_definition.hpp"
#include "ores.scheduler/domain/job_instance.hpp"

namespace ores::scheduler::messaging {

// ============================================================================
// get_job_definitions
// ============================================================================

/**
 * @brief Request to retrieve all job definitions for the current tenant/party.
 *
 * Wire format: empty payload.
 */
struct get_job_definitions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_job_definitions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_definitions_request& v);

/**
 * @brief Response containing all job definitions.
 *
 * Wire format:
 * - 4 bytes: count (uint32)
 * - N × job_definition
 */
struct get_job_definitions_response final {
    std::vector<domain::job_definition> definitions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_job_definitions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_definitions_response& v);

// ============================================================================
// schedule_job
// ============================================================================

/**
 * @brief Request to schedule a new job with pg_cron.
 *
 * Wire format:
 * - job_definition fields (id excluded; server assigns it)
 * - string: change_reason_code
 * - string: change_commentary
 */
struct schedule_job_request final {
    domain::job_definition definition;
    std::string change_reason_code;
    std::string change_commentary;

    std::vector<std::byte> serialize() const;
    static std::expected<schedule_job_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const schedule_job_request& v);

/**
 * @brief Response after scheduling a job.
 *
 * Wire format:
 * - bool: success
 * - string: message
 * - bool: has_definition (true when success)
 * - job_definition (only if has_definition)
 */
struct schedule_job_response final {
    bool success = false;
    std::string message;
    std::optional<domain::job_definition> definition;

    std::vector<std::byte> serialize() const;
    static std::expected<schedule_job_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const schedule_job_response& v);

// ============================================================================
// unschedule_job
// ============================================================================

/**
 * @brief Request to unschedule a job from pg_cron (definition is retained).
 *
 * Wire format:
 * - 16 bytes: job_definition_id (UUID)
 * - string: change_reason_code
 * - string: change_commentary
 */
struct unschedule_job_request final {
    boost::uuids::uuid job_definition_id;
    std::string change_reason_code;
    std::string change_commentary;

    std::vector<std::byte> serialize() const;
    static std::expected<unschedule_job_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unschedule_job_request& v);

/**
 * @brief Response after unscheduling a job.
 *
 * Wire format:
 * - bool: success
 * - string: message
 */
struct unschedule_job_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<unschedule_job_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unschedule_job_response& v);

// ============================================================================
// get_job_history
// ============================================================================

/**
 * @brief Request to retrieve execution history for a job.
 *
 * Wire format:
 * - 16 bytes: job_definition_id (UUID)
 * - 4 bytes: limit (uint32, 0 = use server default of 100)
 */
struct get_job_history_request final {
    boost::uuids::uuid job_definition_id;
    std::uint32_t limit = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<get_job_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_history_request& v);

/**
 * @brief Response containing job execution history.
 *
 * Wire format:
 * - bool: success
 * - string: message
 * - 4 bytes: count (uint32)
 * - N × job_instance
 */
struct get_job_history_response final {
    bool success = false;
    std::string message;
    std::vector<domain::job_instance> instances;

    std::vector<std::byte> serialize() const;
    static std::expected<get_job_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_history_response& v);

}

namespace ores::comms::messaging {

// Scheduler message_traits specializations
template<>
struct message_traits<scheduler::messaging::get_job_definitions_request> {
    using request_type = scheduler::messaging::get_job_definitions_request;
    using response_type = scheduler::messaging::get_job_definitions_response;
    static constexpr message_type request_message_type =
        message_type::get_job_definitions_request;
};

template<>
struct message_traits<scheduler::messaging::schedule_job_request> {
    using request_type = scheduler::messaging::schedule_job_request;
    using response_type = scheduler::messaging::schedule_job_response;
    static constexpr message_type request_message_type =
        message_type::schedule_job_request;
};

template<>
struct message_traits<scheduler::messaging::unschedule_job_request> {
    using request_type = scheduler::messaging::unschedule_job_request;
    using response_type = scheduler::messaging::unschedule_job_response;
    static constexpr message_type request_message_type =
        message_type::unschedule_job_request;
};

template<>
struct message_traits<scheduler::messaging::get_job_history_request> {
    using request_type = scheduler::messaging::get_job_history_request;
    using response_type = scheduler::messaging::get_job_history_response;
    static constexpr message_type request_message_type =
        message_type::get_job_history_request;
};

}

#endif
