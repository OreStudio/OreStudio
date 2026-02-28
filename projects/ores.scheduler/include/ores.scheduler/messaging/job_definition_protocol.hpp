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
#ifndef ORES_SCHEDULER_MESSAGING_JOB_DEFINITION_PROTOCOL_HPP
#define ORES_SCHEDULER_MESSAGING_JOB_DEFINITION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.scheduler/domain/job_definition.hpp"

namespace ores::scheduler::messaging {

// ============================================================================
// Job Definition Messages
// ============================================================================

/**
 * @brief Request to save one or more job definitions (create or update).
 */
struct save_job_definition_request final {
    std::vector<domain::job_definition> definitions;

    static save_job_definition_request from(domain::job_definition definition);
    static save_job_definition_request from(
        std::vector<domain::job_definition> definitions);

    std::vector<std::byte> serialize() const;
    static std::expected<save_job_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_job_definition_request& v);

/**
 * @brief Response confirming job definition save operation(s).
 */
struct save_job_definition_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_job_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_job_definition_response& v);

/**
 * @brief Request to delete one or more job definitions.
 */
struct delete_job_definition_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_job_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_job_definition_request& v);

/**
 * @brief Response confirming job definition deletion(s).
 */
struct delete_job_definition_response final {
    bool success = false;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_job_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_job_definition_response& v);

/**
 * @brief Request to retrieve version history for a job definition.
 */
struct get_job_definition_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_job_definition_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_definition_history_request& v);

/**
 * @brief Response containing job definition version history.
 */
struct get_job_definition_history_response final {
    bool success;
    std::string message;
    std::vector<domain::job_definition> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_job_definition_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_job_definition_history_response& v);

}

namespace ores::comms::messaging {

// Job Definition traits
template<>
struct message_traits<scheduler::messaging::save_job_definition_request> {
    using request_type = scheduler::messaging::save_job_definition_request;
    using response_type = scheduler::messaging::save_job_definition_response;
    static constexpr message_type request_message_type =
        message_type::save_job_definition_request;
};

template<>
struct message_traits<scheduler::messaging::delete_job_definition_request> {
    using request_type = scheduler::messaging::delete_job_definition_request;
    using response_type = scheduler::messaging::delete_job_definition_response;
    static constexpr message_type request_message_type =
        message_type::delete_job_definition_request;
};

template<>
struct message_traits<scheduler::messaging::get_job_definition_history_request> {
    using request_type = scheduler::messaging::get_job_definition_history_request;
    using response_type = scheduler::messaging::get_job_definition_history_response;
    static constexpr message_type request_message_type =
        message_type::get_job_definition_history_request;
};

}

#endif
