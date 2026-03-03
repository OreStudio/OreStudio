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
#ifndef ORES_REPORTING_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP
#define ORES_REPORTING_MESSAGING_REPORT_DEFINITION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.reporting/domain/report_definition.hpp"

namespace ores::reporting::messaging {

// ============================================================================
// Report Definition Messages
// ============================================================================

/**
 * @brief Request to retrieve all report definitions.
 */
struct get_report_definitions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_report_definitions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_definitions_request& v);

/**
 * @brief Response containing all report definitions.
 */
struct get_report_definitions_response final {
    std::vector<domain::report_definition> definitions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_definitions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_definitions_response& v);

/**
 * @brief Request to save a report definition (create or update).
 */
struct save_report_definition_request final {
    domain::report_definition definition;

    std::vector<std::byte> serialize() const;
    static std::expected<save_report_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_report_definition_request& v);

/**
 * @brief Response confirming report definition save operation.
 */
struct save_report_definition_response final {
    bool success;
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<save_report_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const save_report_definition_response& v);

/**
 * @brief Result for a single report definition deletion.
 */
struct delete_report_definition_result final {
    boost::uuids::uuid id;  ///< Primary key
    bool success;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const delete_report_definition_result& v);

/**
 * @brief Request to delete one or more report definitions.
 */
struct delete_report_definition_request final {
    std::vector<boost::uuids::uuid> ids;  ///< Primary keys

    std::vector<std::byte> serialize() const;
    static std::expected<delete_report_definition_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_report_definition_request& v);

/**
 * @brief Response confirming report definition deletion(s).
 */
struct delete_report_definition_response final {
    std::vector<delete_report_definition_result> results;

    std::vector<std::byte> serialize() const;
    static std::expected<delete_report_definition_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const delete_report_definition_response& v);

/**
 * @brief Request to retrieve version history for a report definition.
 */
struct get_report_definition_history_request final {
    boost::uuids::uuid id;  ///< Primary key

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_definition_history_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_definition_history_request& v);

/**
 * @brief Response containing report definition version history.
 */
struct get_report_definition_history_response final {
    bool success;
    std::string message;
    std::vector<domain::report_definition> versions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_report_definition_history_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_report_definition_history_response& v);

// ============================================================================
// Report Scheduling Messages
// ============================================================================

/**
 * @brief Request to schedule (activate) one or more report definitions.
 */
struct schedule_report_definitions_request final {
    std::vector<boost::uuids::uuid> ids;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;

    std::vector<std::byte> serialize() const;
    static std::expected<schedule_report_definitions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const schedule_report_definitions_request& v);

/**
 * @brief Response confirming report definition scheduling.
 */
struct schedule_report_definitions_response final {
    bool success = false;
    std::string message;
    int scheduled_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<schedule_report_definitions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const schedule_report_definitions_response& v);

/**
 * @brief Request to unschedule (deactivate) one or more report definitions.
 */
struct unschedule_report_definitions_request final {
    std::vector<boost::uuids::uuid> ids;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;

    std::vector<std::byte> serialize() const;
    static std::expected<unschedule_report_definitions_request,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unschedule_report_definitions_request& v);

/**
 * @brief Response confirming report definition unscheduling.
 */
struct unschedule_report_definitions_response final {
    bool success = false;
    std::string message;
    int unscheduled_count = 0;

    std::vector<std::byte> serialize() const;
    static std::expected<unschedule_report_definitions_response,
                         ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const unschedule_report_definitions_response& v);

}

namespace ores::comms::messaging {

// Report Definition traits
template<>
struct message_traits<reporting::messaging::get_report_definitions_request> {
    using request_type = reporting::messaging::get_report_definitions_request;
    using response_type = reporting::messaging::get_report_definitions_response;
    static constexpr message_type request_message_type =
        message_type::get_report_definitions_request;
};

template<>
struct message_traits<reporting::messaging::save_report_definition_request> {
    using request_type = reporting::messaging::save_report_definition_request;
    using response_type = reporting::messaging::save_report_definition_response;
    static constexpr message_type request_message_type =
        message_type::save_report_definition_request;
};

template<>
struct message_traits<reporting::messaging::delete_report_definition_request> {
    using request_type = reporting::messaging::delete_report_definition_request;
    using response_type = reporting::messaging::delete_report_definition_response;
    static constexpr message_type request_message_type =
        message_type::delete_report_definition_request;
};

template<>
struct message_traits<reporting::messaging::get_report_definition_history_request> {
    using request_type = reporting::messaging::get_report_definition_history_request;
    using response_type = reporting::messaging::get_report_definition_history_response;
    static constexpr message_type request_message_type =
        message_type::get_report_definition_history_request;
};

template<>
struct message_traits<reporting::messaging::schedule_report_definitions_request> {
    using request_type = reporting::messaging::schedule_report_definitions_request;
    using response_type = reporting::messaging::schedule_report_definitions_response;
    static constexpr message_type request_message_type =
        message_type::schedule_report_definitions_request;
};

template<>
struct message_traits<reporting::messaging::unschedule_report_definitions_request> {
    using request_type = reporting::messaging::unschedule_report_definitions_request;
    using response_type = reporting::messaging::unschedule_report_definitions_response;
    static constexpr message_type request_message_type =
        message_type::unschedule_report_definitions_request;
};

}

#endif
